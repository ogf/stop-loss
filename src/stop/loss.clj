(ns stop.loss
  (:require [lamina.core :as l]))

(defn price-changed-message [time-ms new-price]
  {:pre [((complement neg?) time-ms) ((complement neg?) new-price)]}
  {:time-ms time-ms :price new-price})

(defn time-changed-message [time-ms]
  {:pre [((complement neg?) time-ms)]}
  {:time-ms time-ms})

(def ^:dynamic max-time-in-loss-zone-ms 30000)

(def ^:dynamic max-time-in-trailing-zone-ms 15000)

(defn elapsed-time
  ([op time-ms messages]
     (elapsed-time op time-ms (first messages) (last messages)))
  ([op time-ms from to]
     (and (:time-ms from) (:time-ms to)
          (op (- (:time-ms to) (:time-ms from)) time-ms))))

(defn check-if-new-prices [history]
  (let [last-history (last history)
        max max-time-in-trailing-zone-ms
        [expired not-expired] (split-with #(elapsed-time >= max % last-history)
                                          history)]
    (if-let [last-expired (last expired)]
      {:new-price (-> (apply min-key :price last-expired not-expired)
                      :price)
       :history (vec not-expired)}
      {:history history})))

(defn inherit-last-price-if-needed [last-message message]
  (cond
   (:price message) message
   (not last-message) nil
   :else (merge last-message message)))

(defn trailing-zone [current-price-point-atom]
  (fn [{:keys [history]} message]
    (if-let [message (inherit-last-price-if-needed (last history) message)]
      (if (<= (:price message) @current-price-point-atom)
        {:history []}
        (check-if-new-prices (conj history message)))
      {:history []})))

(defn loss-zone [current-price-point-atom difference-allowed]
  (fn [history {:keys [price time-ms] :as message}]
    (if (or (not price)
            (>= (- @current-price-point-atom price)
                difference-allowed))
      (conj history message)
      [])))

(defn buy-at
  ([initial-price]
     (buy-at initial-price 1))
  ([initial-price difference-allowed]
     (let [receiver (l/channel)           
           current-price-point (atom initial-price)           
           seller (->> receiver
                       (l/reductions* (loss-zone current-price-point
                                                 difference-allowed)
                                      [])
                       (l/filter* (partial elapsed-time >= max-time-in-loss-zone-ms))
                       (l/map* (constantly :sell)))
           upper (->> receiver
                      (l/reductions* (trailing-zone current-price-point)
                                     {:history []})
                      (l/filter* :new-price)
                      (l/map* :new-price))]
       (l/receive-all upper #(reset! current-price-point %))
       {:seller seller :receiver receiver
        :current-price-point (fn [] @current-price-point)})))
