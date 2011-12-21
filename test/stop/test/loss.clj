(ns stop.test.loss
  (:use stop.loss clojure.test)
  (:require [lamina.core :as l]))

(defn emit [channel message-fn & messages]
  (doseq [m (map (fn [args]
                   (if (coll? args)
                     (apply message-fn args)
                     (message-fn args))) messages)]
    (l/enqueue channel m)))

(defmacro sell-requested [channel]
  `(= :sell (first (l/channel-seq ~channel))))

(defmacro no-sell [channel]
  `(empty? (l/channel-seq ~channel)))

(deftest selling
  (testing "if it surpasses max time in loss zone sell"
    (let [{:keys [seller receiver]} (buy-at 10)]
      (emit receiver price-changed-message [0 9] [20000 8])
      (emit receiver time-changed-message max-time-in-loss-zone-ms)
      (is (sell-requested seller))))
  (testing "if it doesn't surpass max time in loss zone don't sell"
    (let [{:keys [seller receiver]} (buy-at 10)]
      (emit receiver price-changed-message [0 9] [20000 8])
      (emit receiver time-changed-message (dec max-time-in-loss-zone-ms))
      (is (no-sell seller))))
  (testing "if some price outside of loss zone in the meantime don't sell"
    (let [{:keys [seller receiver]} (buy-at 10)]
      (emit receiver price-changed-message [0 9] [20000 10])
      (emit receiver time-changed-message max-time-in-loss-zone-ms)
      (is (no-sell seller)))))

(defn keep-at [receiver price & {:keys [from-time for]}]
  (emit receiver price-changed-message [from-time price])
  (emit receiver time-changed-message (+ from-time for)))

(deftest trailing
  (let [t max-time-in-trailing-zone-ms]
    (testing "If it surpasses max time in trailing zone, price point goes up"
      (let [{:keys [seller receiver current-price-point]} (buy-at 10)]
        (emit receiver price-changed-message [0 13] [t 12])
        (is (= (current-price-point) 12))))
    (testing "Time messages also trigger price going up"
      (let [{:keys [seller receiver current-price-point]} (buy-at 10)]
        (emit receiver price-changed-message [0 13])
        (emit receiver time-changed-message t)
        (is (= (current-price-point) 13))))
    (testing "The new price point is used for selling"
      (let [{:keys [seller receiver current-price-point]} (buy-at 10)]
        (emit receiver price-changed-message [0 13] [t 12])
        (is (= (current-price-point) 12))
        (is (no-sell seller))
        (keep-at receiver 11 :from-time (inc t) :for max-time-in-loss-zone-ms)
        (is (sell-requested seller))))
    (testing "The new price point is the minimum of the not yet expired prices and the last one of the expired.
 Being 'expired' the messages that have been in the system for more than the max-time-in-trailing-zone-ms"
      (let [{:keys [seller receiver current-price-point]} (buy-at 10)]
        (emit receiver price-changed-message [0 13] [2 11] [t 14])
        (is (= (current-price-point) 11))
        (emit receiver time-changed-message (+ t max-time-in-trailing-zone-ms))
        (is (= (current-price-point) 14)))
      (let [{:keys [seller receiver current-price-point]} (buy-at 10)]
        (emit receiver price-changed-message [0 13] [2 11] [3 12] [(+ t 3) 14])
        (is (= (current-price-point) 12))))
    (testing "A value equal or below causes trailing to abort"
      (let [{:keys [seller receiver current-price-point]} (buy-at 10)]
        (emit receiver price-changed-message [0 13] [(dec t) 9] [t 14])
        (is (= (current-price-point) 10))))))

