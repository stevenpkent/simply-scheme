(ns skent.ss-04
  (:require [clojure.math.numeric-tower :as math]))

(defn square
  [x]
  (* x x))

(def states-capitols
  {#{"AK" "Alaska"} "Juneau"
   #{"AL" "Alabama"} "Montgomery"
   #{"AR" "Arkansas"} "Little Rock"
   #{"AZ" "Arizona"} "Phoenix"
   #{"CA" "California"} "Sacramento"})

(defn capitol
  [state coll]
  (some-> (filter #(contains? (key %) state) coll) first val))

(defn f->c
  [f]
  (* (/ 5 9) (- f 32)))

(defn c->f
  [c]
  (+ (* (/ 9 5) c) 32))

(defn fourth
  [x]
  (math/expt x 4))

(defn fourth*
  [x]
  (* x x x x))

(defn fourth**
  [x]
  (* (square x) (square x)))

(defn absolute-value
  [x]
  (Math/abs x))

(defn absolute-value*
  [x]
  (Math/sqrt (square x)))

(defn scientific
  [x power]
  (* x (Math/pow 10 power)))

(defn sci-coefficent
  [target-n power]
  (/ target-n (Math/pow 10 power)))

(defn sci-exponent
  [x y]
  (let [a (/ y x)]
    (Math/log10 a)))

(defn discount
  [price discount-percentage]
  (let [discount (* discount-percentage 0.01)
        discount-amount (* price discount)]
    (- price discount-amount)))

(defn compute-tip
  [bill-amount]
  (->>
   (- (Math/ceil (* bill-amount 1.15)) bill-amount)
   (format "%.2f")
   (Double/parseDouble)))







