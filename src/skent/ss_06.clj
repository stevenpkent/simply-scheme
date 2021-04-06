(ns skent.ss-06
  (:require [skent.ss-02 :as sk]
            [clojure.string :as str]))

(defn greet
  [name]
  (if (= (first name) 'professor)
    (sk/sentence '(i hope i am not bothering you) 'professor (last name))
    (sk/sentence '(what up yo?) (first name))))

(defn greet*
  [name]
  (cond
    (= (first name) 'professor) (sk/sentence '(i hope i am not bothering you) 'professor (last name))
    :else (sk/sentence '(what up yo?) (first name))))

(defn greet**
  [name]
  (condp = (= (first name) 'professor)
    true (sk/sentence '(i hope i am not bothering you) 'professor (last name))
    false (sk/sentence '(what up yo?) (first name))))

(defn member? 
  [coll looking-for]
  (->
   (some #(= % looking-for) coll)
   some?))

(defn evenly-divisible?
  [n1 n2]
  (= (mod n1 n2) 0))

(defn string-contains?
  [s re]
  (some? (re-find re s)))

(defn create-regex
  [s]
  (re-pattern s))

(defn buzz
  [num]
  (let [divisor           7
        divides-evenly?   (evenly-divisible? num divisor)
        re                (create-regex (str divisor))
        contains-seven?   (string-contains? (str num) re)]
    (if (or divides-evenly? contains-seven?) 'buzz num))) 

;; example from page 78
(defn gotta-be-special-don't-you?
  [decimal-list]
  (let [parted (partition 2 1 decimal-list)
        filtered (filter #(apply < %) parted)
        roman-sum-unordered (apply + (map #(Math/abs (apply - %)) filtered))
        literal-sum-unordered (apply + (flatten filtered))
        literal-sum-all (apply + decimal-list)]
    (+ (- literal-sum-all literal-sum-unordered) roman-sum-unordered)))

;; example from page 78
(defn roman
  [s]
  (let [decimal-list (map #({\I 1, \V 5, \X 10, \L 50, \C 100, \D 500, \M 1000} %) s)]
    (if (apply >= decimal-list)
      (apply + decimal-list)
      (gotta-be-special-don't-you? decimal-list))))

(defn european-time
;; exercise 6.5
  [s]
  (let [[n am-pm] (str/split s #" ")
        number (Integer/parseInt n)
        is-12 (= number 12)]
    (if (= am-pm "AM")
      (if is-12 (+ number 12) number)
      (if is-12 number (+ number 12)))))

;; exercise 6.5
(defn american-time
  [n]
  (cond
    (= n 24) "12 AM"
    (< n 12) (str n " AM")
    (= n 12) (str n " PM")
    :else (str (- n 12) " PM")))

