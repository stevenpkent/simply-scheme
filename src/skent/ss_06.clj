(ns skent.ss-06
  (:require [skent.ss-02 :as sk]
            [clojure.string :as str]
            [skent.ss-01 :as sk1]))

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

;; exercise 6.5
(defn european-time
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

;; exercise 6.6
(defn teen?
  [age]
  (< 12 age 20))

;; exercise 6.7
(defn type-of
  [x]
  (let [t (type x)]
  (cond
    (= t clojure.lang.Symbol) 'WORD
    (= t clojure.lang.PersistentList) 'SENTENCE
    (= t java.lang.Boolean) 'BOOLEAN
    :else t)))

;; exercise 6.8
(defn indefinite-article
  [s]
  (let [f (first (str s))]
    (if (sk1/vowel? f) (str "an " s) (str "a " s))))

;; exercise 6.9
(defn this-many
  [n s]
  (if (> n 1)
    (str n " " (str s "s"))
    (str n " " s)))

;; exercise 6.10
(defn sort2 
  [ns]
  (sort ns))

;; exercise 6.11
(defn handle-feb
  [y]
  (let [even-4 (= (mod y 4) 0)
        even-100 (= (mod y 100) 0)
        even-400 (= (mod y 400) 0)]
    (cond
      (and even-4 (not even-100)) 29
      (and even-4 even-400) 29
      :else 28)))

(defn max-day
 [m y] 
  (cond
    (contains? #{1 3 5 7 8 10} m) 31
    (contains? #{4 6 9 11} m) 30
    :else (handle-feb y)))

(defn valid-date? 
  [m d y]
  (every? true? [(every? #(> % 0) [m d y]) 
                 (<= m 12) 
                 (<= d (max-day m y))]))

