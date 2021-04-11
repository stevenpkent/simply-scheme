(ns skent.ss-08
  (:require [skent.ss-02 :as sk]
            [clojure.string :as str]
            [skent.ss-01 :as sk1]))

;; pg 104 Every = Clojure map
(defn first-letters
  [sent]
  (map first sent))

(defn last-letters
  [sent]
  (map last sent))

(defn prepend-minus
  [sent]
  (map #(str "-" %) sent))

(defn plural
  [noun]
  (if (= (last noun) \y)
    (sk/word (apply str (sk/but-last noun)) 'ies)
    (sk/word noun 's)))

(defn plurals
  [coll]
  (map #(plural %) coll))

(defn doubles*
  [coll]
  (map #(sk/word % %) coll))

(defn squares
  [coll]
  (map #(* % %) coll))

(defn sentence-of-first-two
  [s]
  (->> 
   (str/split s #" ")
   (map #(sk/sentence (first %) (second %)))
   (apply sk/sentence)))

(defn g
  [wrd]
  (sk/sentence (sk/word "with" wrd) "you"))

(defn gs
  [coll]
  (map #(g %) coll))

;; p. 107 Keep = Clojure filter
(defn keep*
  [f coll]
  (filter f coll))

;; (keep* even? '(1 2 3 4 5))
;; (keep* #(= (last %) \e) '("please" "put" "the" "salami" "above" "the" "blue" "elephant"))
;; (keep* number? '(1 after 909))
;; (keep* #(sk/vowel? %) "piggies")

(defn parse-int 
  [number-string]
  (try (Integer/parseInt number-string)
       (catch Exception e nil)))

(defn keep-numbers-from-string
  [s]
  (->>
   (str/split s #"")
   (map #(parse-int %))
   (keep* some?)))

(defn first-letter-of-keeper-words
  [wrds]
  (->> wrds
       (keep* #(sk1/keep-word? %))
       (first-letters)))

;; pg. 108 Accumulate = Clojure reduce
;; (reduce + '(6 3 4 -5 7 8 9))
;; (reduce str '(A C L U))
;; (reduce max '(128 32 134 136 3))

(defn hyphenate
  [w1 w2]
  (sk/word w1 '- w2))

;; (reduce hyphenate '("ob" "la" "di" "ob" "la" "da"))

