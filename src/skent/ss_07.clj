(ns skent.ss-07
  (:require [skent.ss-02 :as sk]))

;; the book says Scheme doesn't allow this,
;; that you have to define multiple let blocks
;; to use the value of a in the calculation for b
;; Clojure actually allows you to do this, though
(defn let-question
  []
  (let [a (+ 7 4)
        b (* a 5)]
    (+ a b)))

;; exercise 7.3
;; the book said this doesn't work, but it actually does
(defn superlative
  [adjective word]
  (sk/sentence (sk/word adjective 'est) word))

;; exercise 7.4
;; these exercises kind of point out differences
;; between Scheme and Clojure. this function works as 
;; intended in Scheme because of the explanation in let-question above
;; and it does not work as intended in Clojure because a Cljoure let does
;; actually allow your let block to define a new value using something or things
;; previously defined in the let block.
;; so, below, + does get redefined to *. but in the 2nd line of the let,
;; * gets redefined to what you redefined + to in the first line.
;; in this case, that means * gets redefined to *
(defn sum-square
  [a b]
  (let [+ *
        * +]
    (* (+ a a) (+ b b))))

