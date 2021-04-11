(ns skent.ss-05
  (:require [skent.ss-02 :as sk]
            [clojure.string :as str]))

;; sk/first*
;; sk/last*
;; sk/but-first
;; sk/but-last

;; text wanted to define our own second fn
;; Clojure has (second) natively

(defn item
  [index coll]
  (let [what-they-want (- index 1)
        outside-range? (or (< what-they-want 0) (> what-they-want (count coll)))]
    (if outside-range?
      "index out of range"
      (nth coll what-they-want))))

;; sk/word
;; sk/sentence

;; exercise 5.2
(defn f1
  [l1 l2]
  (concat (sk/but-first l1) (sk/but-last l2)))

(defn f2
  [l1 l2]
  (let [part-1 (f1 l1 l2)
        part-2 (str/join (str (first l1) (last l2)))]
    (flatten (list part-1 part-2))))

(defn f3
  [l1 _]
  (concat l1 l1))

(defn f4
  [l1 l2]
  (->>
   (list (second l1) (second l2))
   (apply str)
   str/upper-case))

;; exercise 5.14
(defn third
  [thing]
  (nth thing 2))

;; exercise 5.15
(defn first-two
  [word]
  (str/upper-case (str/join (take 2 word))))

;; exercise 5.16
(defn two-first
  ([w1 w2]
   (str (first w1) (first w2)))
  ([sent]
   (-> (map first sent) str/join)))

;; exercise 5.17
(defn knight
  [name]
  (conj name 'sir))

;; exercise 5.18
(defn ends
  [name]
  (sk/word (sk/first* name) (sk/last* name)))

;; exercise 5.19
(defn insert-and
  [sent]
  (let [bl (sk/but-last sent)
        l (list (sk/last* sent))
        l2 (conj l 'and)]
    (flatten (list bl l2))))

;; exercise 5.20
(defn middle-names
  [l]
  (sk/but-last (sk/but-first l)))

;; exercise 5.21
(defn query
  [sent]
  (->>
   (flatten 
    (list 
     (reverse (take 2 sent)) 
     (str (last sent) "?")))
   (str/join " ")))