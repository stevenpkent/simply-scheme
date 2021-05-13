(ns skent.ss-01
  (:require [clojure.string :as str]))

;; page 6
(defn word
  [& args]
  (apply str args))

;; page 8
(def words-to-ignore #{"a" "the" "an" "in" "of" "and" "for" "to" "with"})

(defn ignore-word?
  [word]
  (contains? words-to-ignore word))

(def real-word? (complement ignore-word?))

(defn acronym
  [phrase]
  (->> (str/split phrase #" ")
       (filter real-word?)
       (map first)
       (map str/upper-case)
       (str/join)))

;; STOPPED AT PAGE 10

(defn vowel?
  [letter]
  (contains? #{\a \e \i \o \u} letter))



(defn but-first
  [thing]
  (let [x (seq thing)]
    (if (= (class thing) String)
      (str/join (drop 1 x))
      (drop 1 x))))

(defn rotate
  [wrd]
  (word (but-first wrd) (first wrd)))

(defn pig-latin
  [wrd]
  (if (vowel? (first wrd))
    (word wrd "ay")
    (pig-latin (rotate wrd))))

(defn piggies-latin
  [words]
  (as-> words V
      (str/split V #" ")
      (map pig-latin V)
      (str/join " " V)))

(def menu-choices 
  '(
    ("small" "medium" "large")
    ("vanilla" "ultra chocolate" "rum raisin" "ginger")
    ("cone" "cup")))

(defn menu
  [choices]
  (for [size      (first choices) 
        flavor    (first (rest choices)) 
        container (last choices)] 
    (format "%S %S %S" size flavor container)))

;; loop - recur is tail-call optimized
(defn factorial
  [n]
  (loop [x n result 1]
    (if (= x 1)
      result
      (recur (dec x) (* x result)))))

(defn factorial*
  [n]
  (if (= n 1)
    1
    (* n (factorial* (- n 1)))))

(def beatles '("john" "paul" "george" "ringo"))
(def letters '("a" "b" "c" "d" "e"))

(defn- combinations-helper [the-set result]
  (-> (for [x the-set, r result :when (not-any? #{x} r)] (conj r x)) (set)))

(defn combinations
  [the-set chunk-size]
    (loop [i 0 result #{#{}}]
      (if (= i chunk-size)
        result
        (recur (+ i 1) (combinations-helper the-set result)))))
