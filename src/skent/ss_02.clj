(ns skent.ss-02
  (:require [clojure.math.numeric-tower :as math]
            [clojure.string :as str]))

(defn sentence
  [& args]
  (->> (flatten args) (interpose " ") str/join))

(def re-space #" ")

(defn do-the-caps
  [& args]
  (as-> (sentence args) s  (str/split s re-space) (map str/capitalize s) (sentence s)))

;; (Math/sqrt)
;; (quote 12 6) = 2
;; (rem 24 7) = 3
;; (Math/random) = number between 0 and 1
;; (max 3 24 321 3) = 321
;; (math/expt 2 3) = 8 ;; had to add dependency to deps.edn, then require it in this namespace
;; (Math/round 17.5) = 18

;; first, butfirst, last, butlast, word, and count
(defn first*
  [thing]
  (first thing))

(defn but-first
  [thing]
  (rest thing))

(defn last*
  [thing]
  (last thing))

(defn but-last
  [thing]
  (drop-last 1 thing))

(defn word
  [& args]
  (apply str args))

(defn count*
  [thing]
  (count thing))

;; member? implemented as some in clojure
(defn member? 
  [target coll]
  (some? (some #(= target %) coll)))

;; (double-<f> 2 9 +)
(defn double-<f>
  [a b fn]
  (* 2 (fn a b)))

(defn every-first
  [coll]
  (-> (map first coll) (str/join)))

;; (keep #(if (vowel? %) %) "constantinople")
(defn vowel?
  [x]
  (member? x #{\a \e \i \o \u}))

;; (map #(if (consonant? %) %) "constantinople")
;; (filter consonant? "constantinople")
;; (for [x "constantinople" :when (consonant? x)] x)
;; (keep #(if (consonant? %) %) "constantinople")
;; (every? consonant? "trfgvb")
(defn consonant?
  [x]
  (not (vowel? x)))

;; (even?)
;; (odd?)
;; (number?)
;; (count)
;; (=)
;; (first)
;; if
;; (last)
;; (max)
;; (some), (some?)
;; (and) (not) (or)
;; (quot) quotient
;; (rand) (rand-int) (rand-nth) (shuffle)
;; (rem) remainder, (mod) modulus
;; (Math/sqrt)
