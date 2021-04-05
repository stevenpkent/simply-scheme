(ns skent.ss-03
  (:require [skent.ss-02 :refer (sentence)]))

(defn add-s
  [verb]
  (str verb "s"))

(defn third-person
  [verb]
  (sentence "she " (add-s verb)))
