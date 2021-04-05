(ns skent.ss-00
  (:gen-class))

(defn beatle? [name]
  (contains? #{"john" "paul" "george" "ringo"} name))

(defn but-first [input]
  (-> (reverse input) (butlast) (reverse)))
