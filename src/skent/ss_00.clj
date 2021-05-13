(ns skent.ss-00)

(def the-beatles #{"john" "paul" "george" "ringo"})

(defn beatle? [name]
  (contains? the-beatles name))