(ns skent.simply-scheme
  (:gen-class))

(defn greet
  [data]
  (println (str "Hello, " (or (:name data) "World") "!")))

(defn -main
  [& args]
  (greet {:name (first args)}))
