(ns clojure-math.pe50)

(require '[clojure-math.helper.prime :as prime])

(defn run
 ([]
   (str (apply + (take 21 (filter #(prime/prime? %) (range)))) )))
(defn answer [& args]
 (println "50: " args)
 (run)
)

