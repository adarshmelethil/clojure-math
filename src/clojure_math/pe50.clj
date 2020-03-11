(ns clojure-math.pe50)

(require '[clojure-math.helper.prime :as prime])

(defn answer []
 (str (apply + (take 21 (filter #(prime/prime? %) (range)))) ))

