(ns clojure-math.core
  (:gen-class)
  (:require 
   [clojure-math.pe35]
   [clojure-math.pe41]
   [clojure-math.pe50]
   [clojure-math.pe85]
   [clojure-math.pe76]
   [clojure-math.pe18]
   [clojure-math.pe67]
   [clojure-math.pe4]
   [clojure-math.sorting]))


(def pe_answers 
 (apply hash-map 
  (flatten 
   (map 
    #(vector (str %) 
      (symbol (str "clojure-math.pe" %) "answer"))
    [41 50 35 85 76 18 67 4]))))

(defn num? [number-string]
 (try
  (Integer/parseInt number-string)
  (catch Exception e nil)))

(def non-euler 
 (apply hash-map 
  (flatten 
   (map 
    #(vector (str %) 
      (symbol (str "clojure-math." %) "main"))
    ["sorting"]))))

(defn thisfunc []
  (println "this"))

(defn -main
  "Entry Point"
  [& args]
  (let [first-arg (first args)]
   (cond
    (contains? pe_answers first-arg)
    (println  (apply (resolve (get pe_answers first-arg)) (rest args)) )
    (contains? non-euler first-arg)
    (println (apply (resolve (get non-euler first-arg)) (rest args)))
    :else (println "Avalible args:\n" (keys pe_answers) "\n" (keys non-euler))
    )))
