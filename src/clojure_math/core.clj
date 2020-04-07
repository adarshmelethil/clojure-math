(ns clojure-math.core
  (:gen-class)
  (:require 
   [clojure-math.pe35]
   [clojure-math.pe41]
   [clojure-math.pe50]
   [clojure-math.pe85]
   [clojure-math.pe76]
   [clojure-math.pe18]
   [clojure-math.pe67]))


(def pe_answers 
 (apply hash-map 
  (flatten (map 
   #(vector (str %) 
     (symbol (str "clojure-math.pe" %) "answer"))
    [41 50 35 85 76 18 67]))))
 ; "41" (symbol (str pe
 ; "50" 'pe50))

(defn -main
  "Entry Point"
  [& args]
   (let [problem_number (first args)]
    (cond
     (string? problem_number)
     (let [ans_func (get pe_answers problem_number)]
      (case ans_func
       nil (println "Answer " problem_number " not found. Avalible answer:\n" (keys pe_answers))
       (println (apply (resolve ans_func) []))))
     :else (println "No number provided. Avalible answer:\n" (keys pe_answers)))))
