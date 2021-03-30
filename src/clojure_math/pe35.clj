(ns clojure-math.pe35
 (:require [clojure-math.helper.prime :as mprime]))

(def problem
 "Circular primes
The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
How many circular primes are there below one million?\n")

(defn cycle-word [word]
 (clojure.string/join "" [(subs word 1) (subs word 0 1)]))

(defn word-cycles [word]
 (loop [word-list [word]]
  (if (= (count word-list) (count (first word-list)))
   word-list
   (recur (conj word-list (cycle-word (last word-list)))))))

(defn cycle-prime? [prime]
 (every? true? (map #(mprime/prime? (Integer. %)) (word-cycles (str prime)))))
 

(defn run
 ([]
  (run 1000000))
 ([n]
  (count (filter cycle-prime? (filter mprime/prime? (range 2 n)))))
  )
; (defn below)
(defn answer [& args]
  (println "35: " args)
  (run))
