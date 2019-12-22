; (ns main)
; (:require [clojure.math.numeric-tower :as math])
(require '[clojure.string :as string])

(println 
"We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once.
For example, 2143 is a 4-digit pandigital and is also prime.
What is the largest n-digit pandigital prime that exists?\n")


(defn is-prime
 ([n]
  (cond 
  (< n 4) (> n 1)
  (= (mod n 2) 0) false
  :else (is-prime n 3)))
 ([n p]
  (cond 
   (> p (Math/sqrt n)) true
   (= (mod n p) 0) false
   :else (recur n (+ p 2)))))
(def is-prime (memoize is-prime))

(defn list-to-num [ll] (reduce #(+ (* %1 10) %2) ll))

(defn combinations [nums]
 (if (empty? nums)
  '()
   (reduce concat 
    (map
     (fn [a]
      (let [rest-of-nums (remove #(= a %) nums)]
       (if (empty? rest-of-nums) 
        [[a]]
        (map #(cons a %) (combinations rest-of-nums)))))
      (sort > nums)))))
(def combinations (memoize combinations))
(defn answer 
 ([]
  (answer 10))
 ([n]
  (let [ans (first (filter #(is-prime (list-to-num %)) (combinations (range 1 n))))]
   (if (nil? ans)
    (recur (- n 1))
    (list-to-num ans))))
)

(println (time (answer)))
