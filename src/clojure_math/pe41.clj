(ns clojure-math.pe41)
(require '[clojure.string :as string])
(require '[clojure-math.helper.prime :as prime])

(defn pe41_problem []
 "Pandigital Prime
 We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once.
 For example, 2143 is a 4-digit pandigital and is also prime.
 What is the largest n-digit pandigital prime that exists?\n")


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

(defn list-to-num [ll] (reduce #(+ (* %1 10) %2) ll))

(defn answer
 ([]
  (answer 10))
 ([n]
  (let [ans (first (filter #(prime/prime? (list-to-num %)) (combinations (range 1 n))))]
   (if (nil? ans)
    (recur (- n 1))
    (list-to-num ans)))))
