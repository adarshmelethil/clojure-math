(ns helper)

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

