(ns clojure-math.helper.prime)

(defn prime?
 ([n]
  (cond 
  (< n 4) (> n 1)
  (= (mod n 2) 0) false
  :else (prime? n 3)))
 ([n p]
  (cond
   (> p (Math/sqrt n)) true
   (= (mod n p) 0) false
   :else (recur n (+ p 2)))))
(def prime? (memoize prime?))

(defn- remove-multiples [p nums]
 (remove #(= (mod % p) 0) nums))

(defn primes-below [n]
 (loop [lps []
        lnums (range 2 n)]
  (if (empty? lnums)
   lps
   (recur
    (conj lps (first lnums))
    (remove-multiples (first lnums) lnums)))))

(defn is-divisible-by? [num nums]
 (loop [ns nums]
  (let [other (first ns)]
   (cond (nil? other)          false
         (= (mod num other) 0) true
         :else (recur (rest ns))))))

(defn next-prime [primelist]
 (loop [n (+ (last primelist) 2)]
  (if (= n 4) 3
   (if (is-divisible-by? n primelist)
    (recur (+ n 2)) n))))

(defn primes
 ([] (primes [2]))
 ([ps]
  (lazy-seq
   (cons (last ps) (primes (conj ps (next-prime ps)))))))

