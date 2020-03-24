(ns clojure-math.pe76)

(def problem 
 "
It is possible to write five as a sum in exactly six different ways:

4 + 1
3 + 2
3 + 1 + 1
2 + 2 + 1
2 + 1 + 1 + 1
1 + 1 + 1 + 1 + 1

How many different ways can one hundred be written as a sum of at least two positive integers?
 ")

(defn- possible-nums [n]
 (vec (range 2 (+ (quot n 2) 1))))
(def possible-nums (memoize possible-nums))

(defn- sums-with-multiple [n v]
 (conj
  (vec (take (- (quot n v) 1) (repeat v)))
  (+ v (- n (* (quot n v) v)))))
(def sums-with-multiple (memoize sums-with-multiple))

(defn take-n [start vec-nums n]
 ; (println n start vec-nums)
 (if (<= (count vec-nums) n)
  (vec [(vec (concat start [(reduce + (take n vec-nums))]))])
  (vec (dedupe
   (vec (map sort (concat
    [(vec (concat start [(reduce + (take n vec-nums))] (nthrest vec-nums n)))]
    (take-n (concat start [(first vec-nums)]) (rest vec-nums) n))))))))
(def take-n (memoize take-n))

(defn- combine-nums [ns]
 (if (<= (count ns) 2)
  (vec [ns])
  (let [ns-rest (rest ns)]
   (vec (concat [ns] (reduce concat (map (fn [taken]
     (vec (map #(vec (concat [(first ns)] %)) (take-n [] ns-rest taken))))
     (range 2 (+ (count ns-rest) 1)))))))))
(def combine-nums (memoize combine-nums))

(defn- sums-without-one [n]
 (vec (reduce concat (map #(combine-nums %)
  (map #(sums-with-multiple n %)
    (possible-nums n))))))
(def sums-without-one (memoize sums-without-one))

(defn- counting-summations [n]
 (if (= n 2) 1
  (let [without-ones (sums-without-one n)]
   
   (let [n-1 (counting-summations (- n 1))]
    (let [ans (+ n-1 (count without-ones) 1)]
     (println n without-ones (count without-ones) n-1 "=" ans)
     ans
     )))))

(def counting-summations (memoize counting-summations))

(defn series-diff [series]
 (map - (rest series) (take (- (count series) 1) series)))


(defn answer
 ([]
  (answer 20))
 ([n]
  ; (take-n [] [2 2 2 3] 2)
  ; (let [n (map counting-summations (range 2 20))]
  ;  (println n)
  ;  (let [d1 (series-diff n)]
  ;   (println d1)
  ;   (let [d2 (series-diff d1)]
  ;    (println d2)
  ;   )
  ;  )
  ;  )
  ; (sums-without-one 9)
  (time (counting-summations n))
  ))

