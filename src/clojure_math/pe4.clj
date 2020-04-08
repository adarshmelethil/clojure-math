(ns clojure-math.pe4
 (:gen-class)
 (:require [clojure-math.helper.prime :as mprime]
           [clojure.term.colors :as colors]))

(def problem
 "
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
\n")

; (defn digit-count [n]
;  (count (num->digits n)))

; (defn multiply-till2 [nums]
;  ; (println nums)
;  (if (< (count nums) 3) nums
;   (loop [n (last nums)
;          cur-mul (first nums)
;          rest-nums (butlast nums)]
;     (if (nil? cur-mul) [n]
;     (let [new-n (* cur-mul n)]
;      (if (> (digit-count new-n) 3)
;       [n (reduce * rest-nums)]
;       (recur new-n (second rest-nums) (rest rest-nums))))))))

; (defn valid-multiple [prime-factors]
;  (every? #(= (digit-count %) 3) (multiply-till2 prime-factors)))

; (def all-primes (time (take-while #(< % 999) (mprime/primes))))
; (defn multiples [num]
;  (loop [factors [] idx 0 n num]
;   (cond 
;    (= n 1) factors
;    (>= idx (count all-primes)) (conj factors n)
;    (some #(= n %) all-primes) (conj factors n)
;    :else 
;     (let [val (nth all-primes idx)]
;      (if (= (mod n val) 0)
;       (recur (conj factors val) 0 (/ n val))
;       (recur factors (inc idx) n))))))
; (def multiples (memoize multiples))

; (defn find-palindrome []
;  (loop [palin-half (palindrome-half-below (* 999 999))]
;   (let [palin-num (palindrome-half->num palin-half)]
;    (if (valid-multiple (multiples palin-num)) palin-num
;     (recur (dec-palindrome-half palin-half))))))

; (defn palindrome? [digits-list]
;  (cond
;   (nil? digits-list) true
;   (= (first digits-list) (last digits-list)) (recur (butlast (rest digits-list)))
;   :else false))
; (defn ans1 []
;  (loop [x 999 y 999]
;   (if (palindrome? (num->digits (* x y))) (do (println x y)(* x y))
;    (recur (if (< y 100) (dec x) x) (if (< y 100) x (dec y))))))

(defn dec-palindrome-half [digits-list]
 (if (= (last digits-list) 0) 
  (conj (dec-palindrome-half (vec (butlast digits-list))) 9)
  (update digits-list
    (dec (count digits-list)) dec)))

(defn digits->num [digits-list]
 (reduce #(+ (* %1 10) %2) (first digits-list) (rest digits-list)))

(defn pow [x y]
 (reduce * (repeat y x)))
(defn palindrome-half->num [digits-list]
 (+ (* (digits->num digits-list) (pow 10 (count digits-list)))
    (digits->num (reverse digits-list))))

(defn num->digits [n]
 (->> n str (map (comp read-string str)) vec))

(defn palindrome-half-below [upper-bound]
 (let [digits-list (num->digits upper-bound)]
  (let [half-upper-digits-list (take (/ (count digits-list) 2) digits-list)]
   (dec-palindrome-half (vec half-upper-digits-list)))))

(defn next-pair [coor]
 (let [[x y] coor]
  (if (= x 0)
   (if (= y 0)
    [0 1]
    (let [n (inc y)]
     (if (even? n)
      [(quot n 2) (quot n 2)]
      [(quot n 2) (inc (quot n 2))])))
   [(dec x) (inc y)])))
(defn pair->num [starter coor]
 (let [[x y] coor]
  (* (- starter x) (- starter y))))

(defn dec-plain [palin-half till]
 (if (<= (palindrome-half->num palin-half) till) palin-half
  (recur (dec-palindrome-half palin-half) till)))
(defn dec-coor [coor starter till]
 (if (<= (pair->num starter coor) till) coor
  (recur (next-pair coor) starter till)))
(defn ans2 [nine-digits]
 (loop [palin-half (palindrome-half-below (* nine-digits nine-digits))
        coor [0 0]]
   (let [palin-num (palindrome-half->num palin-half)
         coor-num (pair->num nine-digits coor)]
    (if (= palin-num coor-num) palin-num
     (if (> palin-num coor-num)
      (recur (dec-plain (dec-palindrome-half palin-half) coor-num) coor)
      (recur palin-half (dec-coor (next-pair coor) nine-digits palin-num)))))))

(defn answer
 ([]
  (time (ans2 999))))

