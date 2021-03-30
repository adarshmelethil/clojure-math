(ns clojure-math.pe4
 (:gen-class)
 (:require [clojure-math.helper.prime :as mprime]
           [clojure.term.colors :as colors]))

(def problem
 "
A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

Find the largest palindrome made from the product of two 3-digit numbers.
\n")

(defmacro mtime
  [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr]
     [(/ (double (- (. System (nanoTime)) start#)) 1000000.0) ret#]))

(defonce counter (atom {}))
(defmacro defncount [name args body]
 ; (swap! counter #(assoc % (keyword name) 0))
 `(defn ~name ~args
    ; (swap! counter #(update % (keyword ((meta #'~name) :name)) inc))
    ~body
  )
 )

(defncount dec-palindrome-half [digits-list]
 (if (= (last digits-list) 0) 
  (conj (dec-palindrome-half (vec (butlast digits-list))) 9)
  (update digits-list
    (dec (count digits-list)) dec)))

(defncount digits->num [digits-list]
 (reduce #(+ (* %1 10) %2) (first digits-list) (rest digits-list)))
(def digits->num (memoize digits->num))

(defncount pow [x y]
 (reduce * (repeat y x)))
(defncount palindrome-half->num [digits-list]
 (+ (* (digits->num digits-list) (pow 10 (count digits-list)))
    (digits->num (reverse digits-list))))
(def palindrome-half->num (memoize palindrome-half->num))

(defncount num->digits [n]
 (->> n str (map (comp read-string str)) vec))
(def num->digits (memoize num->digits))

(defncount palindrome-half-below [upper-bound]
 (let [digits-list (num->digits upper-bound)]
  (let [half-upper-digits-list (take (/ (count digits-list) 2) digits-list)]
   (dec-palindrome-half (vec half-upper-digits-list)))))

(defncount next-pair [coor]
 (let [[x y] coor]
  (if (= x 0)
   (if (= y 0)
    [0 1]
    (let [n (inc y)]
     (if (even? n)
      [(quot n 2) (quot n 2)]
      [(quot n 2) (inc (quot n 2))])))
   [(dec x) (inc y)])))
(defncount pair->num [starter coor]
 (let [[x y] coor]
  (* (- starter x) (- starter y))))

(defncount dec-plain [palin-half till]
 (if (<= (palindrome-half->num palin-half) till) palin-half
  (recur (dec-palindrome-half palin-half) till)))
; (def dec-plain (memoize dec-coor))

(defncount dec-coor [coor starter till]
 (if (<= (pair->num starter coor) till) coor
  (recur (next-pair coor) starter till)))
; (def dec-coor (memoize dec-coor))

(defncount ans2 [nine-digits]
 (loop [palin-half (palindrome-half-below (* nine-digits nine-digits))
        coor [0 0]]
   (let [palin-num (palindrome-half->num palin-half)
         coor-num (pair->num nine-digits coor)]
    (if (= palin-num coor-num) palin-num
     (if (> palin-num coor-num)
      (recur (dec-plain (dec-palindrome-half palin-half) coor-num) coor)
      (recur palin-half (dec-coor (next-pair coor) nine-digits palin-num)))))))

; Abdul's
(defn make-sym [^Integer half]
  (let [^String str-half (str half)]
    (Integer/parseInt
     (str str-half
         ;; (s/reverse str-half)))))
         (apply str (reverse str-half))))))

(defn dec-plain-till [palin-half till]
 (if (<= (make-sym palin-half) till) palin-half
  (recur (dec palin-half) till)))

(defn ans3 [nine-digits]
 (loop [palin-half nine-digits
        coor [0 0]]
  (let [palin-num (make-sym palin-half)
        coor-num  (pair->num nine-digits coor)] ; 999-1 * 999-3 < 998899 
    (if (= palin-num coor-num) palin-num
     (if (> palin-num coor-num)
      (recur (dec-plain-till (dec palin-half) coor-num) coor)
      (recur palin-half (dec-coor (next-pair coor) nine-digits palin-num)))))))

(defn run 
 ([]
  (println (time (ans2 999)))
  (doseq [cc @counter]
   (println cc))
  (println (time (ans3 999)))))
(defn answer [& args]
  (println "4: " args)
  (run))

