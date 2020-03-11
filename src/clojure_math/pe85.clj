(ns clojure-math.pe85
 (:require [clojure-math.helper.prime :as mprime]))

(def problem
 "By counting carefully it can be seen that a rectangular grid measuring 3 by 2 contains eighteen rectangles:
Although there exists no rectangular grid that contains exactly two million rectangles, find the area of the grid with the nearest solution.\n")


(defn fits [gr gc r c]
 (cond
  (< gr r) (throw (Exception. (str "Rect rows '" r "' are larger than grid rows '" gr "'.")))
  (< gc c) (throw (Exception. (str "Rect cols '" c "' are larger than grid rows '" gc "'.")))
  (some true? (map #(<= % 0) [gr gc r c]))
   (throw (Exception. (str "Must have a minimum of 1 row and col: " (clojure.string/join ", " [gr gc r c])))))
 (* (+ (- gr r) 1) (+ (- gc c) 1)))

(defn sub-rects [gr gc]
 (reduce concat (map 
  (fn [r] (map 
           (fn [c] (vec [r c]))
           (range 1 (+ gc 1))))
  (range 1 (+ gr 1)))))


(defn total-sub-rects [gr gc]
 (reduce + (map #(fits gr gc (first %) (second %)) (sub-rects gr gc))))


(defn sub-rect-count [gr gc]
 ; Grid Row Col
 (let [grc (sort [gr gc])]
  (if (= (first grc) 1)
   (if (= (second grc) 1)
    1
    (* (+ (second grc) 1) (/ (second grc) 2)))
   (* (+ (* (sub-rect-count 1 (first grc)) (second grc)) (sub-rect-count 1 (first grc))) (/ (second grc) 2)))))
(def sub-rect-count (memoize sub-rect-count))

; (defn below)
(defn answer
 ([]
  (answer 2000000))
 ([n]
  
  (let [n 30]
   (map 
    (fn [r] (println (clojure.string/join "\t" 
     (map #(if (>= r %) (total-sub-rects r %) " ") (range 1 n)) )))
    (range 1 n)))
  ; (println "total-sub-rects: " (time (total-sub-rects 100 100)))
  ; (println "sub-rect-count: " (time (sub-rect-count 100 100)))
  ; (map #(sub-rect-count (first %) (second %)) (reduce concat (map
  ;  (fn [x] (map #(vec [x %]) (range x 11)))
  ;  (range 1 11)
  ; )))
  ; (sub-rect-count 2 2)
  ))
