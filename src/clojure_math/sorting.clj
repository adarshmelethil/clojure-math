(ns clojure-math.sorting
  (:gen-class)
  (:require [clojure.term.colors :as colors]
            [clojure.tools.cli :refer [parse-opts]]))

(defn binary-tree-insert
 ([tree val]
  (if (empty? tree)
   [val 1 [] []]
   (binary-tree-insert tree val [])))
 ([tree val index]
  (let [tree-val (get-in tree (conj index 0))]
   (cond
    (nil? tree-val) (assoc-in tree index [val 1 [] []])
    (<= val tree-val) (recur (update-in tree (conj index 1) inc) val (conj index 2))
    :else (recur (update-in tree (conj index 1) inc) val (conj index 3))))))


;; (defn )
(defn binary-tree-balance [tree]

  tree)

(defn tree->list [tree]
 (if (empty? tree) []
     (concat (tree->list (nth tree 1)) [(first tree)] (tree->list (nth tree 2)))))

(defn binary-tree-sort [thelist]
 (tree->list (reduce #(binary-tree-insert %1 %2) [] thelist )))

(defn insert-sorted
 ([tlist val] 
  (insert-sorted tlist val 0))
 ([tlist val n] 
  (if (>= n (count tlist))
   (conj tlist val)
   (if (<= val (nth tlist  n))
    (vec (concat (take n tlist) [val] (nthrest tlist n)))
    (recur tlist val (inc n))))))

(defn insert-sort [thelist]
 (reduce #(insert-sorted %1 %2) [] thelist))

(defn run [size length]
 (let [thelist (repeatedly length #(rand-int size))
       baseline (time (sort thelist))]

  (println "binary-tree")
  (if (not= (time (binary-tree-sort thelist)) baseline) (println "FAILED binary-tree-sort"))

  (println "insert-sort")
  (if (not= (time (insert-sort thelist)) baseline) (println "FAILED insert-sort"))
 ))

(def cli-options
 [["-s" "--size NUM_SIZE" "Size of the random numbers in list"
   :default 100
   :parse-fn #(Integer/parseInt %)]
  ["-l" "--length LIST_LENGTH" "Length of the list to be sorted"
   :default 10
   :parse-fn #(Integer/parseInt %)]
   ])

(defn main [& args]
 (println "Sorting..." args)
 (let [{{size :size length :length} :options} (parse-opts args cli-options)]
  (run size length)
  ; (let [thelist (repeatedly length #(rand-int size))]
  ;  (binary-tree-sort thelist)
  ;  (insert-sort thelist)
  ;  )
  
 ))

