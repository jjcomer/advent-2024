(ns y2025.d7
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2025/day/7

;; Generator Logic

;; Solution Logic

(def find-timelines
  (memoize
   (fn [grid r c]
     (cond
       (or (neg? c) (>= c (count (first grid)))) 0
       (>= r (count grid)) 1
       :else (let [timelines (if (= \^ (get-in grid [r c]))
                               (+ (find-timelines grid (inc r) (inc c))
                                  (find-timelines grid (inc r) (dec c)))
                               (find-timelines grid (inc r) c))]
               timelines)))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [rows (str/split-lines input)
        s-location [0 (str/index-of (first rows) \S)]]
    [s-location
     (mapv #(mapv identity %) rows)]))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[s-location grid]] 
  (loop [columns #{(second s-location)}
         rows (range (count (first grid)))
         splits 0]
    (if-let [row (first rows)]
      (let [[new-splits new-columns] (reduce (fn [[splits columns] column]
                                               (if (= \^ (get-in grid [row column]))
                                                 [(inc splits)
                                                  (conj columns (inc column) (dec column))]
                                                 [splits (conj columns column)]))
                                             [0 #{}]
                                             columns)]
        (recur new-columns (rest rows) (+ splits new-splits)))
      splits)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[[r c] grid]]
  (find-timelines grid r c))
