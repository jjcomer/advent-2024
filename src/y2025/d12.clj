(ns y2025.d12
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2025/day/12

;; Generator Logic

(defn parse-shape [shape]
  (count (filter #{\#} shape)))

(defn parse-tree [tree]
  (let [[raw-dim raw-presents] (str/split tree #": ")
        dim (str/split raw-dim #"x")
        presents (str/split raw-presents #" ")]
    [(mapv parse-long dim)
     (mapv parse-long presents)]))

;; Solution Logic

(defn check-fit [presents [[h w] required]]
  (let [area (* h w)
        minimum_area (transduce (map (fn [[present required]]
                                       (* required (nth presents present))))
                                +
                                (map-indexed vector required))]
    (>= area minimum_area)))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [chunks (str/split input #"\n\n")
        trees (mapv parse-tree (str/split-lines (last chunks)))
        presents (mapv parse-shape (butlast chunks))]
    [presents trees]))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[presents trees]]
  (->> trees
       (filter (partial check-fit presents))
       count))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input])
