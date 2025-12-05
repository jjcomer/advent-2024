(ns y2025.d6
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2025/day/6

;; Generator Logic
(defn find-op [op]
  (case op
    "+" +
    "*" *))

(defn p1-parse [input]
  (let [raw-grid (map #(str/split (str/trim %) #"\s+") 
                      (str/split-lines input))
        problems (apply map vector raw-grid)]
    (mapv (fn [args]
            [(find-op (last args)) (mapv parse-long (butlast args))])
          problems)))

(defn p2-parse [input]
  (let [raw-grid (str/split-lines input)
        ops (map find-op (str/split (str/trim (last raw-grid)) #"\s+"))
        raw-nums (butlast raw-grid) 
        nums (into [] (comp (map #(->> raw-nums
                                       (map (fn [s] (subs s % (inc %))))
                                       (apply str)
                                       (str/trim)))
                            (partition-by str/blank?)
                            (remove #(= 1 (count %)))
                            (map #(map parse-long %)))
                   (range (count (first raw-nums))))]
    (mapv vector ops nums)))

;; Solution Logic

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  [(p1-parse input)
   (p2-parse input)])

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[input _]]
  (transduce (map (fn [[op args]] (reduce op args)))
             +
             input))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[_ input]]
  (transduce (map (fn [[op args]] (reduce op args)))
             +
             input))
