(ns y2025.d5
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2025/day/5

;; Generator Logic

;; Solution Logic

(defn generate-ranges [ranges]
  (map (fn [[low high]]
         #(<= low % high))
       ranges))

(defn compress-ranges [ranges]
  (let [ranges (sort-by first ranges)]
    (loop [current (first ranges)
           compressed []
           [next & tail] (rest ranges)]
      (if next
        (let [[cl ch] current
              [nl nh] next]
          (if (<= nl ch)
            (recur [cl (max ch nh)] compressed tail)
            (recur next (conj compressed current) tail)))
        (conj compressed current)))))

;; Entry Points
(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [[ranges ingredients] (str/split input #"\n\n")
        ingredients (mapv parse-long (str/split-lines ingredients))
        ranges (mapv #(let [[_ low high] (re-matches #"(\d+)-(\d+)" %)]
                        [(parse-long low) (parse-long high)])
                     (str/split-lines ranges))]
    [ranges ingredients]))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[ranges ingredients]]
  (let [range-fns (generate-ranges (compress-ranges ranges))]
    (count (filter (apply some-fn range-fns) ingredients))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[ranges _]]
  (reduce (fn [acc [low high]]
            (+ acc 1 (- high low)))
          0
          (compress-ranges ranges)))
