(ns y2024.d2
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2024/day/2

;; Solution Logic

(defn p1-check [r]
  (or (every? (fn [[r1 r2]]
                (and (> r2 r1)
                     (<= 1 (abs (- r2 r1)) 3)))
              (partition 2 1 r))
      (every? (fn [[r1 r2]]
                (and (< r2 r1)
                     (<= 1 (abs (- r2 r1)) 3)))
              (partition 2 1 r))))

(defn p2-check [r]
  (if (p1-check r)
    true
    (loop [head [] tail r]
      (if-let [middle (first tail)]
        (if (p1-check (concat head (rest tail)))
          true
          (recur (conj head middle) (rest tail)))
        false))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/split-lines
       (mapv #(mapv parse-long (str/split % #" ")))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (->> input
       (filter p1-check)
       count))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (->> input
       (filter p2-check)
       count))
