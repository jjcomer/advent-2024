(ns y2024.d7
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2024/day/7

;; Generator Logic

(defn parse-equation [line]
  (let [[result parameters] (str/split line #": ")]
    [(parse-long result) (mapv parse-long (str/split parameters #"\s+"))]))

;; Solution Logic

(defn solvable? [answer parameters]
  (if (= 1 (count parameters))
    (= answer (first parameters))
    (let [[a b & tail] parameters]
      (or (solvable? answer (concat [(+ a b)] tail))
          (recur answer (concat [(* a b)] tail))))))

(defn solvable-2? [answer parameters]
  (if (= 1 (count parameters))
    (= answer (first parameters))
    (let [[a b & tail] parameters]
      (when (<= a answer)
        (or (solvable-2? answer (concat [(+ a b)] tail))
            (solvable-2? answer (concat [(parse-long (str a b))] tail))
            (recur answer (concat [(* a b)] tail)))))))

(defn solve-problem [input test-fn]
  (transduce (comp (filter (fn [[answer parameters]]
                             (test-fn answer parameters)))
                   (map first))
             +
             input))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/split-lines
       (mapv parse-equation)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (solve-problem input solvable?))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (solve-problem input solvable-2?))

