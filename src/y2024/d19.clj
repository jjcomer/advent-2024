(ns y2024.d19
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2024/day/19

;; Generator Logic

;; Solution Logic

(defn match? [pattern towel]
  (when (>= (count pattern) (count towel))
    (= (subs pattern 0 (count towel)) towel)))

(let [cache (atom {})]
  (defn match-pattern [towels pattern]
    (if-let [result (get @cache pattern)]
      result
      (if (empty? pattern)
        1
        (let [matching-towels (filter #(match? pattern %) towels)
              result (transduce (map #(match-pattern towels (subs pattern (count %)))) + matching-towels)]
          (swap! cache assoc pattern result)
          result)))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [[towels patterns] (str/split input #"\n\n")]
    [(str/split towels #", ")
     (str/split-lines patterns)]))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[towels patterns]]
  (count (filter #(->> % (match-pattern towels) pos?) patterns)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[towels patterns]]
  (transduce (map #(match-pattern towels %)) + patterns))
