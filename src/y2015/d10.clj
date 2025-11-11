(ns y2015.d10
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2015/day/10

;; Generator Logic

;; Solution Logic

(defn look-say [number]
  (reduce (fn [new-num grouping]
            (conj new-num (count grouping) (first grouping)))
          []
          (partition-by identity number)))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (mapv #(- (int %) 48) (seq (str/trim input))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (count (reduce (fn [acc _]
                   (look-say acc))
                 input
                 (range 40))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (count (reduce (fn [acc _]
                   (look-say acc))
                 input
                 (range 50))))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
