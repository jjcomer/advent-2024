(ns y2015.d12
  (:require [clojure.test :as t :refer [deftest]]
            [cheshire.core :as json]
            [clojure.walk :as walk]))

;; PROBLEM LINK https://adventofcode.com/2015/day/12

;; Generator Logic

;; Solution Logic

(def num-reg #"(-?\d+)")

(defn find-numbers [s]
  (->> s
       (re-seq num-reg)
       (map first)
       (map parse-long)))

(defn remove-red [s] 
  (->> s
       json/parse-string
       (walk/prewalk (fn [x] (when-not (and (map? x)
                                            (some #(= "red" %) (vals x))) 
                               x)))
       json/generate-string))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  input)

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (reduce + (find-numbers input)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (->> input
       remove-red
       find-numbers
       (reduce +)))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
