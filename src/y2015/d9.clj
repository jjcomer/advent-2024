(ns y2015.d9
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2015/day/9

(def t "London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141")

;; Generator Logic

(def reg #"(\w+) to (\w+) = (\d+)")

(defn parse-line [m l]
  (let [[_ to from distance] (re-matches reg l)
        distance (parse-long distance)]
    (-> m
        (assoc [to from] distance)
        (assoc [from to] distance))))

;; Solution Logic

(defn get-routes [distances]
  (let [cities (into #{} (flatten (keys distances)))]
    (combo/permutations cities)))

(defn get-distance [distances route]
  (transduce (map distances) + (partition 2 1 route)))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (reduce (fn [distances line]
            (parse-line distances line))
          {} 
          (str/split-lines input)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (let [routes (get-routes input)]
    (transduce (map #(get-distance input %)) min Long/MAX_VALUE routes)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [routes (get-routes input)]
    (transduce (map #(get-distance input %)) max 0 routes)))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
