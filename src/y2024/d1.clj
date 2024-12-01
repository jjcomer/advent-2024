(ns y2024.d1
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2024/day/1

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/split-lines
       (reduce (fn [[list-a list-b] line]
                 (let [[a b] (str/split line #"   ")]
                   [(conj list-a (parse-long a)) (conj list-b (parse-long b))]))
               [[] []])))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[list-a list-b]]
  (let [distances (map #(abs (- %1 %2)) (sort list-a) (sort list-b))]
    (reduce + distances)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[list-a list-b]]
  (let [freq (frequencies list-b)]
    (transduce (map #(* % (freq % 0)))
               +
               list-a)))
