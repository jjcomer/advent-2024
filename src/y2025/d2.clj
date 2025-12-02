(ns y2025.d2
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2025/day/2

;; Generator Logic

(defn parse-range [r]
  (let [[_ low high] (re-matches #"^(\d+)-(\d+)$" r)]
    [(parse-long low) (parse-long high)]))

;; Solution Logic

(def part1r #"^(\d+)\1$")
(def part2r #"^(\d+)\1+$")

(defn solve [re ranges]
  (transduce (comp (mapcat (fn [[low high]] (range low (inc high))))
                   (filter #(re-find re (str %))))
             +
             ranges))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (mapv parse-range (str/split (str/trim input) #",")))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (solve part1r input))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (solve part2r input))
