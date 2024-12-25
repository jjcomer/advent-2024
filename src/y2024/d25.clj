(ns y2024.d25
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2024/day/25

;; Generator Logic

(defn parse-schematic [schematic]
  (reduce (fn [acc line]
            (let [incs (map #(if (= \# %) 1 0) line)]
              (map + acc incs)))
          (repeat 5 -1)
          (str/split-lines schematic)))

;; Solution Logic

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [input (str/split input #"\n\n")]
    (reduce (fn [[keys locks] schematic]
              (let [parsed (parse-schematic schematic)]
                (if (= \# (first schematic))
                  [keys (conj locks parsed)]
                  [(conj keys parsed) locks])))
            [[] []]
            input)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[keys locks]]
  (count (for [key keys
               lock locks
               :when (every? (fn [[k l]] (< (+ l k) 6)) 
                             (map vector key lock))]
           [key lock])))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input])
