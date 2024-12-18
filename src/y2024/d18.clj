(ns y2024.d18
  (:require [clojure.set :as set]))

;; PROBLEM LINK https://adventofcode.com/2024/day/18

;; Generator Logic

;; Solution Logic

(defn gen-maze [bytes n-to-drop]
  (let [dropped (into #{} (take n-to-drop bytes))] 
    (into #{} (for [x (range 71)
                    y (range 71)
                    :when (not (dropped [x y]))]
                [x y]))))

(defn neighbours [[x y]]
  [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])

(defn shortest-path [maze start end]
  (loop [queue [start]
         seen #{}
         i 0]
    (let [next (set (mapcat #(remove seen (filter maze (neighbours %))) queue))]
      (cond
        (contains? next end) (inc i)
        (empty? next) -1
        :else (recur next (set/union seen next) (inc i))))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (partition 2 (map parse-long (re-seq #"\d+" input))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [bytes]
  (shortest-path (gen-maze bytes 1024) [0 0] [70 70]))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [bytes]
  (let [first-bad (first (keep (fn [i]
                                 (when (= -1 (shortest-path (gen-maze bytes i) [0 0] [70 70]))
                                   i))
                               (range 1024 (count bytes))))]
    [first-bad (last (take first-bad bytes))]))

