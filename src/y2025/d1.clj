(ns y2025.d1
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2025/day/1

;; Generator Logic

(defn parse-entry [l]
  [(if (= \L (first l)) -1 1)
   (parse-long (subs l 1))])

;; Solution Logic
(defn move-safe [current [direction distance]]
  (mod (+ current (* direction distance)) 100))

(defn move-safe-2 [directions]
  (loop [current 50 
         zeros 0 
         directions directions]
    (if-let [[direction distance] (first directions)]
      (let [temp (+ current (* direction distance))
            zs (+ (quot (abs temp) 100)
                  (if (and (pos? current) (<= temp 0))
                    1 0))] 
        (recur (mod temp 100) (+ zeros zs) (rest directions)))
      zeros)))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/split-lines
       (mapv parse-entry)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (let [locations (reductions move-safe 50 input)]
    (count (filter zero? locations))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (move-safe-2 input))
