(ns y2024.d6
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2024/day/6

;; Generator Logic

;; Solution Logic

(defn turn [direction]
  (case direction
    [0 -1] [1 0]
    [1 0] [0 1]
    [0 1] [-1 0]
    [-1 0] [0 -1]))

(defn walk-grid [grid starting-location]
  (loop [location starting-location
         direction [0 -1]
         visited #{starting-location}] 
    (let [new-location (map + location direction)]
      (if-let [type (grid new-location)]
        (if (= \# type)
          (recur location (turn direction) visited)
          (recur new-location direction (conj visited location)))
        (conj visited location)))))

(defn check-loop [grid starting-location]
  (loop [location starting-location
         direction [0 -1]
         visited {starting-location #{direction}}]
    (if (grid location)
     (let [new-location (map + location direction)]
       (if (= \# (grid new-location))
         (or ((get visited location #{}) (turn direction))
             (recur location (turn direction) visited))
         (recur new-location direction (update visited location (fnil conj #{}) direction))))
      false)))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [grid (reduce (fn [grid [y row]]
                       (reduce (fn [grid [x point]]
                                 (assoc grid [x y] point))
                               grid
                               (map-indexed vector row)))
                     {}
                     (map-indexed vector (str/split-lines input)))
        start-location (first (for [[k v] grid :when (= v \^)] k))]
    [(assoc grid start-location \.)
     start-location]))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[grid start-location]]
  (count (walk-grid grid start-location)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[grid start-location]]
  (let [visited (walk-grid grid start-location)
        visited (disj visited start-location)] 
    (->> visited 
         (pmap #(check-loop (assoc grid % \#) start-location))
         (filter identity)
         count)))
