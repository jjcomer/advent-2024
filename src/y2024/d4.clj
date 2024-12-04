(ns y2024.d4
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; PROBLEM LINK https://adventofcode.com/2024/day/4

;; Generator Logic

;; Solution Logic

(defn find-xmas-inner [grid x y dx dy]
  (loop [searching [\X \M \A \S] x x y y points #{}]
    (if-let [next-letter (first searching)]
      (if (= next-letter (get grid [x y] \!))
        (recur (rest searching) (+ x dx) (+ y dy) (conj points [x y]))
        false)
      points)))

(defn find-xmas [grid x y]
  (reduce (fn [found [dx dy]]
            (if-let [xmas (find-xmas-inner grid x y dx dy)]
              (conj found xmas)
              found))
          #{}
          [[-1 -1] [-1 1] [1 -1] [1 1] [0 1] [0 -1] [1 0] [-1 0]]))

(defn find-x-mas [grid x y]
  (if (= (grid [x y]) \A)
    (let [mas-1 (into #{} [(grid [x y]) (grid [(dec x) (dec y)]) (grid [(inc x) (inc y)])])
          mas-2 (into #{} [(grid [x y]) (grid [(dec x) (inc y)]) (grid [(inc x) (dec y)])])]
      (if (= mas-1 mas-2 #{\M \A \S})
        #{[[x y] [(dec x) (dec y)] [(inc x) (inc y)]
           [(dec x) (inc y)] [(inc x) (dec y)]]}
        #{}))
    #{}))

(defn count-pattern [grid find-pattern]
  (->> grid
       keys
       (reduce (fn [patterns [x y]]
                 (set/union patterns (find-pattern grid x y)))
               #{})
       count))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (reduce (fn [grid [y row]]
            (reduce (fn [grid [x point]]
                      (assoc grid [x y] point))
                    grid
                    (map-indexed vector row)))
          {}
          (map-indexed vector (str/split-lines input))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [grid]
  (count-pattern grid find-xmas))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [grid]
  (count-pattern grid find-x-mas))
