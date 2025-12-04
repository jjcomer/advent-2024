(ns y2025.d4
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2025/day/4

;; Generator Logic

;; Solution Logic

(def neighbours [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])

(defn get-neighbours [coord]
  (map #(mapv + coord %) neighbours))

(defn can-move? [grid coord]
  (< (count (keep grid (get-neighbours coord))) 4))

(defn to-remove [grid]
  (filter (partial can-move? grid) grid))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (reduce (fn [grid [y row]]
            (reduce (fn [grid [x i]]
                      (if (= \@ i)
                        (conj grid [y x])
                        grid))
                    grid
                    (map-indexed vector (seq row))))
          #{}
          (map-indexed vector (str/split-lines input))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (count (to-remove input)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [final-grid (reduce (fn [grid _]
                             (if-let [remove (seq (to-remove grid))]
                               (apply disj grid remove)
                               (reduced grid)))
                           input
                           (range))]
    (- (count input) (count final-grid))))
