(ns y2015.d6
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2015/day/6

;; Generator Logic

(def pattern #"^(.*) (\d+),(\d+).*?(\d+),(\d+)$")

(defn parse-matches [[_ mode & coords]]
  (concat [mode] (map parse-long coords)))

;; Solution Logic

(defn p1-modes [mode]
  (case mode
    "turn on" (constantly true)
    "turn off" (constantly false)
    "toggle" #(not %)))

(defn p2-modes [mode]
  (case mode
    "turn on" inc
    "turn off" #(max 0 (dec %))
    "toggle" #(+ 2 %)))

(defn perform-step [grid mode-map mode x1 y1 x2 y2]
  (let [mode-fn (mode-map mode)] 
    (reduce (fn [grid y]
              (assoc grid y (reduce (fn [row x]
                                      (update row x mode-fn))
                                    (nth grid y)
                                    (range x1 (inc x2)))))
            grid
            (range y1 (inc y2)))))

(defn empty-grid [default]
  (into [] (take 1000) (repeat (into [] (take 1000) (repeat default)))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/split-lines
       (map #(re-matches pattern %))
       (map parse-matches)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (let [final-grid (reduce (fn [grid [mode x1 y1 x2 y2]]
                             (perform-step grid p1-modes mode x1 y1 x2 y2))
                           (empty-grid false)
                           input)]
    (count (filter identity (flatten final-grid)))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [final-grid (reduce (fn [grid [mode x1 y1 x2 y2]]
                             (perform-step grid p2-modes mode x1 y1 x2 y2))
                           (empty-grid 0)
                           input)]
    (reduce + (flatten final-grid))))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
