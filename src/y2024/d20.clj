(ns y2024.d20
  (:require [clojure.test :as t :refer [deftest]] 
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2024/day/20

;; Generator Logic

;; Solution Logic

(def empty-queue clojure.lang.PersistentQueue/EMPTY)

(defn get-neighbours [paths [row col]]
  (sequence
   (comp (map (fn [[dr dc]] [(+ row dr) (+ col dc)]))
         (filter paths))
   [[-1 0] [1 0] [0 -1] [0 1]]))

(defn get-distance [[row1 col1] [row2 col2]]
  (long (+ (abs (- row1 row2)) (abs (- col1 col2)))))

(defn find-distances [paths start]
  (loop [distances {start 0}
         queue (conj empty-queue start)]
    (if-let [position (peek queue)]
      (let [current-distance (get distances position)
            neighbours (remove #(contains? distances %) (get-neighbours paths position))]
        (recur (reduce (fn [distances neighbour]
                         (assoc distances neighbour (+ current-distance 1)))
                       distances
                       neighbours)
               (reduce conj (pop queue) neighbours)))
      distances)))

(defn find-close-pairs [path distance]
  (loop [pairs []
         path (sort-by first path)]
    (if-let [[r1 _ :as p] (first path)]
      (recur (transduce (comp (take-while #(<= (- (first %) r1) distance))
                              (keep #(let [d (get-distance p %)]
                                       (when (<= 2 d distance)
                                         [p % d]))))
                        conj
                        pairs
                        (rest path))
             (rest path))
      pairs)))

(defn find-savings [distances pairs]
  (count (filter (fn [[p1 p2 d]]
                   (>= (- (abs (- (distances p1) (distances p2))) d) 100))
                 pairs)))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (reduce (fn [[paths start end] [col row]]
            (reduce (fn [[paths start end] [row point]]
                      [(if (not= \# point) (conj paths [row col]) paths)
                       (if (= \S point) [row col] start)
                       (if (= \E point) [row col] end)])
                    [paths start end]
                    (map-indexed vector row)))
          [#{} nil nil]
          (map-indexed vector (str/split-lines input))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[paths start end]]
  (let [distances (find-distances paths end)
        pairs (find-close-pairs paths 2)] 
    (find-savings distances pairs)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[paths start end]]
  (let [distances (find-distances paths end)
        pairs (find-close-pairs paths 20)] 
    (find-savings distances pairs)))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
