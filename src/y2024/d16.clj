(ns y2024.d16
  (:require [clojure.string :as str]
            [clojure.data.priority-map :as pm]))

;; PROBLEM LINK https://adventofcode.com/2024/day/16

;; Generator Logic

;; Solution Logic

(def dirs [[-1 0] [1 0] [0 -1] [0 1]])

(defn addv [v1 v2]
  (mapv + v1 v2))

(defn find-path-1 [maze start]
  (loop [scores {}
         queue (pm/priority-map [start [0 1] [start]] 0)]
    (if-let [[[position direction path] score] (peek queue)]
      (if (= \E (get-in maze position))
        score
        (let [possible-moves (->> dirs
                                  (remove #(= direction (map - %)))
                                  (map (fn [d]
                                         (if (= d direction)
                                           [(addv d position) d (inc score)]
                                           [(addv d position) d (+ 1001 score)])))
                                  (filter (fn [[position _ _]]
                                            (#{\E \. \S} (get-in maze position))))
                                  (filter (fn [[position direction score]]
                                            (> (get scores [position direction] Long/MAX_VALUE) score))))]
          (recur (reduce (fn [scores [position direction score]]
                           (assoc scores [position direction] score))
                         scores
                         possible-moves)
                 (reduce (fn [queue [position direction score]]
                           (assoc queue [position direction (conj path position)] score))
                         (pop queue)
                         possible-moves))))
      (println "No path found"))))

(defn find-path-2 [maze start]
  (loop [scores {}
         queue (pm/priority-map [start [0 1] [start]] 0)
         result Long/MAX_VALUE
         tiles #{}]
    (if-let [[[position direction path] score] (peek queue)]
      (if (= \E (get-in maze position))
        (let [result (min result score)]
          (recur scores
                 (pop queue)
                 result
                 (if (= score result)
                   (into tiles path)
                   tiles)))
        (let [possible-moves (->> dirs
                                  (remove #(= direction (map - %)))
                                  (map (fn [d]
                                         (if (= d direction)
                                           [(addv d position) d (inc score)]
                                           [(addv d position) d (+ 1001 score)])))
                                  (filter (fn [[position _ _]]
                                            (#{\E \. \S} (get-in maze position))))
                                  (filter (fn [[position direction score]]
                                            (>= (get scores [position direction] Long/MAX_VALUE) score))))]
          (recur (reduce (fn [scores [position direction score]]
                           (assoc scores [position direction] score))
                         scores
                         possible-moves)
                 (reduce (fn [queue [position direction score]]
                           (assoc queue [position direction (conj path position)] score))
                         (pop queue)
                         possible-moves)
                 result
                 tiles)))
      (count tiles))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [maze (into [] (map #(into [] %)) (str/split-lines input))
        start-position (first (for [r (range (count maze))
                                    c (range (count (maze 0)))
                                    :when (= \S (get-in maze [r c]))]
                                [r c]))]
    [maze
     start-position]))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[maze start]]
  (find-path-1 maze start))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[maze start]]
  (find-path-2 maze start))

