(ns y2024.d10
  (:require [clojure.string :as str]))

(def empty-queue clojure.lang.PersistentQueue/EMPTY)

(defn parse-char [c]
  (- (int c) 48))

;; PROBLEM LINK https://adventofcode.com/2024/day/10

;; Generator Logic

;; Solution Logic

(defn find-trail-ends [grid [x y]]
  (loop [to-check (conj empty-queue [x y 0])
         visited #{}
         ends #{}]
    (if-let [[x y height] (first to-check)]
      (if (visited [x y])
        (recur (rest to-check)
               visited
               ends)
        (if (= 9 height)
          (recur (rest to-check)
                 (conj visited [x y])
                 (conj ends [x y]))
          (let [next-to-check (keep (fn [[x y]]
                                      (when (= (get grid [x y] -1) (inc height))
                                        [x y (get grid [x y])]))
                                    [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])]
            (recur (into (rest to-check) next-to-check)
                   (conj visited [x y])
                   ends))))
      ends)))

(let [cache (atom {})]
  (defn find-ranking* [grid x y height]
    (if-let [paths (get @cache [x y height])]
      paths
      (if (= 9 (get grid [x y] -1))
        1
        (let [paths (transduce (comp
                                (keep (fn [[x y]]
                                        (when (= (get grid [x y] -1) (inc height))
                                          [x y (get grid [x y])])))
                                (map (fn [[x y height]]
                                       (find-ranking* grid x y height))))
                               +
                               [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]])]
          (swap! cache assoc [x y height] paths)
          paths)))))

(defn find-ranking [grid [x y]]
  (find-ranking* grid x y 0))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [grid (reduce (fn [grid [y row]]
                       (reduce (fn [grid [x point]]
                                 (assoc grid [x y] (parse-char point)))
                               grid
                               (map-indexed vector row)))
                     {}
                     (map-indexed vector (str/split-lines input)))
        trailheads (for [[k v] grid :when (= v 0)] k)]
    [grid trailheads]))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[grid trailheads]]
  (transduce (map (fn [[x y]]
                    (count (find-trail-ends grid [x y]))))
             +
             trailheads))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[grid trailheads]]
  (transduce (map (fn [[x y]]
                    (find-ranking grid [x y])))
             +
             trailheads))

