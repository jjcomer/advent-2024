(ns y2025.d8
  (:require [clojure.math :as m]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2025/day/8

;; Generator Logic

;; Solution Logic

(defn distance [[p1x p1y p1z] [p2x p2y p2z]]
  (+ (m/pow (- p2x p1x) 2)
     (m/pow (- p2y p1y) 2)
     (m/pow (- p2z p1z) 2)))

(defn parent [parents x]
  (let [p (parents x)]
    (if (= p x)
      p
      (recur parents p))))

(defn join [parents sizes x y]
  (let [px (parent parents x)
        py (parent parents y)]
    (if (= px py)
      [parents sizes]
      (let [[px py] (if (> (sizes px) (sizes py))
                      [py px]
                      [px py])]
        [(assoc parents px py)
         (assoc sizes
                py (+ (sizes px) (sizes py))
                px 0)]))))

(defn get-distances [points]
  (for [x (range (count points))
        y (range (inc x) (count points))
        :let [d (distance (points x) (points y))]]
    [d x y]))

(defn p1-crunch [i distances parents sizes]
  (loop [i (range i)
         distances distances
         parents parents
         sizes sizes]
    (if (empty? i)
      [parents sizes]
      (let [[_ x y] (first distances)
            [new_parent new_sizes] (join parents sizes x y)]
        (recur (rest i)
               (rest distances)
               new_parent
               new_sizes)))))

(defn p2-crunch [n distances parents sizes]
  (loop [distances distances
         parents parents
         sizes sizes]
    (let [[_ x y] (first distances)
          [new_parents new_sizes] (join parents sizes x y)]
      (if (= n (new_sizes (parent new_parents x)))
        [x y]
        (recur (rest distances)
               new_parents
               new_sizes)))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/split-lines
       (mapv #(mapv parse-long (str/split % #",")))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [points]
    (let [distances (sort-by first (get-distances points))
          parents (into [] (range (count points)))
          sizes (into [] (repeat (count points) 1))
          [_ final_sizes] (p1-crunch 1000 distances parents sizes)]
      (apply * (take 3 (sort-by identity > final_sizes)))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [points]
  (let [distances (sort-by first (get-distances points))
        parents (into [] (range (count points)))
        sizes (into [] (repeat (count points) 1))
        [x y] (p2-crunch (count points) distances parents sizes)]
    (* (get-in points [x 0])
       (get-in points [y 0]))))
