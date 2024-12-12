(ns y2024.d12
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; PROBLEM LINK https://adventofcode.com/2024/day/12

;; Generator Logic

;; Solution Logic

(defn visit [grid visited [x y :as coord]]
  (let [visited (conj visited coord)
        crop (get-in grid [x y])]
    (reduce (fn [visited [dx dy]]
              (let [next-coord [(+ x dx) (+ y dy)]]
                (if-let [next-crop (get-in grid next-coord)]
                  (if (and (= next-crop crop)
                           (not (contains? visited next-coord)))
                    (visit grid visited next-coord)
                    visited)
                  visited)))
            visited
            [[-1 0] [1 0] [0 -1] [0 1]])))

(defn gen-coords [grid]
  (for [y (range (count grid))
        x (range (count (grid 0)))]
    [x y]))

(defn find-regions [grid]
  (loop [visited #{} regions [] coords (gen-coords grid)]
    (if-let [coord (first coords)]
      (if (contains? visited coord)
        (recur visited regions (rest coords))
        (let [region (visit grid #{} coord)]
          (recur (set/union visited region) (conj regions region) (rest coords))))
      regions)))

(defn find-perimeter [region]
  (loop [coords region perimeter 0]
    (if-let [[x y] (first coords)]
      (let [edges (reduce + (for [[dx dy] [[-1 0] [1 0] [0 -1] [0 1]]
                                  :when (not (contains? region [(+ x dx) (+ y dy)]))]
                              1))]
        (recur (rest coords) (+ perimeter edges)))
      perimeter)))

(defn ranges [region]
  (let [xs (map first region)
        ys (map second region)]
    [(apply min xs) (apply max xs) (apply min ys) (apply max ys)]))

(defn find-perimeter2 [region]
  (let [[min-x max-x min-y max-y] (ranges region)
        tb (loop [xs (range min-x (inc max-x))
                  edges 0]
             (if-let [x (first xs)]
               (recur (rest xs)
                      (loop [ys (range min-y (inc max-y))
                             was-top false
                             was-bottom false
                             edges edges]
                        (if-let [y (first ys)]
                          (let [top-edge (and (contains? region [x y])
                                              (not (contains? region [(dec x) y])))
                                bottom-edge (and (contains? region [x y])
                                                 (not (contains? region [(inc x) y])))
                                edges (if (and top-edge (not was-top)) (inc edges) edges)
                                edges (if (and bottom-edge (not was-bottom)) (inc edges) edges)]
                            (recur (rest ys) top-edge bottom-edge edges))
                          edges)))
               edges))
        lr (loop [ys (range min-y (inc max-y))
                  edges 0]
             (if-let [y (first ys)]
               (recur (rest ys)
                      (loop [xs (range min-x (inc max-x))
                             was-left false
                             was-right false
                             edges edges]
                        (if-let [x (first xs)]
                          (let [left-edge (and (contains? region [x y])
                                               (not (contains? region [x (dec y)])))
                                right-edge (and (contains? region [x y])
                                                (not (contains? region [x (inc y)])))
                                edges (if (and left-edge (not was-left)) (inc edges) edges)
                                edges (if (and right-edge (not was-right)) (inc edges) edges)]
                            (recur (rest xs) left-edge right-edge edges))
                          edges)))
               edges))]
    (+ tb lr)))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (mapv #(into [] (seq %)) (str/split-lines input)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [grid]
  (transduce (map #(* (count %) (find-perimeter %)))
             +
             (find-regions grid)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [grid]
  (transduce (map #(* (count %) (find-perimeter2 %)))
             +
             (find-regions grid)))
