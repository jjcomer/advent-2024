(ns y2024.d14
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2024/day/14

;; Generator Logic

(defn parse-particle [particle]
  (let [[p_x p_y v_x v_y] (map parse-long (re-seq #"-?\d+" particle))]
    {:p [p_x p_y]
     :v [v_x v_y]}))

;; Solution Logic

(def WIDTH 101)
(def HEIGHT 103)
(def MID_X (quot WIDTH 2))
(def MID_Y (quot HEIGHT 2))

(defn move-robot [robot time]
  (let [[p_x p_y] (:p robot)
        [v_x v_y] (:v robot)
        d-x (* time v_x)
        d-y (* time v_y)]
    [(mod (+ p_x d-x) WIDTH)
     (mod (+ p_y d-y) HEIGHT)]))

(defn to-quadrant [[x y]]
  (cond
    (and (< x MID_X) (< y MID_Y)) 1
    (and (< MID_X x) (< y MID_Y)) 2
    (and (< x MID_X) (< MID_Y y)) 3
    (and (< MID_X x) (< MID_Y y)) 4))

(defn print-grid [robots]
  (loop [rows (range HEIGHT)]
    (when-let [row (first rows)]
      (println (str/join "" (map (fn [col]
                                   (if (some #(= [col row] %) robots)
                                     \#
                                     \.))
                                 (range WIDTH))))
      (recur (rest rows)))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (map parse-particle (str/split-lines input)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [robots]
  (let [quadrants (into [] (comp (map #(move-robot % 100))
                                 (keep to-quadrant))
                        robots)] 
    (reduce * (vals (frequencies quadrants)))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [robots]
  (loop [time 1]
    (let [positions (into #{} (map #(move-robot % time) robots))]
      (if (= (count positions) (count robots))
        (do
          (print-grid positions)
          time)
        (recur (inc time))))))
