(ns y2025.d9
  (:require [clojure.edn :as edn]
            [clojure.math.combinatorics :as comb]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2025/day/9

;; Generator Logic

;; Solution Logic

(defn rectangle [[[a b] [c d]]]
  [(min a c) (min b d) (max a c) (max b d)])

(defn green-lines [points]
  (map rectangle (partition 2 1 points)))

(defn area [[a b c d]]
  (* (- c a -1) (- d b -1)))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/split-lines
       (mapv #(edn/read-string (str "[" % "]")))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [points]
  (transduce (comp (map rectangle)
                   (map area))
             max
             -1
             (comb/combinations points 2)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [points]
  (let [green (green-lines points)]
    (transduce (map rectangle)
               (completing
                (fn [x [a b c d :as rec]]
                  (let [ar (area rec)]
                    (if (> ar x)
                      (if (some (fn [[p q r s]]
                                  (and (< a r) (< b s) (> c p) (> d q)))
                                green)
                        x
                        ar)
                      x))))
               -1
               (comb/combinations points 2))))
