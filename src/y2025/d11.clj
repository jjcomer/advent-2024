(ns y2025.d11
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2025/day/11

;; Solution Logic

(def find-paths
  (memoize
   (fn [graph start end skip]
     (if (= start end)
       1
       (transduce (comp 
                   (remove skip)
                   (map #(find-paths graph % end skip)))
                  +
                  (get graph start))))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (reduce (fn [graph line]
            (let [[node & edges] (str/split line #":? ")]
              (assoc graph node edges)))
          {}
          (str/split-lines input)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [graph]
  (find-paths graph "you" "out" #{}))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [graph]
  (max (* (find-paths graph "svr" "dac" #{"out" "fft"})
          (find-paths graph "dac" "fft" #{"out" "svr"})
          (find-paths graph "fft" "out" #{"svr" "dac"}))
       (* (find-paths graph "svr" "fft" #{"out" "dac"})
          (find-paths graph "fft" "dac" #{"out" "svr"})
          (find-paths graph "dac" "out" #{"fft" "svr"}))))
