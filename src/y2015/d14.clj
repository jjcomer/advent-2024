(ns y2015.d14
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2015/day/14

;; Generator Logic

(def reg #"^(\w+) .*? (\d+) .*? (\d+) .*? (\d+).*$")

(defn parse-line [line]
  (let [[_ name speed travel-time rest-time] (re-matches reg line)]
    [name 
     (parse-long speed) 
     (parse-long travel-time) 
     (parse-long rest-time)]))

;; Solution Logic

(defn compute-distance [[_ speed travel-time rest-time] time]
  (+ (* speed travel-time (quot time (+ travel-time rest-time)))
     (* speed
        (min travel-time
             (mod time (+ travel-time rest-time))))))

(defn find-winner [reindeer time]
  (first (transduce (map (fn [r]
                           [(first r) (compute-distance r time)]))
                    (completing (fn [[_ d1 :as r1] [_ d2 :as r2]]
                                  (if (< d1 d2) r2 r1)))
                    [nil 0]
                    reindeer)))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/split-lines
       (mapv parse-line)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (transduce (map #(compute-distance % 2503))
             max
             0
             input))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [results (map #(find-winner input %) (map inc (range 2503)))
        result-counts (reduce-kv (fn [acc name wins]
                                   (assoc acc wins name))
                                 (sorted-map)
                                 (frequencies results))]
    (first (last result-counts))))
