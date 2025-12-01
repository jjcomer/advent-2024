(ns y2015.d13
  (:require [clojure.math.combinatorics :as combo]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2015/day/13

;; Generator Logic

(def reg #"^(\w+) would (\w+) (\d+) .* (\w+).$")

(defn parse-line [table line]
  (let [[_ name1 change amount name2] (re-matches reg line)
        change (if (= "lose" change) -1 1)
        amount (* change (parse-long amount))]
    (assoc table [name1 name2] amount)))

;; Solution Logic

(defn get-names [table]
  (into #{} (map first) (keys table)))

(defn compute-happiness [table arrangement] 
  (transduce (map table)
             +
             (partition 2 1 arrangement)))

(defn get-happiness [table arrangement]
  (let [circle-table (concat arrangement [(first arrangement)])
        happiness-1 (compute-happiness table circle-table)
        happiness-2 (compute-happiness table (reverse circle-table))]
    (+ happiness-1 happiness-2)))

(defn add-you [table]
  (reduce (fn [acc name]
            (assoc acc ["you" name ] 0 [name "you"] 0))
          table
          (get-names table)))

(defn find-max-happiness [table]
    (transduce (map (partial get-happiness table))
               max
               0
               (combo/permutations (get-names table))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/split-lines
       (reduce parse-line {})))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input] 
  (find-max-happiness input))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [table (add-you input)]
    (find-max-happiness table)))
