(ns y2025.d10
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.data.priority-map :as pm]))

;; PROBLEM LINK https://adventofcode.com/2025/day/10

;; Generator Logic

(defn parse-line [line] 
  (let [[raw-lights & r] (str/split line #" ")
        raw-buttons (butlast r)
        joltage (last r)]
    [(into [] (rest (butlast (seq raw-lights))))
     (into [] (comp (map edn/read-string) (map vec)) raw-buttons)
     (vec (edn/read-string (str "[" (subs joltage 1 (dec (count joltage))) "]")))]))

;; Solution Logic

(defn gen-initial [goal]
  (vec (repeat (count goal) \.)))

(defn press-button [current button]
  (reduce (fn [current n]
            (update current n #(case % \. \# \# \.)))
          current
          button))

(defn part1 [goal buttons]
  (loop [queue (pm/priority-map [(gen-initial goal) 0] 0)
         seen #{(gen-initial goal)}]
    (when-let [[[current _] steps] (peek queue)] 
      (let [seen (conj seen current)]
        (if (= current goal)
          steps
          (let [next (sequence (comp (map (partial press-button current))
                                     (remove seen)
                                     (map (fn [n] [[n (inc steps)] (inc steps)])))
                               buttons)] 
            (recur (into (pop queue) next)
                   seen)))))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (into [] (map parse-line) (str/split-lines input)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (transduce (map (fn [[goal buttons _]] 
                    (part1 goal buttons)))
             +
             input))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  ":( used python")
