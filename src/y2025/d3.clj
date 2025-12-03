(ns y2025.d3
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2025/day/3

;; Generator Logic

(defn parse-line [l]
  (mapv #(- (int %) 48) (seq l)))

;; Solution Logic

(defn find-joltage [length batteries]
  (loop [joltage []
         batteries batteries]
    (if (= length (count joltage))
      (reduce #(+ %2 (* 10 %1)) 0 joltage)
      (let [[hi hv] (reduce (fn [[mi mv :as c] [xi xv :as x]]
                              (cond
                                (> mv xv) c
                                (< mv xv) x
                                (< mi xi) c))
                            (map-indexed vector (take (+ (count batteries) 
                                                         (- length) 
                                                         (count joltage)
                                                         1) 
                                                      batteries)))]
        (recur (conj joltage hv)
               (drop (inc hi) batteries))))))

(defn find-total-joltage [length batteries]
  (transduce (map (partial find-joltage length))
             +
             batteries))

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
  (find-total-joltage 2 input))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (find-total-joltage 12 input))
