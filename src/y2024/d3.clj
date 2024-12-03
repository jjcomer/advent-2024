(ns y2024.d3
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2024/day/3

;; Generator Logic

;; Solution Logic

(def mul-regex #"mul\(\d+,\d+\)")
(def p2-mul-regex #"mul\(\d+,\d+\)|don't\(\)|do\(\)")

(defn process-mul [raw-mul] 
  (let [[n1 n2] (str/split (subs raw-mul 4 (dec (count raw-mul))) #",")] 
    (* (parse-long n1) (parse-long n2))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  input)

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (transduce (map process-mul) + (re-seq mul-regex input)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (loop [result 0 cmds (re-seq p2-mul-regex input)]
    (if-let [next-cmd (first cmds)]
      (cond 
        (= "don't()" next-cmd)
        (recur result (rest (drop-while #(not= % "do()") (rest cmds)))) 
        (= "do()" next-cmd)
        (recur result (rest cmds))
        :else
        (recur (+ result (process-mul next-cmd)) (rest cmds)))
      result)))
