(ns y2024.d24
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2024/day/24

;; Generator Logic

;; Solution Logic

(defn fill-wires [gates wires]
  (reduce (fn [wires [wire value]]
            (assoc wires wire value))
          (zipmap (keys gates) (repeat nil))
          wires))

(defn simulate-gates [gates wires]
  (loop [wires wires
         gates gates]
    (if (every? identity (vals wires))
      wires
      (recur (reduce (fn [wires [out [gate in1 in2]]]
                       (let [in1 (get wires in1)
                             in2 (get wires in2)
                             out-v (get wires out)]
                         (if (and in1 in2 (not out-v))
                           (assoc wires out (case gate
                                              "AND" (bit-and in1 in2)
                                              "OR" (bit-or in1 in2)
                                              "XOR" (bit-xor in1 in2)))
                           wires)))
                     wires
                     gates)
             (remove (fn [[out _ _]] (get wires out)) gates)))))

(defn sim-gates [gates wires]
  (let [solved-wires (simulate-gates gates (fill-wires gates wires))
        ans-keys (reverse (sort (filter #(str/starts-with? % "z") (keys solved-wires))))]
    (->> ans-keys
         (map #(get solved-wires %))
         (apply str)
         (#(Long/parseLong % 2)))))

;; Entry Points
(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [[raw-inputs raw-gates] (str/split input #"\n\n")
        inputs (into {} (map (fn [line]
                               (let [[name value] (str/split line #": ")]
                                 [name (if (= "0" value) 0 1)]))
                             (str/split-lines raw-inputs)))
        gates (into {} (map (fn [line]
                              (let [[in1 gate in2 out] (str/split line #" (-> )?")]
                                [out [gate in1 in2]]))
                            (str/split-lines raw-gates)))]
    [inputs gates]))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[inputs gates]]
  (sim-gates gates inputs))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[inputs gates]]
  "PEN AND PAPER")
