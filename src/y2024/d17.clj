(ns y2024.d17
  (:require [clojure.string :as str]
            [clojure.math :as m]))

;; PROBLEM LINK https://adventofcode.com/2024/day/17

;; Generator Logic

;; Solution Logic

(defn get-value [operand a b c]
  (case operand
    (0 1 2 3) operand
    4 a
    5 b
    6 c))

(defn adv [a v]
  (quot a (long (m/pow 2 v))))

(defn run-program [{:keys [a b c program]}]
  (loop [output []
         a a
         b b
         c c
         pointer 0]
    (if (>= pointer (count program))
      output
      (let [opcode (program pointer)
            operand (get program (inc pointer) 0)]
        ;;(println output a b c pointer "opcode"opcode  operand)
        (case opcode
          0 (recur output
                   (adv a (get-value operand a b c))
                   b
                   c
                   (+ 2 pointer))
          1 (recur output
                   a
                   (bit-xor b operand)
                   c
                   (+ 2 pointer))
          2 (recur output
                   a
                   (mod (get-value operand a b c) 8)
                   c
                   (+ 2 pointer))
          3 (recur output
                   a
                   b
                   c
                   (if (zero? a)
                     (+ 2 pointer)
                     operand))
          4 (recur output
                   a
                   (bit-xor b c)
                   c
                   (+ 2 pointer))
          5 (recur (conj output (mod (get-value operand a b c) 8))
                   a
                   b
                   c
                   (+ 2 pointer))
          6 (recur output
                   a
                   (adv a (get-value operand a b c))
                   c
                   (+ 2 pointer))
          7 (recur output
                   a
                   b
                   (adv a (get-value operand a b c))
                   (+ 2 pointer)))))))


(defn find-the-a [program]
  (loop [digits (reverse program)
         target []
         solutions [0]]
    (if-let [next-digit (first digits)]
      (let [target (cons next-digit target)
            next-solutions (mapcat (fn [initial]
                                     (let [initial (bit-shift-left initial 3)]
                                       (keep (fn [a]
                                               (when (= target (run-program {:a (+ initial a) :b 0 :c 0 :program program}))
                                                 (+ initial a)))
                                             (range 0 8))))
                                   solutions)]
        (recur (rest digits) target next-solutions))
      (reduce min solutions))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [[a b c & program] (re-seq #"\d+" input)]
    {:a (parse-long a)
     :b (parse-long b)
     :c (parse-long c)
     :program (mapv parse-long program)}))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [prog]
  (str/join "," (run-program prog)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [{:keys [program]}]
  (find-the-a program))

