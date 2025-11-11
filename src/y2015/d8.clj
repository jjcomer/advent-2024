(ns y2015.d8
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2015/day/8

(def t "\"\"\n\"abc\"")

;; Generator Logic

;; Solution Logic

(defn count-length [s length]
  (if (empty? s)
    length
    (let [[head & tail] s]
      (case head
        \" (recur tail length)
        \\ (case (first tail)
             \x (recur (drop 3 tail) (inc length))
             (recur (rest tail) (inc length)))
        (recur tail (inc length))))))

(defn count-length2 [s length]
  (if (empty? s)
    (+ 2 length)
    (let [[head & tail] s]
      (case head
        (\\ \") (recur tail (+ length 2))
        (recur tail (inc length))))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       str/split-lines
       (map seq)
       (map (partial into []))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (let [raw-count (transduce (map count) + input)
        char-count (transduce (map #(count-length % 0)) + input)]
    (- raw-count char-count)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (let [raw-count (transduce (map count) + input)
        expanded-count (transduce (map #(count-length2 % 0)) + input)]
    (- expanded-count raw-count)))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
