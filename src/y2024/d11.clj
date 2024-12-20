(ns y2024.d11
  (:require [clojure.edn :as edn]))

;; PROBLEM LINK https://adventofcode.com/2024/day/11

;; Generator Logic

;; Solution Logic

(defn count-digits [n]
  (if (zero? n)
    1
    (inc (int (Math/log10 n)))))

(defn split-number [n]
  (let [digits (str n)
        len (count digits)
        half (/ len 2)
        first-half (subs digits 0 half)
        second-half (subs digits half len)]
    [(parse-long first-half) (parse-long second-half)]))

(defn evolve [stone]
  (cond
    (zero? stone) [1]
    (even? (count-digits stone)) (split-number stone)
    :else [(* 2024 stone)]))

(let [cache (atom {})]
  (defn blink [stone blinks]
    (if-let [stones (get @cache [stone blinks])] 
      stones
      (if (zero? blinks)
        1
        (let [stones (evolve stone)
              stone-count (transduce (map #(blink % (dec blinks)))
                                     +
                                     stones)]
          (swap! cache assoc [stone blinks] stone-count)
          stone-count)))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (edn/read-string (str "[" input "]")))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [stones]
  (transduce (map #(blink % 25))
             +
             stones))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [stones]
  (transduce (map #(blink % 75))
             +
             stones))
