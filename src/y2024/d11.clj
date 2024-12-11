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
    [(Integer/parseInt first-half) (Integer/parseInt second-half)]))

(defn evolve [stone]
  (cond
    (zero? stone) [1]
    (even? (count-digits stone)) (split-number stone)
    :else [(*' 2024 stone)]))

(defn blink [cache stone blinks]
  (if-let [stones (get @cache [stone blinks])] 
    stones
    (if (zero? blinks)
      1
      (let [stones (evolve stone)
                             stone-count (transduce (map #(blink cache % (dec blinks)))
                                                    +
                                                    stones)]
                         (swap! cache assoc [stone blinks] stone-count)
                         stone-count))))

(defn count-stones [stone blinks]
  (let [cache (atom {})]
    (blink cache stone blinks)))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (edn/read-string (str "[" input "]")))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [stones]
  (transduce (map #(count-stones % 25))
             +
             stones))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [stones]
  (transduce (map #(count-stones % 75))
             +
             stones))
