(ns y2024.d22
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2024/day/22

;; Generator Logic

;; Solution Logic

(defn mix [n1 n2]
  (bit-xor n1 n2))

(defn prune [n]
  (mod n 16777216))

(defn next-secret [n]
  (let [n (-> n (mix (* 64 n)) prune)
        n (-> n (mix (quot n 32)) prune)
        n (-> n (mix (* 2048 n)) prune)]
    n))

(defn gen-secrets [initial n]
  (reductions (fn [prev-secret _]
                (next-secret prev-secret))
              initial
              (range n)))

(defn gen-buyer-prices [prices]
  (let [prices (map #(mod % 10) prices)]
    (persistent!
     (reduce (fn [result [a b c d price]]
               (let [deltas [(- b a) (- c b) (- d c) (- price d)]]
                 (if (contains? result deltas)
                   result
                   (assoc! result deltas price))))
             (transient {})
             (partition 5 1 prices)))))

(defn find-best-delta [prices]
  (let [deltas (map gen-buyer-prices prices)
        best-deltas (apply merge-with + deltas)]
    (reduce max (vals best-deltas))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (mapv parse-long (str/split-lines input)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [secrets]
  (transduce (comp (map #(gen-secrets % 2000))
                   (map last))
             +
             secrets))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [secrets]
  (find-best-delta (map #(gen-secrets % 2000) secrets)))
