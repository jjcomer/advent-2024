(ns y2015.d11
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2015/day/11

;; Generator Logic

;; Solution Logic

(defn rotate-password [password]
  (-> (loop [new-password [] 
             password (rseq password)]
        (if-let [head (first password)]
          (if (= \z head)
            (recur (conj new-password \a) (rest password))
            (apply conj new-password (char (inc (int head))) (rest password)))
          new-password))
      reverse
      vec))

(defn check-password [password]
  (let [triple (->> password
                    (partition 3 1)
                    (filter (fn [[a b c]]
                              (= (int a) (dec (int b)) (- (int c) 2))))
                    first)
        doubles (->> password
                     (partition-by identity)
                     (filter #(= 2 (count %))))
        bad-letters (->> password
                         (filter #{\l \o \i})
                         first)]
    (and triple
         (not bad-letters)
         (>= (count doubles) 2))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (into [] (seq (str/trim input))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (loop [password input]
    (if (check-password password)
      (apply str password)
      (recur (rotate-password password)))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (->> input
       solve-part-1
       generator
       rotate-password 
       solve-part-1))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
