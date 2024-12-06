(ns y2024.d5
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; PROBLEM LINK https://adventofcode.com/2024/day/5

;; Generator Logic

(defn parse-rules [rules]
  (->> rules
       str/split-lines
       (reduce (fn [rules rule]
                 (let [[a b] (str/split rule #"\|")]
                   (update rules (parse-long a) #(if % (conj % (parse-long b))
                                                     #{(parse-long b)}))))
               {})))

(defn parse-updates [updates]
  (->> updates
       str/split-lines
       (mapv (fn [update]
               (mapv parse-long (str/split update #","))))))

;; Solution Logic

(defn verify-update [rules update]
  (loop [front #{} back update]
    (if-let [head (first back)]
      (if-let [rule (rules head)]
        (if (seq (set/intersection front rule))
          false
          (recur (conj front head) (rest back)))
        (recur (conj front head) (rest back)))
      true)))

(defn get-middle [update]
  (let [index (if (even? (count update))
                (dec (quot (count update) 2))
                (quot (count update) 2))]
    (nth update index)))

(defn fix-update [rules update]
  (let [comparator (fn [a b]
                     (if-let [rule (get rules b)]
                       (if (rule a)
                         -1
                         0)
                       0))]
    (sort-by identity comparator update)))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [[raw-rules raw-updates] (str/split input #"\n\n")]
    [(parse-rules raw-rules)
     (parse-updates raw-updates)]))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[rules updates]]
  (transduce (comp (filter #(verify-update rules %))
                   (map get-middle))
             +
             updates))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[rules updates]]
  (transduce (comp (remove #(verify-update rules %))
                   (map #(fix-update rules %))
                   (map get-middle))
             +
             updates))
