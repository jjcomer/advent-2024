(ns y2024.d23
  (:require [clojure.string :as str]
            [clojure.set :as set]))

;; PROBLEM LINK https://adventofcode.com/2024/day/23

;; Solution Logic

(defn find-3-groups [graph]
  (into #{} (for [a (keys graph)
                  b (graph a)
                  c (graph a)
                  :when (and (= \t (first a))
                             ((graph b) c))]
              #{a b c})))

(defn find-group [graph]
  (first (for [a (keys graph)
               b (graph a)
               :let [edges (sequence (comp (remove #(= b %))
                                           (map #(conj (graph %) %)))
                                     (graph a))
                     clique (reduce set/intersection edges)]
               :when (= 13 (count clique))]
           (sort clique))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> input
       (str/split-lines)
       (reduce (fn [graph edge]
                 (let [[from to] (str/split edge #"-")]
                   (-> graph
                       (update from (fnil conj #{}) to)
                       (update to (fnil conj #{}) from))))
               {})))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [graph]
  (count (find-3-groups graph)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [graph]
  (->> graph find-group (str/join ",")))
