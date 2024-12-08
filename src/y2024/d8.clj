(ns y2024.d8
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.numeric-tower :as math]))

;; PROBLEM LINK https://adventofcode.com/2024/day/8

;; Generator Logic

;; Solution Logic

(defn find-towers [grid]
  (->> grid
       (filter #(not= \. (val %)))
       (group-by val)
       (map (fn [[k v]]
              [k (map key v)]))))

(defn find-antinodes [grid towers]
  (reduce (fn [antinodes [_ nodes]]
            (loop [nodes nodes
                   antinodes antinodes]
              (if-let [node-1 (first nodes)]
                (let [new-antinodes (into #{}
                                          (comp (mapcat #(let [dx (- (first %) (first node-1))
                                                               dy (- (second %) (second node-1))]
                                                           [[(+ (first %) dx) (+ (second %) dy)]
                                                            [(- (first node-1) dx) (- (second node-1) dy)]]))
                                                (filter #(contains? grid %)))
                                          (rest nodes))]
                  (recur (rest nodes) (set/union antinodes new-antinodes)))
                antinodes)))
          #{}
          towers))

(defn get-gcd [dx dy]
  (let [gcd (math/gcd dx dy)]
    [(quot dx gcd) (quot dy gcd)]))

(defn gen-antinodes [grid x y dx dy direction]
  (loop [x x y y antinodes [[x y]]]
    (let [[nx ny :as next-node] [(direction x dx) (direction y dy)]]
      (if (contains? grid next-node)
        (recur nx ny (conj antinodes next-node))
        antinodes))))

(defn find-antinodes-2 [grid towers]
  (reduce (fn [antinodes [_ nodes]]
            (loop [nodes nodes
                   antinodes antinodes]
              (if-let [node-1 (first nodes)]
                (let [new-antinodes (into #{}
                                          (comp (mapcat #(let [dx (- (first %) (first node-1))
                                                               dy (- (second %) (second node-1))
                                                               [gdx gdy] (get-gcd dx dy)
                                                               anti-1 (gen-antinodes grid (first node-1) (second node-1) gdx gdy +)
                                                               anti-2 (gen-antinodes grid (first node-1) (second node-1) gdx gdy -)]
                                                           (concat anti-1 anti-2))))
                                          (rest nodes))]
                  (recur (rest nodes) (set/union antinodes new-antinodes)))
                antinodes)))
          #{}
          towers))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (reduce (fn [grid [y row]]
            (reduce (fn [grid [x point]]
                      (assoc grid [x y] point))
                    grid
                    (map-indexed vector row)))
          {}
          (map-indexed vector (str/split-lines input))))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [grid]
  (let [towers (find-towers grid)]
    (count (find-antinodes grid towers))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [grid]
  (let [towers (find-towers grid)]
    (count (find-antinodes-2 grid towers))))
