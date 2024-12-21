(ns y2024.d21
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2024/day/21

;; Generator Logic

;; Solution Logic

(defn gen-map [raw-data]
  (reduce (fn [pad [col row]]
            (assoc pad (get-in raw-data [row col]) [col row]))
          {}
          (for [col (range (count (first raw-data)))
                row (range (count raw-data))] [col row])))

(def numpad
  (gen-map [[\7 \8 \9]
            [\4 \5 \6]
            [\1 \2 \3]
            [\space \0 \A]]))


(def dpad
  (gen-map [[\space \^ \A]
            [\< \v \>]]))

(defn gen-range [a b]
  (into #{} (if (< a b) (range a (inc b)) (range b (dec a) -1))))

(defn generate-moves [[px py :as position] [tx ty :as target] [gx gy]]
  (let [horizontal-move (if (< tx px) \< \>)
        vertical-move (if (< ty py) \^ \v)
        horizontal-distance (abs (- tx px))
        vertical-distance (abs (- ty py))]
    (cond
      (= position target)
      [[\A]]

      (= px tx)
      [(concat (repeat vertical-distance vertical-move) [\A])]

      (= py ty)
      [(concat (repeat horizontal-distance horizontal-move) [\A])]

      :else
      (filter identity
              [(when-not (or (and (= gx tx)
                                  ((gen-range py ty) gy))
                             (and (= gy py)
                                  ((gen-range tx px) gx)))
                 (concat (repeat horizontal-distance horizontal-move)
                         (repeat vertical-distance vertical-move)
                         [\A]))
               (when-not (or (and (= gx px)
                                  ((gen-range ty py) gy))
                             (and (= gy ty)
                                  ((gen-range px tx) gx)))
                 (concat (repeat vertical-distance vertical-move)
                         (repeat horizontal-distance horizontal-move)
                         [\A]))]))))

(defn generate-sequences [code keypad start]
  (if (empty? code)
    [[]]
    (let [position (get keypad start)
          next-position (get keypad (first code))
          gap (get keypad \space)]
      (for [options (generate-moves position next-position gap)
            sequence (generate-sequences (rest code) keypad (first code))]
        (apply conj [options] sequence)))))

(def find-shortest-path
  (memoize
   (fn [code proxies keypad]
     (let [sequences (generate-sequences code keypad \A)]
       (if (zero? proxies)
         (transduce (map #(transduce (map count) + %))
                    min
                    Long/MAX_VALUE
                    sequences)
         (transduce (map #(transduce (map (fn [subsequence]
                                            (find-shortest-path subsequence
                                                                (dec proxies)
                                                                dpad)))
                                     +
                                     %))
                    min
                    Long/MAX_VALUE
                    sequences))))))

(defn gen-score [code path-size]
  (* path-size (parse-long (apply str (butlast code)))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (map #(into [] %) (str/split-lines input)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [codes]
  (transduce (map #(gen-score % (find-shortest-path % 2 numpad)))
             +
             codes))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [codes]
  (transduce (map #(gen-score % (find-shortest-path % 25 numpad)))
             +
             codes))
