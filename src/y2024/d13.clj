(ns y2024.d13
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2024/day/13

;; Generator Logic

(defn parse-machine [machine]
  (let [[a_x a_y b_x b_y prize_x prize_y] (map parse-long (re-seq #"\d+" machine))]
    {:a [a_x a_y]
     :b [b_x b_y]
     :prize [prize_x prize_y]}))

;; Solution Logic

(defn validate [solution-x solution-y part2?] 
  (let [long-x (long solution-x)
        long-y (long solution-y)]
    (if (and (< (abs (- solution-x long-x))
                0.001)
             (< (abs (- solution-y long-y))
                0.001))
      (if part2? 
        (+ (* 3 long-x) long-y)
        (if (and (<= 0 long-x 100)
                 (<= 0 long-y 100)) 
          (+ (* 3 long-x) long-y)
          0))
      0)))

(defn solve-machine [[ax ay] [bx by] [px py] part2?]
  (let [d (float (- (* by ax) (* bx ay)))]
    (if (zero? d)
      0
      (let [solution-x (* (/ 1 d)
                          (- (* by px) (* bx py)))
            solution-y (* (/ 1 d)
                          (+ (* (- ay) px) (* ax py)))]
        (validate solution-x solution-y part2?)))))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (map parse-machine (str/split input #"\n\n")))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [machines]
  (transduce (map (fn [{:keys [a b prize]}]
                    (solve-machine a b prize false)))
             +
             machines))
(def extra-prize 10000000000000)

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [machines]
  (transduce (map (fn [{:keys [a b prize]}]
                    (solve-machine a b (map #(+ extra-prize %) prize) true)))
             +
             machines))
