(ns y2024.d15
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2024/day/15

(def empty-queue clojure.lang.PersistentQueue/EMPTY)

(defn print-grid [grid robot]
  (let [grid (assoc-in grid robot \@)]
    (doseq [row grid]
      (println (apply str row)))))

;; Generator Logic

(defn parse-grid [grid] 
  (let [grid (into [] (map #(into [] %)) (str/split-lines grid)) 
        start-position (first (for [y (range (count grid))
                                    x (range (count (grid 0)))
                                    :when (= \@ (get-in grid [y x]))]
                                [y x]))] 
    [(assoc-in grid start-position \.)
     start-position]))

(defn parse-directions [directions]
  (into []
        (keep #(case %
                 \^ [-1 0]
                 \> [0 1]
                 \v [1 0]
                 \< [0 -1]
                 nil))
        directions))

;; Solution Logic

(defn addv [v1 v2]
  (mapv + v1 v2))

(defn move-p1 [grid start-position moves]
  (loop [grid grid
         robot start-position
         moves moves]
    (if-let [move (first moves)]
      (let [dest (addv robot move)]
        (cond
          (= \. (get-in grid dest))
          (recur grid dest (rest moves))

          (= \O (get-in grid dest))
          (let [temp (loop [temp (addv dest move)]
                       (if (= \O (get-in grid temp))
                         (recur (addv temp move))
                         temp))]
            (if (= \. (get-in grid temp))
              (recur (-> grid
                         (assoc-in dest \.)
                         (assoc-in temp \O))
                     dest
                     (rest moves))
              (recur grid robot (rest moves))))

          :else
          (recur grid robot (rest moves))))
      grid)))

(defn compute-gps [grid symbol]
  (let [boxes (for [y (range (count grid))
                    x (range (count (grid 0)))
                    :when (= symbol (get-in grid [y x]))]
                [y x])]
    (transduce (map (fn [[y x]] (+ x (* 100 y))))
               +
               boxes)))

(defn move-p2 [grid starting-position moves]
  (loop [grid grid
         robot starting-position
         moves moves] 
    (if-let [move (first moves)]
      (let [dest (addv robot move)] 
        (cond
          (= \. (get-in grid dest))
          (recur grid dest (rest moves))

          (#{\[ \]} (get-in grid dest))
          (let [boxes (loop [to-check (conj empty-queue dest)
                             boxes #{dest}]
                        (if-let [current (first to-check)]
                          (let [neighbours (if (= \[ (get-in grid current))
                                             [(addv current [0 1])
                                              (addv current move)]
                                             [(addv current [0 -1])
                                              (addv current move)])
                                neighbours (->> neighbours
                                                (remove boxes)
                                                (filter #(#{\[ \]} (get-in grid %))))]
                            (recur (into (rest to-check) neighbours)
                                   (into boxes neighbours)))
                          boxes))
                spaces (loop [to-check boxes
                              spaces #{}]
                         (if-let [box (first to-check)]
                           (let [space (addv box move)]
                             (if (not (boxes space))
                               (if (= \# (get-in grid space))
                                 (conj spaces space)
                                 (recur (rest to-check) (conj spaces space)))
                               (recur (rest to-check) spaces)))
                           spaces))]
            (if (every? #(= \. (get-in grid %)) spaces)
              (let [boxes-to-push (sort-by 
                                   (if (= 0 (first move))
                                     (fn [box]
                                       (abs (- (second box) (second dest))))
                                     (fn [box]
                                       (abs (- (first box) (first dest)))))
                                   boxes) 
                    new-grid (loop [grid grid
                                    boxes (reverse boxes-to-push)]
                               (if-let [box (first boxes)]
                                 (let [space (addv box move)]
                                   (recur (-> grid
                                              (assoc-in space (get-in grid box))
                                              (assoc-in box (get-in grid space)))
                                          (rest boxes)))
                                 grid))]
                (recur new-grid dest (rest moves)))
              (recur grid robot (rest moves))))

          :else
          (recur grid robot (rest moves))))
      grid)))
;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (let [[raw-grid directions] (str/split input #"\n\n")
        [grid start-position] (parse-grid raw-grid)]
    [raw-grid grid start-position (parse-directions directions)]))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [[_ grid start-position directions]]
  (-> grid
      (move-p1 start-position directions)
      (compute-gps \O)))

(defn expand-grid [grid]
  (-> grid
      (str/replace "O" "[]")
      (str/replace "." "..")
      (str/replace "#" "##")
      (str/replace "@" "@.")
      parse-grid))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [[raw-grid _ _ directions]]
  (let [[grid start-position] (expand-grid raw-grid)]
    (-> grid
        (move-p2 start-position directions)
        (compute-gps \[))))
