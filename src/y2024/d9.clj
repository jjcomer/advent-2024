(ns y2024.d9
  (:require [clojure.string :as str]))

;; PROBLEM LINK https://adventofcode.com/2024/day/9
;; Generator Logic

(defn parse-input [input]
  (let [xs (into [] (comp (map str)
                          (map parse-long))
                 (str (str/trim input) "0"))
        [files spaces] (->> xs
                            (map (fn [id pos len]
                                   {:id (/ id 2)
                                    :position pos
                                    :length len})
                                 (range)
                                 (reductions + 0 xs))
                            (partition 2)
                            (apply map vector))]
    {:files files
     :spaces spaces}))

;; Solution Logic

(defn defrag-drive [drive]
  (loop [files (:files drive)
         spaces (:spaces drive)
         drive []]
    (cond
      (or (empty? files)
          (> (:position (first spaces)) (:position (peek files))))
      (concat drive files)

      (zero? (:length (first spaces)))
      (recur files (rest spaces) drive)

      (zero? (:length (peek files)))
      (recur (pop files) spaces drive)

      :else
      (let [{space-id :id space-pos :position space-len :length} (first spaces)
            {file-id :id file-pos :position file-len :length} (peek files)
            space-needed (min space-len file-len)]
        (recur (conj (pop files) {:id file-id
                                  :length (- file-len space-needed)
                                  :position file-pos})
               (cons {:id space-id
                      :length (- space-len space-needed)
                      :position (+ space-pos space-needed)}
                     (rest spaces))
               (conj drive {:id file-id
                            :length space-needed
                            :position space-pos}))))))

(defn sum [a b]
  (/ (* (inc (- b a)) (+ a b)) 2))

(defn check-sum [drive]
  (transduce (map #(* (:id %) 
                      (sum (:position %) 
                           (+ (:position %) (:length %) -1))))
             +
             drive))

(defn sort-spaces [spaces]
  (->> spaces
       (group-by :length)
       (reduce (fn [acc [len spaces]]
                 (assoc acc len (into (sorted-set-by #(compare (:position %1) 
                                                               (:position %2))) 
                                      spaces)))
               {})))

(defn defrag-drive-2 [drive]
  (loop [files (:files drive)
         spaces (sort-spaces (:spaces drive))
         drive []]
    (if-let [file (peek files)]
      (if-let [possible-lengths (seq (filter #(and (seq (spaces %))
                                                  (< (:position (first (spaces %)))
                                                     (:position file)))
                                            (range (:length file) 10)))]
        (let [best-len (apply min-key (comp :position first spaces) possible-lengths)
              {:keys [position length id] :as space} (first (spaces best-len))]
          (recur (pop files)
                 (-> spaces
                     (update length #(disj % space))
                     (update (- length (:length file))
                             #(conj % {:position (+ position (:length file))
                                       :length (- length (:length file))})))
                 (conj drive (assoc file :position position))))
        (recur (pop files) spaces (conj drive file)))
      drive)))

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (parse-input input))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
  (-> input
      defrag-drive
      check-sum))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (-> input
      defrag-drive-2
      check-sum))
