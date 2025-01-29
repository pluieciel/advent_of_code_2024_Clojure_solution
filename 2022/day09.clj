;advent-of-code-2022.day09
(ns day09
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map #(re-seq #"\d+|\w" %))))

(def dirs {"R" [0 1] "L" [0 -1] "U" [-1 0] "D" [1 0]})
(defn clip [n low up]
  (max low (min n up)))
;part 1
(let [data (parse "./2022/in09")
      head (atom [0 0])
      tail (atom [0 0])
      visited (atom #{})]
  (doseq [[dir step] data]
    (let [step (read-string step)]
      (dotimes [n step]
        (swap! head #(mapv + % (dirs dir)))
        (let [[dy dx] (map - @head @tail)]
          (cond
            (= dy 2) (swap! tail #(mapv + % [1 dx]))
            (= dy -2) (swap! tail #(mapv + % [-1 dx]))
            (= dx 2) (swap! tail #(mapv + % [dy 1]))
            (= dx -2) (swap! tail #(mapv + % [dy -1])))
          (swap! visited conj @tail)))))
  (count @visited))
;part 2
(let [data (parse "./2022/in09")
      heads (vec (repeatedly 10 #(atom [0 0])))
      visited (atom #{})]
  (doseq [[dir step] data]
    (let [step (read-string step)]
      (dotimes [n step]
        (swap! (heads 0) #(mapv + % (dirs dir)))
        (dotimes [idx 9]
          (let [[dy dx] (map - @(heads idx) @(heads (inc idx)))]
            (cond
              (= dy 2) (swap! (heads (inc idx)) #(mapv + % [1 (clip dx -1 1)]))
              (= dy -2) (swap! (heads (inc idx)) #(mapv + % [-1 (clip dx -1 1)]))
              (= dx 2) (swap! (heads (inc idx)) #(mapv + % [(clip dy -1 1) 1]))
              (= dx -2) (swap! (heads (inc idx)) #(mapv + % [(clip dy -1 1) -1])))))
        (swap! visited conj @(heads 9)))))
  (count @visited))