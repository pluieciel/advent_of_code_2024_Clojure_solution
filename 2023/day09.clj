;advent-of-code-2023.day09
(ns day09
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map #(map read-string (str/split % #" ")))))

(defn getnext [lst]
  (loop [l lst hold 0]
    (if (apply = l)
      (+ (first l) hold)
      (recur (map - (rest l) l) (+ hold (last l))))))
;part 1
(->> (parse "./2023/in09")
     (map getnext)
     (reduce +))

(defn getprev [lst]
  (loop [l lst hold '()]
    (if (apply = l)
      (reduce #(- %2 %1) (first l) hold)
      (recur (map - (rest l) l) (conj hold (first l))))))
;part 2
(->> (parse "./2023/in09")
     (map getprev)
     (reduce +))