;advent-of-code-2021.day17
(ns day17
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i) (re-seq #"-?\d+") (map read-string)))

(let [[x1 x2 y1 y2] (parse "2021/in17")]
  (def x1 x1) (def x2 x2) (def y1 y1) (def y2 y2))

(defn hit [[x y]]
  (cond
    (or (< x x1) (> y y2)) 0
    (and (<= x1 x x2) (<= y1 y y2)) 1
    :else 2))

(defn cal [vx vy]
  (loop [pos [0 0] vx vx vy vy highest 0]
    (let [newpos (mapv + pos [vx vy])]
      (cond
        (and (zero? vx) (< (pos 0) x1)) nil
        (zero? (hit newpos)) (recur newpos (if (pos? vx) (dec vx) 0) (dec vy) (max highest (newpos 1)))
        (= 1 (hit newpos)) highest
        (= 2 (hit newpos)) nil))))

;brute force, with no brain...
;part 1
(->> (for [vx (range 21 100) vy (range 50 150) :let [c (cal vx vy)] :when c] c)
     (apply max))

;part 2
(->> (for [vx (range 1 263) vy (range -100 200) :let [c (cal vx vy)] :when c] c)
     count)