;advent-of-code-2019.day09
(ns day09
  (:require [clojure.string :as str]))

(def program
  (->> (str/split (slurp "2019/in09") #",")
       (map read-string)
       (map vector (range))
       (into {})))

(defn getmode [pos idx modes dict rbase]
  (case (get modes idx 0)
    0 (get dict (+ pos (inc idx)) 0)
    1 (+ pos (inc idx))
    2 (+ rbase (get dict (+ pos (inc idx)) 0))))

(defn calone [op dict pos modes rbase]
  (assoc dict
         (getmode pos 2 modes dict rbase)
         (op (get dict (getmode pos 0 modes dict rbase) 0)
             (get dict (getmode pos 1 modes dict rbase) 0))))

(defn calcmp [op dict pos modes rbase]
  (assoc dict
         (getmode pos 2 modes dict rbase)
         (if (op (get dict (getmode pos 0 modes dict rbase) 0)
                 (get dict (getmode pos 1 modes dict rbase) 0))
           1 0)))

(defn jump [dict pos modes rbase f]
  (let [arg (get dict (getmode pos 0 modes dict rbase) 0)]
    (if (f arg)
      (+ pos 3)
      (get dict (getmode pos 1 modes dict rbase) 0))))

(defn cal-wrapper [input]
  (fn cal [dict pos rbase]
    (if (= 99 (get dict pos))
      nil
      (let [op-long (get dict pos)
            [op & modes] (->> op-long
                              str
                              (re-seq #"\d\d$|\d")
                              reverse
                              (map Integer/parseInt))
            modes (vec modes)]
        (case op
          1 (cal (calone + dict pos modes rbase) (+ pos 4) rbase)
          2 (cal (calone * dict pos modes rbase) (+ pos 4) rbase)
          3 (cal (assoc dict (getmode pos 0 modes dict rbase) input) (+ pos 2) rbase)
          4 (do (println (get dict (getmode pos 0 modes dict rbase) 0)) (cal dict (+ pos 2) rbase))
          5 (cal dict (jump dict pos modes rbase zero?) rbase)
          6 (cal dict (jump dict pos modes rbase #(not (zero? %))) rbase)
          7 (cal (calcmp < dict pos modes rbase) (+ pos 4) rbase)
          8 (cal (calcmp = dict pos modes rbase) (+ pos 4) rbase)
          9 (cal dict (+ pos 2) (+ rbase (get dict (getmode pos 0 modes dict rbase) 0))))))))

;; part1
((cal-wrapper 1) program 0 0)

;; part2
((cal-wrapper 2) program 0 0)