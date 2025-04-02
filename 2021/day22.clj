;advent-of-code-2021.day22
(ns day22
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map #(map read-string (re-seq #"on|off|-?\d+" %)))))

(def data (parse "2021/in22"))

;part 1
(loop [res #{} todo data]
  (if (empty? todo)
    (count res)
    (let [[op x1 x2 y1 y2 z1 z2] (first todo)
          lst (for [x (range (max x1 -50) (inc (min x2 50)))
                    y (range (max y1 -50) (inc (min y2 50)))
                    z (range (max z1 -50) (inc (min z2 50)))]
                [x y z])]
      (recur
       (if (= op 'on)
         (reduce conj res lst)
         (reduce disj res lst))
       (rest todo)))))

;part 2
(defn volume [x1 x2 y1 y2 z1 z2]
  (* (inc (- x2 x1))
     (inc (- y2 y1))
     (inc (- z2 z1))))

(defn intersect [[x1 x2 y1 y2 z1 z2] [x3 x4 y3 y4 z3 z4]]
  (let [x (max x1 x3) y (max y1 y3) z (max z1 z3)
        x' (min x2 x4) y' (min y2 y4) z' (min z2 z4)]
    (when (and (<= x x') (<= y y') (<= z z'))
      [x x' y y' z z'])))

(loop [res [[1 (rest (first data))]] todo (rest data)]
  (if (empty? todo)
    (reduce + (map #(* (first %) (apply volume (second %))) res))
    (let [[op x1 x2 y1 y2 z1 z2] (first todo)
          tosub [x1 x2 y1 y2 z1 z2]
          overlaps (keep (fn [[op1 ori1]]
                           (let [inter (intersect tosub ori1)]
                             (when inter [(if (= op1 1) -1 1) inter]))) res)
          overlaps (into res overlaps)
          new (if (= op 'on)
                (conj overlaps [1 tosub])
                overlaps)]
      (recur new (rest todo)))))