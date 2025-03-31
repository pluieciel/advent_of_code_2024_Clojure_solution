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