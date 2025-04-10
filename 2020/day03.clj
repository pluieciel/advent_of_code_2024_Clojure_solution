;advent-of-code-2020.day03
(ns day03
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (mapv vec)))

(def Map (parse "2020/in03"))
(def h (count Map))
(def w (count (first Map)))

(defn cal [x y]
  (loop [r 0 c 0 trees 0]
    (if (>= r h)
      trees
      (let [nr (+ r y)
            nc (mod (+ c x) w)
            t? (if (= \# (get-in Map [nr nc])) 1 0)]
        (recur nr nc (+ trees t?))))))

;part 1
(cal 3 1)
;part 2
(->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
     (map #(apply cal %))
     (apply *))