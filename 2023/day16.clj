;advent-of-code-2023.day16
(ns day16
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i) str/split-lines (mapv vec)))

(let [Map (parse "./2023/in16")
      h (count Map) w (count (first Map))]
  (defn cal [start]
    (->> (loop [done #{} todo [start]]
           (if (empty? todo)
             done
             (let [[pos [dy dx :as dir] :as posdir] (first todo)
                   r (rest todo)
                   curr (get-in Map pos)]
               (cond
                 (nil? curr)
                 (recur done r)
                 (or (= \. curr) (and (= \- curr) (zero? dy)) (and (= \| curr) (zero? dx)))
                 (let [newposdir [(map + pos dir) dir]]
                   (recur (conj done posdir) (if (done newposdir) r (conj r newposdir))))
                 (= \- curr)
                 (recur (conj done posdir) (apply conj r (remove done [[pos [0 -1]] [pos [0 1]]])))
                 (= \| curr)
                 (recur (conj done posdir) (apply conj r (remove done [[pos [-1 0]] [pos [1 0]]])))
                 (= \\ curr)
                 (recur (conj done posdir) (let [newposdir [(map + pos [dx dy]) [dx dy]]]
                                             (if (done newposdir) r (conj r newposdir))))
                 (= \/ curr)
                 (recur (conj done posdir) (let [newposdir [(map + pos [(- dx) (- dy)]) [(- dx) (- dy)]]]
                                             (if (done newposdir) r (conj r newposdir))))))))
         (map first)
         set
         count))
  ;part 1
  (cal [[0 0] [0 1]])
  ;part 2
  (max
   (->> (for [y (range h)]
          (cal [[y 0] [0 1]]))
        (apply max))
   (->> (for [y (range h)]
          (cal [[y (dec w)] [0 -1]]))
        (apply max))
   (->> (for [x (range w)]
          (cal [[0 x] [1 0]]))
        (apply max))
   (->> (for [x (range w)]
          (cal [[(dec h) x] [-1 0]]))
        (apply max))))