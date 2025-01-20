;advent-of-code-2023.day21
(ns day21
  (:require [clojure.string :as str]
            [clojure.data.priority-map :refer [priority-map]]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (mapv vec)))

(def dirs [[0 1] [0 -1] [1 0] [-1 0]])
;part 1
(let [Map (parse "./2023/in21")
      h (count Map) w (count (first Map))
      [start] (for [y (range h) x (range w) :when (= \S (get-in Map [y x]))]
                [y x])
      Map (atom (assoc-in Map start 0))
      cnt (atom 0)]
  (loop [todo (priority-map start 0)]
    (let [[pos step] (peek todo)
          nxttodo (pop todo)
          curr (get-in @Map pos)
          nstep (inc step)]
      (when (even? step) (swap! cnt inc))
      (swap! Map assoc-in pos step)
      (if (> step 64)
        @cnt
        (recur
         (->> (map #(mapv + pos %) dirs)
              (filter #(= \. (get-in @Map %)))
              (reduce #(assoc %1 %2 nstep) nxttodo)))))))

;part 2
(let [Map (parse "./2023/in21")
      h (count Map) w (count (first Map))
      [start] (for [y (range h) x (range w) :when (= \S (get-in Map [y x]))]
                [y x])
      Map (assoc-in Map start \.)
      ;Map (map (fn [row] (vec (apply concat (repeat 5 row)))) Map)
      ;Map (atom (vec (apply concat (repeat 5 Map))))
      Map (atom Map)
      cnt (atom 0)
      far (atom 0)]
  ;change start point and threshold for edge blocks
  ;then, do the calculation on paper
  (loop [todo (priority-map [130 65] 0)]
    (if (empty? todo)
      [@cnt @far]
      (let [[pos step] (peek todo)
            nxttodo (pop todo)
            curr (get-in @Map pos)
            nstep (inc step)]
       ;(println todo)
        (when (even? step) (swap! cnt inc))
        (swap! Map assoc-in pos step)
        (if (> step 130)
          @cnt
         ;@Map
          (recur
           (->> (map #(mapv + pos %) dirs)
                (filter #(= \. (get-in @Map %)))
                (reduce #(assoc %1 %2 nstep) nxttodo))))))))