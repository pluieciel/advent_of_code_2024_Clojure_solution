;advent-of-code-2023.day11
(ns day11
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as comb]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (mapv vec)))
;part 1
(let [Map (parse "./2023/in11")
      Map-trans (apply mapv vector Map)
      h (count Map) w (count (first Map))
      dict (->> (for [y (range h) x (range w) :when (= (get-in Map [y x]) \#)]
                  {[y x] (atom [y x])})
                (apply merge))]
  (doseq [y (range h) :when (every? #(= \. %) (get Map y))]
    (doseq [[[j _] v] dict :when (> j y)]
      (swap! v (fn [[j i]] [(inc j) i]))))
  (doseq [x (range w) :when (every? #(= \. %) (get Map-trans x))]
    (doseq [[[_ i] v] dict :when (> i x)]
      (swap! v (fn [[j i]] [j (inc i)]))))
  (->> (for [[_ v] dict] @v)
       (#(comb/combinations % 2))
       (map (fn [[[y1 x1] [y2 x2]]]
              (+ (Math/abs (- y1 y2)) (Math/abs (- x1 x2)))))
       (reduce +)))
;part 2
(let [Map (parse "./2023/in11")
      Map-trans (apply mapv vector Map)
      h (count Map) w (count (first Map))
      dict (->> (for [y (range h) x (range w) :when (= (get-in Map [y x]) \#)]
                  {[y x] (atom [y x])})
                (apply merge))]
  (doseq [y (range h) :when (every? #(= \. %) (get Map y))]
    (doseq [[[j _] v] dict :when (> j y)]
      (swap! v (fn [[j i]] [(+ j 1000000 -1) i]))))
  (doseq [x (range w) :when (every? #(= \. %) (get Map-trans x))]
    (doseq [[[_ i] v] dict :when (> i x)]
      (swap! v (fn [[j i]] [j (+ i 1000000 -1)]))))
  (->> (for [[_ v] dict] @v)
       (#(comb/combinations % 2))
       (map (fn [[[y1 x1] [y2 x2]]]
              (+ (Math/abs (- y1 y2)) (Math/abs (- x1 x2)))))
       (reduce +)))