;advent-of-code-2021.day13
(ns day13
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (str/split (slurp i) #"\n\n")
       ((fn [[dots folds]]
          [(->> dots
                str/split-lines
                (map #(map read-string (str/split % #","))))
           (->> folds
                str/split-lines
                (map #(map read-string (re-seq #"x|y|\d+" %))))]))))

(defn dofold [dots folds]
  (reduce
   (fn [acc [axis n]]
     (if (= axis 'x)
       (->> acc (map (fn [[x y]] (if (> x n) [(- (* 2 n) x) y] [x y]))))
       (->> acc (map (fn [[x y]] (if (> y n) [x (- (* 2 n) y)] [x y]))))))
   dots folds))

;part 1
(let [[dots folds] (parse "2021/in13")
      res (set (dofold dots (take 1 folds)))]
  (count res))

;part 2
(let [[dots folds] (parse "2021/in13")
      res (set (dofold dots folds))
      w (apply max (map first res))
      h (apply max (map second res))]
  (doseq [y (range (inc h))]
    (doseq [x (range (inc w))]
      (print (if (res [x y]) "#" ".")))
    (println)))