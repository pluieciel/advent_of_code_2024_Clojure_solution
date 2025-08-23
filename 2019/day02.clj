;advent-of-code-2019.day02
(ns day02
  (:require [clojure.string :as str]))

(def input
  (->> (str/split (slurp "2019/in02") #",")
       (map read-string)
       (map vector (range))
       (into {})))

(defn cal [dict pos]
  (if (= 99 (get dict pos))
    dict
    (let [[op a b c] (map #(get dict (+ pos %)) [0 1 2 3])
          [a b] (map #(get dict %) [a b])] 
      (cal (assoc dict c ((if (= 1 op) + *) a b)) (+ 4 pos)))))

(defn simulate [n v]
  (-> input
    (assoc 1 n)
    (assoc 2 v)
    (cal 0)
    (get 0)))

;; part 1
(simulate 12 2)

;; part 2
(for [n (range 0 100)
      v (range 0 100)
      :when (= 19690720 (simulate n v))]
  (+ (* 100 n) v))