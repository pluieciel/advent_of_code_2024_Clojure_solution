;advent-of-code-2023.day13
(ns day13
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       (#(str/split % #"\n\n"))
       (map (fn [one]
              (->> one str/split-lines (mapv vec))))))
;part 1
(defn cal [Map]
  (loop [n 1]
    (cond
      (= n (count Map)) 0
      (let [up (take n Map) down (drop n Map)]
        (->> (map = (reverse up) down)
             (every? true?))) n
      :else (recur (inc n)))))

(->> (parse "./2023/in13")
     (map #(let [m %
                 invm (apply map vector m)
                 v (cal invm)
                 h (cal m)]
             (+ v (* 100 h))))
     (reduce +))
;part 2
(defn newcal [Map]
  (loop [n 1]
    (cond
      (= n (count Map)) 0
      (let [up (take n Map) down (drop n Map)]
        (->> (map #(if (= %1 %2) nil (reduce + (map (fn [a b] (if (= a b) 0 1)) %1 %2)))
                   (reverse up) down)
             (keep identity)
             (= '(1)))) n
      :else (recur (inc n)))))

(->> (parse "./2023/in13")
     (map #(let [m %
                 invm (apply map vector m)
                 v (newcal invm)
                 h (newcal m)]
             (+ v (* 100 h))))
     (reduce +))