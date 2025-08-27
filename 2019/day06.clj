;advent-of-code-2019.day06
(ns day06
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map #(str/split % #"\)"))))

(def child->parent
  (->> (parse "2019/in06")
       (into {} (map (comp vec reverse)))))

(defn cal [node cnt]
  (if-let [p (child->parent node)]
    (cal p (if (number? cnt) (inc cnt) (conj cnt node)))
    cnt))

;; part1
(->> child->parent
     keys
     (map #(cal % 0))
     (reduce +))

;; part2
(->> (mapcat #(rest (cal % [])) ["YOU" "SAN"])
     frequencies
     (filter #(= 1 (val %)))
     count)