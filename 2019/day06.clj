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
       (reduce
        (fn [acc [p c]]
          (assoc acc c p))
        {})))

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
(let [path1 (cal "YOU" [])
      path2 (cal "SAN" [])]
  (->> (concat path1 path2)
       frequencies
       (filter #(= 1 (val %)))
       count
       (#(- % 2))))