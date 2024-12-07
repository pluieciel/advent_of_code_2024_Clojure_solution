;advent-of-code-2024.day-05
(ns day05
  (:require [clojure.string :as str] [clojure.set :as set]))

(let [[p1 p2] (->> (slurp "input")
                (#(str/split % #"\n\n"))
                (map str/split-lines))
      dict (->> p1
             (map #(map read-string (str/split % #"\|")))
             (map (fn [[pre post]] {post #{pre}}))
             (apply merge-with set/union))
      ok? (fn [input]
            (loop [[a & bs] input]
              (cond
                (empty? bs) true
                (empty? (set/intersection (set bs) (get dict a #{}))) (recur bs)
                :else false)))
      ]

  ;part 1
  (->> p2
    (map #(map read-string (str/split % #",")))
    (filter ok?)
    (map #(nth % (/ (count %) 2)))
    (reduce +)
    println)

  ;part 2
  (->> p2
    (map #(map read-string (str/split % #",")))
    (remove ok?)
    (map (fn [l]
       (let [s (set l)]
         (->> l
           (map #(vector % (count (set/intersection (disj s %) (get dict % #{})))))
           (sort-by second)))))
    (map #(first (nth % (/ (count %) 2))))
    (reduce +)
    println))
