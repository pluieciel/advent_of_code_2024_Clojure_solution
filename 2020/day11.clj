;advent-of-code-2020.day11
(ns day11
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (mapv vec)))

(def M (parse "2020/in11"))
(def w (count (first M)))
(def h (count M))
(def ngbs [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]])

(defn count-neighbors [M [x y]]
  (->> ngbs
       (keep #(#{\#} (get-in M (mapv + % [x y]))))
       count))

(defn solve [count-neighbors threshold]
 (loop [Map M]
  (let [newMap (vec
                (for [r (range h)]
                  (vec
                   (for [c (range w)
                         :let [center (get-in Map [r c])
                               n (count-neighbors Map [r c])]]
                     (cond
                       (and (= center \L) (zero? n)) \#
                       (and (= center \#) (>= n threshold)) \L
                       :else center)))))]
    (if (= Map newMap)
      (->> Map
           (map #(count (filter #{\#} %)))
           (reduce +))
      (recur newMap)))))

(defn count-neighbors2 [M [x y]]
  (->> ngbs
       (keep #(#{\#} (loop [[x y] [x y]]
                       (let [[nx ny] (mapv + % [x y])
                             res (get-in M [nx ny] \L)]
                         (if (#{\L \#} res)
                           res
                           (recur [nx ny]))))))
       count))

;part 1
(solve count-neighbors 4)

;part 2
(solve count-neighbors2 5)