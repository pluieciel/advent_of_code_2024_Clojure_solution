;advent-of-code-2024.day-18
(ns day18
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [input]
  (->> (slurp input)
       str/split-lines
       (map #(mapv read-string (re-seq #"\d+" %)))))

(defn dist [pos end]
  (->> (map #(Math/abs (- %1 %2)) pos end)
       (reduce +)))

(defn cal "A*" [start end Walls]
  (let [dirs [[0 1] [1 0] [0 -1] [-1 0]]
        w (first end)]
    (loop [todo [[start 0 (dist start end)]] done #{}]
      (if (empty? todo)
        false
        (let [[pos score _] (first todo)
              newposs (for [[x y :as npos] (map #(mapv + pos %) dirs)
                            :when (and (<= 0 x w)
                                       (<= 0 y w)
                                       (not (get done npos))
                                       (not (get Walls npos)))]
                        [npos (inc score) (+ (inc score) (dist npos end))])]
          (if (empty? (filter #(= end (first %)) newposs))
            (recur (sort-by last (set (concat (rest todo) newposs))) (conj done pos))
            (filter #(= end (% 0)) newposs)))))))

;part 1
(let [Walls (->> (parse "input") (take 1024) set)]
  (->> (cal [0 0] [70 70] Walls)))

;part 2
(let [input (parse "input")
      n 2900
      Walls (->> input (take n) set)]
  (loop [Walls Walls tocheck (drop n input) n n]
    ;(println n)
    (let [doing (first tocheck)
          nWalls (conj Walls doing)]
      (if (cal [0 0] [70 70] nWalls)
        (recur nWalls (rest tocheck) (inc n))
        doing))))