;advent-of-code-2024.day-08
(ns day08
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as comb]
            [clojure.set :as set]))

(let [Map (->> (slurp "input") str/split-lines (mapv #(vec %)))
      [h w] [(count Map) (count (first Map))]
      within? (fn [[y x]] (and (>= y 0) (< y h) (>= x 0) (< x w)))
      freqs (disj (set (flatten Map)) \.)
      fre->poss (fn [fre]
                  (for [y (range h)
                        x (range w)
                        :when (= fre (get-in Map [y x]))]
                    [y x]))
      poss->pairs #(comb/combinations % 2)
      pair->res2 (fn [[pos1 pos2]]
                   (let [res1 (mapv - (map #(* 2 %) pos1) pos2)
                         res2 (mapv - (map #(* 2 %) pos2) pos1)]
                     (set (filter within? [res1 res2]))))
      pair->ress (fn [[pos1 pos2]]
                   (let [diff1 (mapv - pos1 pos2)
                         diff2 (mapv - pos2 pos1)
                         get-dir (fn [diff pos]
                                   (->> (range)
                                        (map #(map (fn [n] (* % n)) diff))
                                        (map #(map + pos %))
                                        (take-while within?)
                                        set))]
                     (set/union (get-dir diff1 pos1) (get-dir diff2 pos2))))]
  ;part 1
  (->> freqs
       (map fre->poss)
       (map poss->pairs)
       (map #(apply set/union (map pair->res2 %)))
       (apply set/union)
       count
       println)
  
  ;part 2
  (->> freqs
       (map fre->poss)
       (map poss->pairs)
       (map #(apply set/union (map pair->ress %)))
       (apply set/union)
       count
       println))