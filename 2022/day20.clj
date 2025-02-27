;advent-of-code-2022.day20
(ns day20
  (:require [clojure.string :as str]
            [clojure.data.finger-tree :as ft]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map read-string)))

(let [arr (parse "2022/in20")
      len (count arr)
      i_arr (map-indexed (fn [idx v] [idx v]) arr)
      res (loop [doing 0 state_idx 0 state (vec i_arr)]
            ;(println state doing state_idx)
            (if (= doing len)
              state
              (let [[idx v] (get state state_idx)]
                (if (< idx doing)
                  (recur doing (inc state_idx) state)
                  (recur (inc doing)
                         state_idx
                         (let [newpos (mod (+ state_idx v) (dec len))
                               newl (concat (take state_idx state)
                                            (drop (inc state_idx) state))]
                           (vec
                            (concat (take newpos newl)
                                    (conj
                                     (drop newpos newl)
                                     [idx v])))))))))
      final (mapv second res)
      idx-z (first (for [i (range len) :when (zero? (get final i))] i))
      idx [1000 2000 3000]]
  (->> idx
       (map #(get final (mod (+ idx-z %) len)))
       (reduce +)))
