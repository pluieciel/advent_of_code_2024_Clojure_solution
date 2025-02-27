;advent-of-code-2022.day20
(ns day20
  (:require [clojure.string :as str]
            [clojure.data.finger-tree :as ft]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map read-string)))

(let [arr (parse "2022/ex20")
      len (count arr)
      i_arr (map-indexed (fn [idx v] [idx idx v]) arr)
      ]
  i_arr)


(g (f x))
(->> x f g)