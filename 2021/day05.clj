;advent-of-code-2021.day05
(ns day05
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (str/split-lines (slurp i))
       (map (fn [l] (map read-string (re-seq #"\d+" l))))))

(def data (parse "2021/in05"))

(->> data
     (filter (fn [[x1 y1 x2 y2]] (or (= x1 x2) (= y1 y2))))
     ((fn [all]
        (reduce
         (fn [acc [x1 y1 x2 y2]]
           (reduce
            conj
            acc
            (for [x (range (min x1 x2) (inc (max x1 x2)))
                  y (range (min y1 y2) (inc (max y1 y2)))]
              [x y])))
         []
         all)))
     frequencies
     (remove #(<= (second %) 1))
     count)