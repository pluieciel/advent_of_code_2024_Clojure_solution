;advent-of-code-2020.day20
(ns day20
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (str/split (slurp i) #"\n\n")
       (map (fn [tile]
              (->> tile
                   str/split-lines
                   ((fn [[no & lines]]
                      [(read-string (re-find #"\d+" no)) (mapv vec lines)])))))
       (into {})))

(def data (parse "2020/in20"))
(def f-r-data (atom data))

(defn get-bandaries [tile]
  (let [trans (apply map vector tile)]
    [(first tile) (last tile) (first trans) (last trans)]))

(let [bandaries (map (fn [[k v]] [k (get-bandaries v)]) data)
      matching (for [[k bs] bandaries]
                 [k (for [b bs]
                      (conj (apply concat
                                   (for [[k2 bs2] bandaries :when (not= k2 k)]
                                     (for [[i b2] (map-indexed (fn [i v] [i v]) bs2)
                                           :let [nor (= b b2) rev (= b (reverse b2))]
                                           :when (or nor rev)]
                                       [k2 i nor]))) b))])]
  
  ;part 1
  (->> matching
       (filter (fn [[k v]] (= 2 (count (filter (fn [one] (= 2 (count one))) v)))))
       (map first)
       (reduce *)))