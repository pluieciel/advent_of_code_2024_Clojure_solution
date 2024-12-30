;advent-of-code-2023.day03
(ns day03
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (slurp input)
       str/split-lines
       (mapv (fn [s] (vec (map-indexed #(vector %2 %1) s))))))
;part 1
(let [Map (parse "./2023/in03")]
  (->> (for [y (range (count Map))]
         (let [row (get Map y)]
           (->> row
                (partition-by #(apply <= (map int [\0 (first %) \9])))
                (#(apply +
                         (for [num %
                               :let [[a ia] (first num) [b ib] (last num)]
                               :when (apply <= (map int [\0 a \9]))
                               :when (->> (for [xx (range (dec ia) (+ 2 ib)) yy (range (dec y) (+ 2 y))
                                                :when (not (and (<= ia xx ib) (= yy y)))]
                                            (first (get-in Map [yy xx] [\.])))
                                          (some (fn [m] (not= m \.))))]
                           (->> num
                                (map first)
                                (apply str)
                                read-string)))))))
       (reduce +)))
;part 2
(let [Map (parse "./2023/in03")]
  (->> (for [y (range (count Map))]
         (let [row (get Map y)]
           (->> row
                (partition-by #(apply <= (map int [\0 (first %) \9])))
                (#(for [num %
                        :let [[a ia] (first num) [b ib] (last num)]
                        :when (apply <= (map int [\0 a \9]))
                        :let [pos* (first
                                    (for [xx (range (dec ia) (+ 2 ib)) yy (range (dec y) (+ 2 y))
                                          :when (not (and (<= ia xx ib) (= yy y)))
                                          :let [m (first (get-in Map [yy xx] [\.]))]
                                          :when (= m \*)]
                                      [yy xx]))]
                        :when pos*]
                    [pos*
                     (->> num
                          (map first)
                          (apply str)
                          read-string)])))))
       (apply concat)
       (group-by first)
       (map (fn [[k v]]
              (if (> (count v) 1)
                (->> v
                     (map second)
                     (apply *))
                0)))
       (reduce +)))