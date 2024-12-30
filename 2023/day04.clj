;advent-of-code-2023.day04
(ns day04
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [input]
  (->> (slurp input)
       str/split-lines))
;part 1
(->> (parse "./2023/in04")
     (map (fn [row]
            (->> row
                 (#(str/split % #": +"))
                 second
                 (#(str/split % #" \| +"))
                 (map #(->> (str/split % #" +") (map read-string) set))
                 (apply set/intersection)
                 count)))
     (filter pos?)
     (map #(Math/pow 2 (dec %)))
     (reduce +))
;part 2
(let [lines (parse "./2023/in04")
      cns (range 1 (inc (count lines)))
      dict (apply merge
                  (for [c cns]
                    {c (atom 1)}))]
  (->> lines
       (map (fn [row]
              (->> row
                   (#(str/split % #": +"))
                   second
                   (#(str/split % #" \| +"))
                   (map #(->> (str/split % #" +") (map read-string) set))
                   (apply set/intersection)
                   count)))
       (map vector cns)
       (#(doseq [[c n] %]
           (doseq [cc (range (inc c) (inc (+ c n)))]
             (swap! (get dict cc) (fn [num] (+ num @(get dict c))))))))
  (->> (for [[_ v] dict]
         @v)
       (reduce +)))