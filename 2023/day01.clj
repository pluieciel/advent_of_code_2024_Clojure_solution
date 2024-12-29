;advent-of-code-2023.day01
(ns day01
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (slurp input) str/split-lines))

;part 1
(->> (parse "./2023/in01")
     (map #(map read-string (re-seq #"\d" %)))
     (transduce (map #(+ (* 10 (first %)) (last %))) +))

;part 2
(def words "one|two|three|four|five|six|seven|eight|nine")
(def dict (->> words
               (#(str/split % #"\|"))
               (#(map vector % (map str (range 1 10))))
               (into {})))
(defn revstr [s] (apply str (reverse s)))
(def dict2 (->> (revstr words)
               (#(str/split % #"\|"))
               (#(map vector % (map str (range 9 0 -1))))
               (into {})))
(->> (parse "./2023/in01")
     (map (fn [s]
            (+ (->> (str/replace s (re-pattern words) dict)
                    (re-seq #"\d")
                    first
                    read-string
                    (* 10))
               (->> (str/replace (revstr s) (re-pattern (revstr words)) dict2)
                    (re-seq #"\d")
                    first
                    read-string))))
     (reduce +))