;advent-of-code-2024.day-19
(ns day19
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (slurp input)
       (#(str/split % #"\n\n"))
       (#(let [[col design] %]
           [(str/split col #", ") (str/split-lines design)]))))

;part 1
(defn match [target col]
  (let [p (re-pattern (str "^" col))]
    (re-find p target)))

;"naive forward search"
(defn cando? [target cols]
  (loop [curr #{target}]
    (let [res (->> (for [t curr]
                     (for [col cols :when (match t col)]
                       (subs t (count (match t col)))))
                   flatten
                   set)]
      (cond
        (some empty? res) true
        (empty? res) false
        :else (recur res)))))

(let [[cols design] (parse "input")]
  (->> design
       (map #(cando? % cols))
       (filter true?)
       count
       println))

;part 2
(defn match-end [target col]
  (let [p (re-pattern (str col "$"))]
    (re-find p target)))

;"DP backward search"
(def cal
  (memoize
   (fn [target cols pos]
     (if (<= pos 0)
       (if (zero? pos) 1 0)
       (->> (for [col cols :when (match-end (subs target 0 pos) col)]
              (cal target cols (- pos (count col))))
            (reduce +))))))

(let [[cols design] (parse "input")]
  (->> design
       (map #(cal % cols (count %)))
       (reduce +)
       println))