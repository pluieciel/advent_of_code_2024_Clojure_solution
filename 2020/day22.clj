;advent-of-code-2020.day22
(ns day22
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (str/split (slurp i) #"\n\n")
       (map #(->> (re-seq #"(?m)^\d+$" %) (map read-string)))))

(def init (parse "2020/in22"))

;;part 1
(defn step [[[p1f & p1r] [p2f & p2r]]]
  (if (> p1f p2f)
    [(concat p1r [p1f p2f]) p2r]
    [p1r (concat p2r [p2f p1f])]))

(->> (iterate step init)
     (drop-while #(every? not-empty %))
     first
     (some identity)
     reverse
     (map * (rest (range)))
     (reduce +))

;;part 2
(defn game [seen [[p1f & p1r :as p1] [p2f & p2r :as p2] :as decks]]
  (cond
    (or (seen decks) (empty? p2)) [1 p1]
    (empty? p1) [2 p2]
    :else
    (game
     (conj seen decks)
     (if (or (and (>= (count p1r) p1f) (>= (count p2r) p2f)
                  (= 1 (first (game #{} [(take p1f p1r) (take p2f p2r)]))))
             (and (not (and (>= (count p1r) p1f) (>= (count p2r) p2f)))
                  (> p1f p2f)))
       [(concat p1r [p1f p2f]) p2r]
       [p1r (concat p2r [p2f p1f])]))))

(->> (game #{} init)
     second
     reverse
     (map * (rest (range)))
     (reduce +))