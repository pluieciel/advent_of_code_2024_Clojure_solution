;advent-of-code-2024.day-23
(ns day23
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as comb]))

(defn parse [input]
  (->> (slurp input) str/split-lines (map #(re-seq #"\w\w" %))))

(comment defn find-conn [e-lst]
  (let [e-set (set e-lst)
        has-e? (fn [[a b]] (or (e-set (list a b)) (e-set (list b a))))]
    (loop [es e-lst res []]
      (if (empty? es)
        res
        (recur (rest es)
               (let [[a b] (first es)
                     ga (some #(% a) res)
                     gb (some #(% b) res)]
                 (cond
                   ga (if )
                   gb
                   :else (conj res #{a b}))
                 ))))))

(let [e-lst (parse "input")]
  (some #{'("il" "wc")} e-lst))




(defn find-tri [pairs]
  (loop [[[a b] & r] pairs ctr 0]
    (if (nil? r)
      ctr
      (recur r
             (+ ctr
                (loop [r2 r ctr2 0]
                  (if (empty? r2)
                    ctr2
                    (let [[c d] (first r2)]
                      (cond
                        (and (= b c)
                             (some #(= \t (first %)) [a b d])
                             (some #{(list a d) (list d a)} r))
                        (recur (rest r2) (inc ctr2))
                        (and (= b d)
                             (some #(= \t (first %)) [a b c])
                             (some #{(list a c) (list c a)} r))
                        (recur (rest r2) (inc ctr2))
                        :else (recur (rest r2) ctr2))))))))))

(->> (parse "input")
     find-tri)
