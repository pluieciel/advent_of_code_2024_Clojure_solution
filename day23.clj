;advent-of-code-2024.day-23
(ns day23
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [input]
  (->> (slurp input) str/split-lines (map #(re-seq #"\w\w" %))))

;part 1
(defn find-tri [pairs]
  (loop [[[a b] & r] pairs ctr 0]
    (if r
      (recur r (+ ctr (loop [r2 r ctr2 0]
                        (if (empty? r2)
                          ctr2
                          (let [[c d] (first r2)]
                            (cond
                              (and (= b c)
                                   (some #(= \t (first %)) [a b d])
                                   (some #{(list a d) (list d a)} r)) (recur (rest r2) (inc ctr2))
                              (and (= b d)
                                   (some #(= \t (first %)) [a b c])
                                   (some #{(list a c) (list c a)} r)) (recur (rest r2) (inc ctr2))
                              :else (recur (rest r2) ctr2)))))))
      ctr)))

(->> (parse "input")
     find-tri
     println)

;part 2
(defn find-conn [e-lst]
  (let [e-set (set e-lst)
        has-e? (fn [a b] (or (e-set (list a b)) (e-set (list b a))))]
    (loop [es e-lst res []]
      (if (empty? es)
        res
        (recur (rest es)
               (let [[a b] (first es)
                     ga (some #(when (% a) %) res)
                     gb (some #(when (% b) %) res)]
                 (cond
                   ga (if (every? #(has-e? b %) (disj ga a))
                        (conj (remove #(= ga %) res) (conj ga b))
                        res)
                   gb (if (every? #(has-e? a %) (disj gb b))
                        (conj (remove #(= gb %) res) (conj gb a))
                        res)
                   :else (conj res #{a b}))))))))

(let [e-lst (parse "input")]
  (->> e-lst
       find-conn
       (apply max-key count)
       sort
       (str/join ",")
       println))