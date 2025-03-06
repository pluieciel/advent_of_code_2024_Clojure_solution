;advent-of-code-2021.day04
(ns day04
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [i]
  (->> (str/split (slurp i) #"\n\n")
       ((fn [[lst & Ms]]
          [(->> (str/split lst #",") (map read-string))
           (->> Ms
                (map (fn [M] (->> M
                                  str/split-lines
                                  (map (fn [l] (map read-string (re-seq #"\d+" l))))
                                  ((fn [table]
                                     (->> (apply map vector table)
                                          (into table)
                                          (map set))))))))]))))

(let [[lst Ms] (parse "2021/in04")
      mem (atom nil)
      newMs (atom (set Ms))]

  (defn check [done table]
    (filter #(empty? (set/difference % done)) table))

  ;part 1
  (loop [done (set (take 5 lst)) todo (drop 5 lst)]
    (let [res (for [table Ms :let [res (check done table)] :when (not-empty res)] [res table])]
      (if (empty? res)
        (do (reset! mem (first todo)) (recur (conj done (first todo)) (rest todo)))
        (->> res first second
             (apply set/union)
             (#(set/difference % done))
             (reduce +)
             (* @mem)))))

  ;part 2
  (loop [done (set (take 5 lst)) todo (drop 5 lst)]
    (let [res (for [table @newMs :let [res (check done table)] :when (not-empty res)] [res table])]
      (if (empty? res)
        (do (reset! mem (first todo)) (recur (conj done (first todo)) (rest todo)))
        (if (> (count @newMs) 1)
          (do (doseq [one res] (swap! newMs disj (second one)))
              (reset! mem (first todo))
              (recur (conj done (first todo)) (rest todo)))
          (->> res first second
             (apply set/union)
             (#(set/difference % done))
             (reduce +)
             (* @mem)))))))