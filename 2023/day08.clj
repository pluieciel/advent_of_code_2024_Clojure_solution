;advent-of-code-2023.day08
(ns day08
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [i]
  (->> (slurp i)
       (#(str/split % #"\n\n"))
       (#(let [[cmds maps] %]
           [cmds
            (->> maps
                 str/split-lines
                 (map (fn [line]
                        (let [[curr dl dr] (re-seq #"\w+" line)]
                          {curr [dl dr]})))
                 (apply merge))]))))
;part 1
(let [[cmds maps] (parse "./2023/in08")]
  (loop [curr "AAA" todo (cycle (vec cmds)) cnt 0]
    (if (= curr "ZZZ")
      cnt
      (recur (-> (get maps curr) (get ({\L 0 \R 1} (first todo))))
             (rest todo)
             (inc cnt)))))

(let [[cmds maps] (parse "./2023/in08")
      starts (->> maps keys (filter #(= \A (last %))))]
  (defn getnext [cur cmd]
    (-> (get maps cur) (get ({\L 0 \R 1} cmd))))
  (loop [curr (nth starts 1) ;0 to 5 to get the loop of each start point
         todo (cycle (map-indexed #(do [%2 %1]) cmds))
         cnt 0
         done {}]
    (when (= \Z (last curr)) (println curr (second (first todo)) cnt))
    (if ;(= cnt 44398)
     (done [curr (second (first todo))])
      [[curr (second (first todo))] (done [curr (second (first todo))]) cnt]
      (recur (getnext curr (ffirst todo))
             (rest todo)
             (inc cnt)
             (assoc done [curr (second (first todo))] (second (first todo)))))))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))
;part 2
(reduce lcm [12083, 22199, 19951, 17141, 14893, 20513])