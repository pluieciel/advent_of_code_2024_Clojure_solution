;advent-of-code-2024.day-12
(ns day12
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(let [Map (->> (slurp "input") str/split-lines (mapv vec))
      get-neighbours (fn [pos]
                       (->> [[-1 0] [1 0] [0 -1] [0 1]]
                            (map #(->> (mapv + pos %)))))
      same-neighbours (fn [p pos]
                        (->> (get-neighbours pos)
                             (filter #(->> % (get-in Map) (= p)))))
      pos->parim (fn [pos]
                   (->> pos
                        (same-neighbours (get-in Map pos))
                        count
                        (- 4)))
      flags (atom #{})
      flood (fn f [p todo area perim] ;\R #{[0 0]} #{[1 0]} [2]
              (if (empty? todo)
                [p area (reduce + perim)]
                (let [doing (first todo)]
                  (swap! flags conj doing)
                  (f p
                     (->> doing
                          (same-neighbours p)
                          (filter #(not (contains? @flags %)))
                          (reduce conj (disj todo doing)))
                     (conj area doing)
                     (conj perim (pos->parim doing))))))
      cal (fn [area-h]
            (->> area-h
                 (apply merge-with set/union)
                 (sort-by first)
                 (map (fn [[y xs]] [y (->> (sort xs)
                                           (#(let [l %]
                                               (->> (concat [-2] l [-2])
                                                    (partition 2 1)
                                                    (remove (fn [v] (let [[a b] v] (= b (inc a))))))))
                                           ((fn [l]
                                              [(->> l (map first) (remove neg?))
                                               (->> l (map second) (remove neg?))])))]))
                 ((fn [lst]
                    (loop [todo lst res [0 0] pre [#{} #{}]]
                      (if (empty? todo)
                        res
                        (let [[_ [l r]] (first todo) [nl nr] res [pl pr] pre]
                          (recur
                           (rest todo)
                           [(+ nl (count (set/difference (set l) pl))) (+ nr (count (set/difference (set r) pr)))]
                           [(set l) (set r)]))))))))
      cal-both (fn [area]
                 (let [v (map #(let [[y x] %] {y #{x}}) area) h (map #(let [[y x] %] {x #{y}}) area)]
                   (concat (cal v) (cal h))))
      flooded (->> (for [y (range (count Map))
                         x (range (count (first Map)))
                         :when (not (contains? @flags [y x]))]
                     (flood (get-in Map [y x]) #{[y x]} #{} [])))]
  ;part 1
  (->> flooded
       (map #(let [[_ area perim] %] (* (count area) perim)))
       (reduce +)
       println)
  ;part 2
  (->> flooded
       (map #(let [[_ area _] %] (* (count area) (reduce + (cal-both area)))))
       (reduce +)
       println))