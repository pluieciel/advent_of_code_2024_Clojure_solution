;advent-of-code-2024.day-12
(ns day12
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn mycmp [[a1 b1] [a2 b2]]
  (let [cmp-a (compare a1 a2)]
    (if (zero? cmp-a)
      (compare b1 b2)
      cmp-a)))

(let [Map (->> (slurp "exa") str/split-lines (mapv vec))
      ;plants (->> Map flatten set)
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
      flood (fn f [p todo area] ;\R #{[0 0]} #{[1 0]}
              (if (empty? todo)
                [p area]
                (let [doing (first todo)]
                  (swap! flags conj doing)
                  (f p
                     (->> doing
                          (same-neighbours p)
                          (filter #(not (contains? @flags %)))
                          (reduce conj (disj todo doing)))
                     (conj area doing)))))
      cal-lr (fn [area]
               (->> area
                    (map #(let [[y x] %] {y #{x}}))
                    (apply merge-with set/union)
                    (sort-by first)
                    (map (fn [[y xs]] [y (->> (sort xs)
                                              (#(let [l %]
                                                  (->> (concat [-2] l [-2])
                                                       (partition 2 1)
                                                       (remove (fn [v] (let [[a b] v] (= b (inc a))))))))
                                              ((fn [l]
                                                 [(->> l first (remove neg?)) (->> l second (remove neg?))]))
                                              )]))
                    ))
      ]
  (->> (for [y (range (count Map))
             x (range (count (first Map)))
             :when (not (contains? @flags [y x]))]
         (flood (get-in Map [y x]) #{[y x]} #{}))
       first
       second
       cal-lr

       (reduce +)
       println))

(let [Map (->> (slurp "input") str/split-lines (mapv vec))
      ;plants (->> Map flatten set)
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
                [p (count area) (reduce + perim)]
                (let [doing (first todo)]
                  (swap! flags conj doing)
                  (f p
                     (->> doing
                          (same-neighbours p)
                          (filter #(not (contains? @flags %)))
                          (reduce conj (disj todo doing))) 
                     (conj area doing) 
                     (conj perim (pos->parim doing))))))
      ]
  (->> (for [y (range (count Map))
             x (range (count (first Map)))
             :when (not (contains? @flags [y x]))]
         (flood (get-in Map [y x]) #{[y x]} #{} [])) 
       (map #(let [[p area perim] %] (* area perim)))
       (reduce +)
       println))