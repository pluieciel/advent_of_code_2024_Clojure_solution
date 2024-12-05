;advent-of-code-2024.day-05

(require '[clojure.set])

(let [[p1 p2] (->> (slurp "input")
                (#(clojure.string/split % #"\n\n"))
                (map clojure.string/split-lines))
      dict (->> p1
             (map #(map read-string (clojure.string/split % #"\|")))
             (map (fn [[pre post]] {post #{pre}}))
             (apply merge-with clojure.set/union))
      ok? (fn [input]
            (loop [[a & bs] input]
              (cond
                (empty? bs) true
                (empty? (clojure.set/intersection (set bs) (get dict a #{}))) (recur bs)
                :else false)))
      ]

  ;part 1
  (->> p2
    (map #(map read-string (clojure.string/split % #",")))
    (filter ok?)
    (map #(nth % (/ (count %) 2)))
    (reduce +)
    println)

  ;part 2
  (->> p2
    (map #(map read-string (clojure.string/split % #",")))
    (remove ok?)
    (map (fn [l]
       (let [s (set l)]
         (->> l
           (map #(vector % (count (clojure.set/intersection (disj s %) (get dict % #{})))))
           (sort-by second)))))
    (map #(first (nth % (/ (count %) 2))))
    (reduce +)
    println))
