;advent-of-code-2024.day-07
(let [ops [* +]
      ok? (fn [[f & r] ops]
            (some #(= f %)
              (loop [res [(first r)] l (rest r)]
                (if (empty? l)
                  res
                  (recur
                   (mapcat #(map (fn [op] (op % (first l))) ops) (filter #(<= % f) res))
                   (rest l))))))
      ops2 [* + (fn [a b] (read-string (str a b)))]
      parsed (->> (slurp "input")
               clojure.string/split-lines
               (map #(map read-string (clojure.string/split % #": | "))))
  ]
  ;part 1
  (->> parsed
    (filter #(ok? % ops))
    (map first)
    (reduce +)
    println)
  ;part 2
  (->> parsed
    (filter #(ok? % ops2))
    (map first)
    (reduce +)
    println))
