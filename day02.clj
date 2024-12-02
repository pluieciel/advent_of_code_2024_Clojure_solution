;part 1
(let [input (slurp "input")
      parse #(map read-string (clojure.string/split % #" "))
      lines (map parse (clojure.string/split input #"\n"))
      diff (map #(map - (butlast %) (rest %)) lines)
      ok? #(or (every? #{1 2 3} %) (every? #{-1 -2 -3} %))
      res (count (filter ok? diff))
      ]
  (println res))

;part 2
(let [input (slurp "input")
      parse #(map read-string (clojure.string/split % #" "))
      lines (map parse (clojure.string/split input #"\n"))
      dif #(map - (butlast %) (rest %))
      diff (map dif lines)
      ok? #(or (every? #{1 2 3} %) (every? #{-1 -2 -3} %))
      res (count (filter ok? diff))

      todo (remove #(ok? (dif %)) lines)
      drop-nth #(concat (take % %2) (drop (inc %) %2))
      ok2? #(some true?
                  (for [n (range (count %))]
                    (ok? (dif (drop-nth n %)))))
      res2 (count (filter ok2? todo))
      ]
  (println (+ res res2)))
