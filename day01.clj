(ns advent-of-code-2024.day-01)
;part 1
(let [input (slurp "input")
      parse #(map read-string (clojure.string/split % #"   "))
      lines (map parse (clojure.string/split input #"\n"))
      [l1 l2] (map sort (apply map list lines))
      res (reduce + (map #(Math/abs (- %1 %2)) l1 l2))
      ]
  (println res))

;part 2
(let [input (slurp "input")
      parse #(map read-string (clojure.string/split % #"   "))
      lines (map parse (clojure.string/split input #"\n"))
      [l1 l2] (apply map list lines)
      fre (frequencies l2)
      res (reduce + (map #(* % (get fre % 0)) l1))
      ]
  (println res))
