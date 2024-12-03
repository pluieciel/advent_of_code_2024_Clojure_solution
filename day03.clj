;part 1
(let [input (slurp "input")
      muls (re-seq #"mul\(\d{1,3},\d{1,3}\)" input)
      nums (map #(apply * (map read-string (re-seq #"\d+" %))) muls)]
  (println (reduce + nums)))

;part 2
(let [input (slurp "input")
      muls-dos-donts (re-seq #"mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)" input)
      d_flag (atom true)
      do_n (atom 0)
      flag #(case %
              "do()" (do (reset! d_flag true) (swap! do_n inc) [@d_flag @do_n])
              "don't()" (do (reset! d_flag false) [@d_flag @do_n])
              [@d_flag @do_n])
      muls-p (partition-by flag (conj muls-dos-donts "do()"))
      dos (filter #(= "do()" (first %)) muls-p)
      parse-mul (fn [mul] (apply * (map read-string (re-seq #"\d+" mul))))
      cal-do-list (fn [[d & muls]] (reduce + (map parse-mul muls)))
      res (reduce + (map cal-do-list dos))
      ]
  (println res))
