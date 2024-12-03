;advent-of-code-2024.day-03
;part 1
(let [input (slurp "input")
      muls (re-seq #"mul\(\d{1,3},\d{1,3}\)" input)
      nums (map #(apply * (map read-string (re-seq #"\d+" %))) muls)]
  (println (reduce + nums)))

;part 2: regex
(let [patt #"(?s)((?<=^)|(?<=do\(\))).+?((?=$)|(?=don't\(\)))"
      patt-mul #"mul\(\d{1,3},\d{1,3}\)"
      parse-mul #(apply * (map read-string (re-seq #"\d+" %)))]
  (->> (slurp "input")
    (re-seq patt)
    (mapcat #(re-seq patt-mul (first %)))
    (map parse-mul)
    (reduce +)
    println))

;part 2: partition-by
(let [patt #"mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)"
      [d_flag do_n] [(atom true) (atom 0)]
      flag #(case %
              "do()" (do (reset! d_flag true) (swap! do_n inc) [@d_flag @do_n])
              "don't()" (do (reset! d_flag false) [@d_flag @do_n])
              [@d_flag @do_n])
      parse-mul (fn [mul] (apply * (map read-string (re-seq #"\d+" mul))))
      cal-do-list (fn [[_ & muls]] (reduce + (map parse-mul muls)))]
  (->> (slurp "input")
    (re-seq patt)
    (#(conj % "do()"))
    (partition-by flag)
    (filter #(= "do()" (first %)))
    (map cal-do-list)
    (reduce +)
    println))
