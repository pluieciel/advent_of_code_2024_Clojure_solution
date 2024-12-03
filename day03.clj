;advent-of-code-2024.day-03
;part 1
(let [input (slurp "input")
      muls (re-seq #"mul\(\d{1,3},\d{1,3}\)" input)
      nums (map #(apply * (map read-string (re-seq #"\d+" %))) muls)]
  (println (reduce + nums)))

;part 2: regex
(let [patt #"(?s)(?:(?<=^)|(?<=do\(\))).+?(?:(?=$)|(?=don't\(\)))"
      patt-mul #"mul\(\d{1,3},\d{1,3}\)"
      parse-mul #(apply * (map read-string (re-seq #"\d+" %)))]
  (->> (slurp "input")
    (re-seq patt)
    (mapcat #(re-seq patt-mul %))
    (map parse-mul)
    (reduce +)
    println))

;part 2: partition-by
(let [patt #"mul\(\d{1,3},\d{1,3}\)|do\(\)|don't\(\)"
      d_flag (atom true)
      flag #(case %
              "do()" (do (reset! d_flag true) @d_flag)
              "don't()" (do (reset! d_flag false) @d_flag)
              @d_flag)
      do? #(= "do()" %)
      parse-mul (fn [mul] (apply * (map read-string (re-seq #"\d+" mul))))
      cal-muls (fn [muls] (reduce + (map parse-mul (remove do? muls))))]
  (->> (slurp "input")
    (re-seq patt)
    (partition-by flag)
    (remove #(= "don't()" (first %)))
    (map cal-muls)
    (reduce +)
    println))
