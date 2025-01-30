;advent-of-code-2022.day10
(ns day10
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines))

;part 1
(let [ops (parse "./2022/in10")
      process (reduce (fn [acc line]
                        (let [[time x] (last acc)]
                          (if (= "noop" line)
                            (conj acc [(inc time) x])
                            (let [v (read-string (re-find #"-?\d+" line))]
                              (conj acc [(+ 2 time) (+ x v)])))))
                      [[0 1]]
                      ops)
      dict (into {} process)
      target (range 20 221 40)]
  
  (->> target
       (map (fn [n] (if-let [res (get dict (dec n))]
                      (* n res)
                      (* n (get dict (- n 2))))))
       (reduce +)))

;part 2
(let [ops (parse "./2022/in10")
      process (reduce (fn [acc line]
                        (let [[time x] (last acc)]
                          (if (= "noop" line)
                            (conj acc [(inc time) x])
                            (let [v (read-string (re-find #"-?\d+" line))]
                              (conj acc [(+ 2 time) (+ x v)])))))
                      [[0 1]]
                      ops)
      dict (into {} process)]
  (->> (range 240)
       (map (fn [pos]
              (if (zero? pos)
                "#"
                (let [x (get dict pos (get dict (dec pos)))]
                  (if (#{-1 0 1} (- (mod pos 40) x))
                    "#"
                    ".")))))
       (partition 40)
       (map #(apply str %))
       (#(doseq [line %]
           (println line)))))