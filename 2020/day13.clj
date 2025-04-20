;advent-of-code-2020.day13
(ns day13
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       ((fn [[t b]]
          [(read-string t)
           (map read-string (re-seq #"\d+|x" b))]))))

;part 1
(let [[t buses] (parse "2020/ex13")]
  (->> buses
       (filter number?)
       (map #(vector % (- % (mod t %))))
       (apply min-key second)
       (apply *)))

;part 2, CRT
(defn inverse [n m]
  (let [r (mod n m)]
    (loop [x 1]
      (if (= 1 (mod (* r x) m))
        x
        (recur (inc x))))))

(let [[_ buses] (parse "2020/in13")]
  (->> buses
       (map-indexed vector)
       (filter #(number? (second %)))
       (map (fn [[i b]] [(if (zero? i) i (- b i)) b]))
       ((fn [buses]
          (let [N (->> buses (map second) (reduce *))]
            (->> (map
                  (fn [[i b]]
                    (let [Ni (quot N b)
                          inv (inverse Ni b)]
                      (* i Ni inv)))
                  buses)
                 (reduce +)
                 (#(mod % N))))))))