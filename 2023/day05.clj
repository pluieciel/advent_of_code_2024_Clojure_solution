;advent-of-code-2023.day05
(ns day05
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [input]
  (->> (slurp input)
       (#(str/split % #"\n\n"))))
;part 1
(let [[seeds & maps] (parse "./2023/in05")
      seeds (->> seeds (#(str/split % #" ")) rest (map read-string))
      maps (->> maps 
                (map (fn [one]
                       (->> (str/split one #"\n")
                            rest
                            (map (fn [line]
                                   (->> (str/split line #" ")
                                        (map read-string))))))))]
  
  (->> (for [seed seeds]
         (reduce
          (fn [curr ms]
            (let [target (filter #(let [[_ ss len] %] (<= ss curr (+ ss (dec len)))) ms)]
              (if (empty? target)
                curr
                (let [[ds ss _] (first target)]
                  (+ ds (- curr ss))))))
          seed maps))
       (apply min)))

;part 2
(let [[seeds & maps] (parse "./2023/in05")
      seeds (->> seeds (#(str/split % #" ")) rest (map read-string))
      maps (->> maps
                (map (fn [one]
                       (->> (str/split one #"\n")
                            rest
                            (map (fn [line]
                                   (->> (str/split line #" ")
                                        (map read-string))))))))
      seeds (->> seeds (partition 2) (map #(let [[s l] %] [s (+ s l)])) (sort-by first))]
  (defn step [seeds m]
    ;(println seeds m)
    (let [bps (->> m (mapcat #(let [[_ ss l] %] [ss (+ ss l)])) set sort)
          b-seeds (loop [res [] todo (sort-by first seeds) bps bps]
                    ;(println "*" res todo bps)
                    (if (or (empty? bps) (empty? todo))
                      (concat res todo)
                      (let [bp (first bps)
                            [s e :as ra] (first todo)]
                        (cond
                          (<= bp s) (recur res todo (rest bps))
                          (>= bp e) (recur (conj res ra) (rest todo) bps)
                          :else (recur (conj res [s bp]) (conj (rest todo) [bp e]) (rest bps))))))]
      ;(println b-seeds)
      (vec
       (for [seed b-seeds :let [[s e] seed
                                new (first (for [one m
                                                 :let [[ds ss l] one
                                                       se (+ ss l)]
                                                 :when (<= ss s e se)]
                                             [(+ ds (- s ss)) (+ ds (- e ss))]))]]
         (if new new seed)))))
  (->> (reduce #(step %1 %2) seeds maps)
       (sort-by first)
       ffirst))