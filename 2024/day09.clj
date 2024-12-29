;advent-of-code-2024.day-08
(ns day09)

(let [input (->> (slurp "input") vec (#(conj % \0)) (map #(- (int %) 48)))
      lst (->> input
               (partition 2)
               (map (fn [id [a b]] (concat (repeat a id) (repeat b nil))) (range))
               (apply concat))
      length (->> (remove nil? lst) count)
      res (loop [l lst rev (reverse (remove nil? lst)) res []]
            (if (= length (count res))
              res
              (let [id (first l)]
                (if id
                  (recur (rest l) rev (conj res id))
                  (recur (rest l) (rest rev) (conj res (first rev)))))))
      lst2 (->> input
               (partition 2)
               (mapv (fn [id [a b]] (vector (repeat a id) (repeat b nil))) (range)))
      maxid (count lst2)
      res2 (loop [n (dec maxid) res lst2]
             (if (zero? n)
               res
               (let [need (->> (get-in lst2 [n 0]) count)
                     target (filter #(and (and (ffirst %) (< (ffirst %) n)) (<= need (count (last %)))) res)]
                 (if (empty? target)
                   (recur (dec n) res)
                   (let [target-id (->> target ffirst first)
                         nil-length (->> (get res target-id) last count)]
                     (recur (dec n) 
                            (-> res
                                (assoc-in [n 0] (repeat need nil))
                                (assoc target-id (vec (concat (butlast (get res target-id))
                                                              [(get-in res [n 0])
                                                               (repeat (- nil-length need) nil)]))))))))))]
  ;part 1
  (->> res
       (map * (range))
       (reduce +)
       println)
  ;part 2
  (->> res2
       flatten
       (map #(* %1 (if %2 %2 0)) (range))
       (reduce +)
       println))