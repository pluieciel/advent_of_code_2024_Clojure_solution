;advent-of-code-2022.day11
(ns day11
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (str/split (slurp i) #"\n\n")
       (mapv (fn [monkey]
              (->> monkey
                   str/split-lines
                   (#(let [[idx items ops test t f] %
                           idx (read-string (re-find #"\d+" idx))
                           items (atom (mapv read-string (re-seq #"\d+" items)))
                           ops-fn (let [op (->> (str (re-find #"\+|\*" ops) "'") read-string resolve)
                                        n (re-find #"\d+" ops)]
                                    (if n
                                      (fn [num] (op num (read-string n)))
                                      (fn [num] (op num num))))
                           div (read-string (re-find #"\d+" test))
                           m_t (read-string (re-find #"\d+" t))
                           m_f (read-string (re-find #"\d+" f))]
                       [idx items ops-fn div m_t m_f])))))))

(let [monkeys (parse "./2022/ex11")
      cnt (atom (vec (repeat (count monkeys) 0)))]
  (doseq [i (range 10000)]
    (when (zero? (mod i 10)) (println i))
    (doseq [[idx items ops-fn div m_t m_f] monkeys]
      (let [res (map (fn [item]
                       (->> (ops-fn item)
                            (#(vector % (if (zero? (mod % div)) m_t m_f))))) @items)]
        ;(println idx @items)
        (swap! cnt update idx #(+' % (count @items)))
        (doseq [[score dest] res]
          (swap! (get-in monkeys [dest 1]) conj score))
        (reset! items []))))
  (->> @cnt
       (sort)
       (take-last 2)
       (reduce *)))
;part 1
(let [monkeys (parse "./2022/in11")
      cnt (atom (vec (repeat (count monkeys) 0)))]
  (doseq [i (range 20)]
    (doseq [[idx items ops-fn div m_t m_f] monkeys]
      (let [res (map (fn [item]
                       (->> (ops-fn item)
                            (#(quot % 3))
                            (#(vector % (if (zero? (mod % div)) m_t m_f))))) @items)]
        ;(println idx @items)
        (swap! cnt update idx #(+ % (count @items)))
        (doseq [[score dest] res]
          (swap! (get-in monkeys [dest 1]) conj score))
        (reset! items []))))
  (->> @cnt
       (sort)
       (take-last 2)
       (reduce *)))