;advent-of-code-2023.day19
(ns day19
  (:require [clojure.string :as str]))

(def dict {"x" 0 "m" 1 "a" 2 "s" 3})

(defn parse [i]
  (->> (slurp i)
       (#(str/split % #"\n\n"))
       ((fn [[rules data]]
          [(->> rules
                str/split-lines)
           (->> data
                str/split-lines
                (map #(->> (re-seq #"\d+" %) (mapv read-string))))]))))
;part 1
(let [[rules data] (parse "./2023/in19")
      rules (->> rules
                 (map (fn [line]
                        (->> (re-seq #"[^{},]+" line)
                             ((fn [l]
                                (let [[k & cs] l
                                      default (last cs)
                                      cs (butlast cs)]
                                  {k (fn [m]
                                       (->> (mapv (fn [c]
                                                    (->> (re-seq #"^[xmax]|>|<|\d+|:|\w+" c)
                                                         ((fn [lst]
                                                            (let [[a op n _ dest] lst
                                                                  op (read-string op)
                                                                  n (read-string n)]
                                                              [((resolve op) (get m (get dict a)) n) dest]))))) cs)
                                            (#(conj % [true default]))
                                            (filter #(first %))
                                            first
                                            second))}))))))
                 (apply merge))]
  (->> data
       (map (fn [nums]
              (loop [curr "in"]
                (case curr
                  "A" (reduce + nums)
                  "R" 0
                  (recur ((get rules curr) nums))))))
       (reduce +)))

;part 2
(let [[rules _] (parse "./2023/in19")
      rules (->> rules
                 (map (fn [line]
                        (->> (re-seq #"[^{},]+" line)
                             ((fn [l]
                                (let [[k & cs] l
                                      default (last cs)
                                      cs (butlast cs)]
                                  {k (->> (mapv #(re-seq #"^[xmax]|>|<|\d+|\w+" %) cs)
                                          (#(conj % [:else default])))}))))))
                 (apply merge))
      start [[1 4001] [1 4001] [1 4001] [1 4001]]
      res (atom [])]
  (defn onestep [ticker nums] ;main recursion function
    (cond
      (= ticker "A") (swap! res conj nums)
      (= ticker "R") nil
      :else
      (let [r-lst (get rules ticker)]
        (loop [todo r-lst nums nums]
          (let [r (first todo)]
            (if (= (first r) :else)
              (onestep (second r) nums)
              (let [[key op num dest] r
                    key (get dict key)
                    num (read-string num)
                    [s e] (get nums key)]
                (case op
                  "<" (cond
                        (<= e num) (onestep dest nums)
                        (>= s num) (recur (rest todo) nums)
                        :else (let [fore [s num] post [num e]]
                                (onestep dest (assoc nums key fore))
                                (recur (rest todo) (assoc nums key post))))
                  ">" (cond
                        (> s num) (onestep dest nums)
                        (<= e (inc num)) (recur (rest todo) nums)
                        :else (let [fore [s (inc num)] post [(inc num) e]]
                                (onestep dest (assoc nums key post))
                                (recur (rest todo) (assoc nums key fore)))))
                )))))))
  (onestep "in" start)
  (->> @res
       (map (fn [one]
              (->> (map (fn [[s e]] (- e s)) one)
                   (reduce *))))
       (reduce +)))