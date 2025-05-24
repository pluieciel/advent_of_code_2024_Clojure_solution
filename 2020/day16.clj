;advent-of-code-2020.day16
(ns day16
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (str/split (slurp i) #"\n\n")))

;part 1
(let [[rules-str _ tks-str] (parse "2020/in16")
      rules (->> (re-seq #"\d+" rules-str)
                 (map read-string)
                 (partition 2))
      tks (->> (re-seq #"\d+" tks-str)
               (map read-string))]
  (->> tks
       (remove (fn [n] (some #(<= (first %) n (second %)) rules)))
       (reduce +)))

;part 2
(defn ok [nums [rule-idx rule]]
  (when (every? (fn [n] (some #(<= (first %) n (second %)) rule)) nums) rule-idx))

(let [[rules-str mytk-str tks-str] (parse "2020/in16")
      rules (->> (str/split-lines rules-str)
                 (map-indexed #(vector % (->> (re-seq #"\d+" %2)
                                              (map read-string)
                                              (partition 2)))))
      rules-raw (mapcat second rules)
      mytk (->> (str/split-lines mytk-str)
                last
                (#(str/split % #","))
                (map read-string))
      tks (->> (str/split-lines tks-str)
               rest
               (map #(->> (str/split % #",")
                          (map read-string))))
      cand-tks (filter (fn [nums]
                         (every?
                          (fn [n]
                            (some #(<= (first %) n (second %)) rules-raw))
                          nums))
                       tks)
      all (conj cand-tks mytk)
      all-t (apply map list all)
      poss (map (fn [nums] (keep #(ok nums %) rules)) all-t)
      poss-idx (map-indexed #(vector % %2) poss)
      poss-sorted (sort-by #(count (second %)) poss-idx)]
  (->> (conj poss-sorted [-1 '()])
       (partition 2 1)
       (map (fn [[[a la] [b lb]]]
              [b (remove (set la) lb)]))
       (sort-by #(first (second %)))
       (take 6)
       (map first)
       (map #(nth mytk %))
       (reduce *)))