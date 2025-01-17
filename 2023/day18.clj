;advent-of-code-2023.day18
(ns day18
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i) str/split-lines (map #(str/split % #" "))))

(def dirs {"R" [0 1] "D" [1 0] "L" [0 -1] "U" [-1 0]})

(defn get-len [lst]
  (->> lst
       (map (fn [[a b]] (inc (- b a))))
       (reduce +)))
;part 1
(let [actions (parse "./2023/in18")
      start [0 0]
      path (->> (reduce (fn [done action]
                          (let [pos (last done)
                                [d step _] action
                                step (read-string step)
                                newpos (mapv + pos (map #(* % step) (get dirs d)))]
                            (conj done newpos)))
                        [start] actions)
                rest)
      rows (->> path
                (group-by first)
                (#(for [[r lst] %] [r (partition 2 (sort (map second lst)))]))
                (sort-by first))]
  (let [[[rn rowone] & nxtrows] rows]
    (loop [tot 0
           prern rn
           prerow rowone
           todo nxtrows]
      (if (empty? todo)
        tot
        (let [[nxtrn nxtrow] (first todo)
              block (* (- nxtrn prern) (get-len prerow))
              [newrow makeup] (loop [res []
                                     rowa prerow
                                     rowb nxtrow
                                     makeup 0]
                                (cond
                                  (empty? rowa) [(concat res rowb) makeup]
                                  (empty? rowb) [(concat res rowa) makeup]
                                  :else
                                  (let [[s e :as a] (first rowa)
                                        [sn en :as b] (first rowb)]
                                    (cond
                                      (< en s) (recur (conj res b) rowa (rest rowb) makeup)
                                      (= en s) (recur res (conj (rest rowa) [sn e]) (rest rowb) makeup)
                                      (and (= s sn) (< en e)) (recur res (conj (rest rowa) [en e]) (rest rowb) (+ makeup (- en sn)))
                                      (and (= s sn) (= en e)) (recur res (rest rowa) (rest rowb) (+ makeup (inc (- en sn))))
                                      (and (< s sn) (< en e)) (recur (conj res [s sn]) (conj (rest rowa) [en e]) (rest rowb) (+ makeup (dec (- en sn))))
                                      (= en e) (recur (conj res [s sn]) (rest rowa) (rest rowb) (+ makeup (- en sn)))
                                      (= e sn) (recur res (rest rowa) (conj (rest rowb) [s en]) makeup)
                                      (< e sn) (recur (conj res a) (rest rowa) rowb makeup)))))]
          (recur (+ tot block makeup)
                 nxtrn
                 newrow
                 (rest todo)))))))

;part 2
(let [actions (parse "./2023/in18")
      start [0 0]
      path (->> (reduce (fn [done action]
                          (let [pos (last done)
                                [_ _ color] action
                                step (read-string (str "0x" (subs color 2 7)))
                                dir (get (vec (vals dirs)) (read-string (subs color 7 8)))
                                newpos (mapv + pos (map #(* % step) dir))]
                            (conj done newpos)))
                        [start] actions)
                rest)
      rows (->> path
                (group-by first)
                (#(for [[r lst] %] [r (partition 2 (sort (map second lst)))]))
                (sort-by first))]
  (let [[[rn rowone] & nxtrows] rows]
    (loop [tot 0
           prern rn
           prerow rowone
           todo nxtrows]
      (if (empty? todo)
        tot
        (let [[nxtrn nxtrow] (first todo)
              block (* (- nxtrn prern) (get-len prerow))
              [newrow makeup] (loop [res []
                                     rowa prerow
                                     rowb nxtrow
                                     makeup 0]
                                (cond
                                  (empty? rowa) [(concat res rowb) makeup]
                                  (empty? rowb) [(concat res rowa) makeup]
                                  :else
                                  (let [[s e :as a] (first rowa)
                                        [sn en :as b] (first rowb)]
                                    (cond
                                      (< en s) (recur (conj res b) rowa (rest rowb) makeup)
                                      (= en s) (recur res (conj (rest rowa) [sn e]) (rest rowb) makeup)
                                      (and (= s sn) (< en e)) (recur res (conj (rest rowa) [en e]) (rest rowb) (+ makeup (- en sn)))
                                      (and (= s sn) (= en e)) (recur res (rest rowa) (rest rowb) (+ makeup (inc (- en sn))))
                                      (and (< s sn) (< en e)) (recur (conj res [s sn]) (conj (rest rowa) [en e]) (rest rowb) (+ makeup (dec (- en sn))))
                                      (= en e) (recur (conj res [s sn]) (rest rowa) (rest rowb) (+ makeup (- en sn)))
                                      (= e sn) (recur res (rest rowa) (conj (rest rowb) [s en]) makeup)
                                      (< e sn) (recur (conj res a) (rest rowa) rowb makeup)))))]
          (recur (+ tot block makeup)
                 nxtrn
                 newrow
                 (rest todo)))))))