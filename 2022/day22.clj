;advent-of-code-2022.day22
(ns day22
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (str/split (slurp i) #"\n\n")
       ((fn [[M ops]]
          [(->> M
                str/split-lines
                (mapv vec))
           (re-seq #"\d+|\w" ops)]))))

;part 1
(let [[M ops] (parse "2022/in22")
      e-col (atom 0)
      range-row (mapv (fn [row]
                        (let [e (count row)
                              s (count (take-while #(= % \space) row))]
                          (when (> e @e-col) (reset! e-col e))
                          [s e])) M)
      range-col (let [range-row-idx (map-indexed (fn [i v] [i v]) range-row)]
                  (vec
                   (for [c (range @e-col)]
                     (->> (filter (fn [[r [s e]]] (<= s c (dec e))) range-row-idx)
                          ((fn [l] [(ffirst l) (inc (first (last l)))]))))))
      start [0 (ffirst range-row)]
      dirs [[0 1] [1 0] [0 -1] [-1 0]]
      get-next (fn [[r c] [dr dc]]
                 (cond
                   (zero? dr) (let [[s e] (get range-row r)
                                    newc (+ c dc)]
                                (cond
                                  (< newc s) [r (dec e)]
                                  (= newc e) [r s]
                                  :else [r newc]))
                   :else (let [[s e] (get range-col c)
                                    newr (+ r dr)]
                                (cond
                                  (< newr s) [(dec e) c]
                                  (= newr e) [s c]
                                  :else [newr c]))))]
  (->>
   (loop [pos start dir-i 0 todo ops]
     (if (empty? todo)
       (conj (mapv inc pos) dir-i)
       (let [doing (first todo)]
         (case doing
           "R" (recur pos (mod (inc dir-i) 4) (rest todo))
           "L" (recur pos (mod (dec dir-i) 4) (rest todo))
           (let [step (read-string doing)
                 dir (get dirs dir-i)]
             (recur
              (loop [togo step curpos pos]
                (if (zero? togo)
                  curpos
                  (let [nexpos (get-next curpos dir)]
                    (if (= \# (get-in M nexpos))
                      curpos
                      (recur (dec togo) nexpos)))))
              dir-i
              (rest todo)))))))
   (map * [1000 4 1])
   (reduce +)))

;part 2, hard coded jump map that will only work for my input
(let [[M ops] (parse "2022/in22")
      e-col (atom 0)
      range-row (mapv (fn [row]
                        (let [e (count row)
                              s (count (take-while #(= % \space) row))]
                          (when (> e @e-col) (reset! e-col e))
                          [s e])) M)
      range-col (let [range-row-idx (map-indexed (fn [i v] [i v]) range-row)]
                  (vec
                   (for [c (range @e-col)]
                     (->> (filter (fn [[r [s e]]] (<= s c (dec e))) range-row-idx)
                          ((fn [l] [(ffirst l) (inc (first (last l)))]))))))
      start [0 (ffirst range-row)]
      dirs [[0 1] [1 0] [0 -1] [-1 0]]
      get-next (fn [[r c] dir-i]
                 (let [[dr dc] (get dirs dir-i)]
                   (cond
                     (zero? dr) (let [[s e] (get range-row r)
                                      newc (+ c dc)]
                                  (cond
                                    (< newc s) (cond
                                                 (<= 0 r 49) [[(+ (- 49 r) 100) 0] 0]
                                                 (<= 50 r 99) [[100 (- r 50)] 1]
                                                 (<= 100 r 149) [[(- 49 (- r 100)) 50] 0]
                                                 (<= 150 r 199) [[0 (+ 50 (- r 150))] 1])
                                    (= newc e) (cond
                                                 (<= 0 r 49) [[(+ (- 49 r) 100) 99] 2]
                                                 (<= 50 r 99) [[49 (+ (- r 50) 100)] 3]
                                                 (<= 100 r 149) [[(- 49 (- r 100)) 149] 2]
                                                 (<= 150 r 199) [[149 (+ (- r 150) 50)] 3])
                                    :else [[r newc] dir-i]))
                     :else (let [[s e] (get range-col c)
                                 newr (+ r dr)]
                             (cond
                               (< newr s) (cond
                                            (<= 0 c 49) [[(+ 50 c) 50] 0]
                                            (<= 50 c 99) [[(+ (- c 50) 150) 0] 0]
                                            (<= 100 c 149) [[199 (- c 100)] 3])
                               (= newr e) (cond
                                            (<= 0 c 49) [[0 (+ c 100)] 1]
                                            (<= 50 c 99) [[(+ (- c 50) 150) 49] 2]
                                            (<= 100 c 149) [[(+ (- c 100) 50) 99] 2])
                               :else [[newr c] dir-i])))))]
  (->>
   (loop [pos start dir-i 0 todo ops]
     (if (empty? todo)
       (conj (mapv inc pos) dir-i)
       (let [doing (first todo)]
         (case doing
           "R" (recur pos (mod (inc dir-i) 4) (rest todo))
           "L" (recur pos (mod (dec dir-i) 4) (rest todo))
           (let [step (read-string doing)
                 [curpos innerdir-i] (loop [togo step curpos pos innerdir-i dir-i]
                                       (if (zero? togo)
                                         [curpos innerdir-i]
                                         (let [[nexpos newdir-i] (get-next curpos innerdir-i)]
                                           (if (= \# (get-in M nexpos))
                                             [curpos innerdir-i]
                                             (recur (dec togo) nexpos newdir-i)))))]
             (recur curpos innerdir-i (rest todo)))))))
   (map * [1000 4 1])
   (reduce +)))