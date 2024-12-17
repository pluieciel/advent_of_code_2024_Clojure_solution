;advent-of-code-2024.day-17
(ns day17
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse [input]
  (->> (slurp input) (re-seq #"\d+") (map read-string)))

;part 1
(let [[a b c & lst] (parse "input")
      [A B C ptr] (map atom [a b c 0])
      lst (vec lst)
      out (transient [])
      comb #(case % 4 @A 5 @B 6 @C 7 "ERROR" %)]
  (while (get lst @ptr)
    (let [op (get lst @ptr)
          va (get lst (inc @ptr))]
      (case op
        0 (reset! A (int (/ @A (Math/pow 2 (comb va)))))
        1 (reset! B (bit-xor (mod @B 8) va))
        2 (reset! B (mod (comb va) 8))
        3 (when (not= @A 0) (reset! ptr va))
        4 (reset! B (bit-xor @B @C))
        5 (conj! out (mod (comb va) 8))
        6 (reset! B (int (/ @A (Math/pow 2 (comb va)))))
        7 (reset! C (int (/ @A (Math/pow 2 (comb va))))))
      (when (not (and (= op 3) (not= @A 0))) (swap! ptr + 2))))
  (->> (persistent! out)
       (str/join ",")
       println))

;part 2
;only work for this program:
;Program: 2,4,1,3,7,5,4,2,0,3,1,5,5,5,3,0
(defn rev [n]
  (filter #(let [x (first %)
                 z (bit-xor (mod x 8) 3)]
             (= (bit-xor n 5) (mod (bit-xor z (bit-shift-right x z)) 8)))
          (map #(vector % (bit-and % 2r1111111) (bit-shift-right % 3))
               (range 1024))))

(defn step [doing lst]
  (apply concat
         (for [v doing :when (some #(= (last (last v)) (% 1)) lst)]
           (let [[_ _ r3] (last v)]
             (for [nxt (filter #(= r3 (% 1)) lst)]
               (conj v nxt))))))

(let [[a b c & lst] (parse "input")
      candi (map rev lst)] 
  (->> (loop [doing (map #(vector %) (first candi))
              todo (rest candi)]
         (if (empty? todo)
           doing
           (recur (step doing (first todo)) (rest todo))))
       (map #(map first %))
       (map (fn [lst]
              (loop [todo (rest lst) res (first lst) pos 8]
                (if (empty? todo)
                  res
                  (recur (rest todo) (+' (mod res pos) (*' (first todo) pos)) (*' pos 8))))))
       (apply min)
       println))