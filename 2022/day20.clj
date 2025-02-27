;advent-of-code-2022.day20
(ns day20
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines
       (map read-string)))

;part 1
(let [arr (parse "2022/in20")
      len (count arr)
      i_arr (map-indexed (fn [idx v] [idx v]) arr)
      res (loop [doing 0 state_idx 0 state (vec i_arr)]
            ;(println state doing state_idx)
            (if (= doing len)
              state
              (let [[idx v] (get state state_idx)]
                (if (< idx doing)
                  (recur doing (inc state_idx) state)
                  (recur (inc doing)
                         state_idx
                         (let [newpos (mod (+ state_idx v) (dec len))
                               newl (concat (take state_idx state)
                                            (drop (inc state_idx) state))]
                           (vec
                            (concat (take newpos newl)
                                    (conj
                                     (drop newpos newl)
                                     [idx v])))))))))
      final (mapv second res)
      idx-z (first (for [i (range len) :when (zero? (get final i))] i))
      idx [1000 2000 3000]]
  (->> idx
       (map #(get final (mod (+ idx-z %) len)))
       (reduce +)))

;part 2
(let [arr (parse "2022/in20")
      len (count arr)
      cst 811589153
      i_arr (map-indexed (fn [idx v] [idx v]) (map #(* % cst) arr))
      get_state_idx (fn [idx state]
                      (loop [i 0]
                        (cond
                          (= i len) (throw (Exception. "idx not found"))
                          (= idx (first (state i))) i
                          :else (recur (inc i)))))
      res (loop [time 0 temp (vec i_arr)]
            (if (= time 10)
              temp
              (recur
               (inc time)
               (loop [doing 0 state temp]
                 (if (= doing len)
                   state
                   (let [state_idx (get_state_idx doing state)
                         [idx v] (get state state_idx)
                         newpos (mod (+ state_idx v) (dec len))
                         newl (concat (take state_idx state)
                                      (drop (inc state_idx) state))
                         newstate (vec
                                   (concat
                                    (take newpos newl)
                                    (conj
                                     (drop newpos newl)
                                     [idx v])))
                         newdoing (inc doing)]
                     (recur newdoing newstate)))))))
      final (mapv second res)
      idx-z (first (for [i (range len) :when (zero? (get final i))] i))
      idx [1000 2000 3000]]
  (->> idx
       (map #(get final (mod (+ idx-z %) len)))
       (reduce +)))