;advent-of-code-2022.day17
(ns day17
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       vec))

(def rocks (cycle
            [[2r111100]
             [2r10000 2r111000 2r10000]
             [2r111000 2r1000 2r1000];first one is bottom
             [2r100000 2r100000 2r100000 2r100000]
             [2r110000 2r110000]]))

(def walls 2r100000001)

(def jets (cycle (parse "./2022/in17")))

(defn dojet [rock bpos Map jet]
  (let [op (if (= jet \<) #(bit-shift-left % 1) #(bit-shift-right % 1))
        nr (map op rock)
        tocheck (subvec Map bpos (+ bpos (count rock)))
        check (map bit-and nr tocheck)]
    (if (every? zero? check)
      nr
      rock)))

(defn dofall [rock bpos Map]
  (if (zero? bpos)
    false
    (let [newbpos (dec bpos)
          tocheck (subvec Map newbpos (+ newbpos (count rock)))
          check (map bit-and rock tocheck)]
      (every? zero? check))))

(loop [rocks rocks jets jets cnt 0 Map []]
  (if (= cnt 2022)
    (count Map)
    (let [[newjets newMap]
          (loop [rock (first rocks)
                 njs jets
                 bpos (+ (count Map) 3)
                 nM (into Map (repeat (+ 3 (count rock)) walls))
                 alter 0]
            (if (zero? alter);jet/fall
              (let [nr (dojet rock bpos nM (first njs))]
                (recur nr (rest njs) bpos nM 1))
              (if (dofall rock bpos nM)
                (recur rock njs (dec bpos) nM 0)
                [njs (->> (reduce (fn [v [i r]]
                                 (assoc v i (bit-or (get v i) r)))
                               nM (map-indexed #(vector (+ %1 bpos) %2) rock))
                          (#(loop [m %]
                              (if (= walls (last m))
                                (recur (butlast m))
                                (vec m)))))])))]
      (recur (rest rocks) newjets (inc cnt) newMap))))
