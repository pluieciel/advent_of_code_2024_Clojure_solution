;advent-of-code-2019.day07
(ns day07
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.math.combinatorics :as comb]
            [clojure.core.async :as async :refer [>! <! >!! <!! go chan]]))

(def program
  (->> (str/split (slurp "2019/in07") #",")
       (map read-string)
       (map vector (range))
       (into {})))

(defn calone [op dict pos modes]
  (assoc dict
         (get dict (+ pos 3))
         (op (if (zero? (get modes 0 0))
               (dict (dict (+ pos 1)))
               (dict (+ pos 1)))
             (if (zero? (get modes 1 0))
               (dict (dict (+ pos 2)))
               (dict (+ pos 2))))))

(defn calcmp [op dict pos modes]
  (assoc dict
         (get dict (+ pos 3))
         (if (op (if (zero? (get modes 0 0))
                   (dict (dict (+ pos 1)))
                   (dict (+ pos 1)))
                 (if (zero? (get modes 1 0))
                   (dict (dict (+ pos 2)))
                   (dict (+ pos 2))))
           1 0)))

(defn jump [dict pos modes f]
  (let [arg (if (zero? (get modes 0 0))
              (dict (dict (+ pos 1)))
              (dict (+ pos 1)))]
    (if (f arg)
      (+ pos 3)
      (if (zero? (get modes 1 0))
        (dict (dict (+ pos 2)))
        (dict (+ pos 2))))))

(defn cal [dict pos inputflag inputs]
  (if (= 99 (get dict pos))
    nil
    (let [op-long (get dict pos)
          [op & modes] (->> op-long
                            str
                            (re-seq #"\d\d$|\d")
                            reverse
                            (map Integer/parseInt))
          modes (vec modes)]
      (case op
        1 (cal (calone + dict pos modes) (+ pos 4) inputflag inputs)
        2 (cal (calone * dict pos modes) (+ pos 4) inputflag inputs)
        3 (cal (assoc dict (dict (inc pos)) (inputs inputflag)) (+ pos 2) (inc inputflag) inputs)
        4 (dict (dict (inc pos)))
        5 (cal dict (jump dict pos modes zero?) inputflag inputs)
        6 (cal dict (jump dict pos modes #(not (zero? %))) inputflag inputs)
        7 (cal (calcmp < dict pos modes) (+ pos 4) inputflag inputs)
        8 (cal (calcmp = dict pos modes) (+ pos 4) inputflag inputs)))))

(defn cal-phases [phases]
  (reduce
   (fn [acc phase]
     (cal program 0 0 [phase acc]))
   0
   phases))

;; part1
(->> (comb/permutations (range 5))
     (map cal-phases)
     (apply max))

;; part2
(defn cal2 [dict pos amp final channels res]
  (if (= 99 (get dict pos))
    (when (= amp 4) (>!! res final))
    (let [op-long (get dict pos)
          [op & modes] (->> op-long
                            str
                            (re-seq #"\d\d$|\d")
                            reverse
                            (map Integer/parseInt))
          modes (vec modes)]
      (case op
        1 (cal2 (calone + dict pos modes) (+ pos 4) amp final channels res)
        2 (cal2 (calone * dict pos modes) (+ pos 4) amp final channels res)
        3 (cal2 (assoc dict (dict (inc pos)) (<!! (channels amp))) (+ pos 2) amp final channels res)
        4 (let [result (dict (dict (inc pos)))]
            (>!! (channels (mod (inc amp) 5)) result)
            (cal2 dict (+ pos 2) amp (if (= amp 4) result final) channels res))
        5 (cal2 dict (jump dict pos modes zero?) amp final channels res)
        6 (cal2 dict (jump dict pos modes #(not (zero? %))) amp final channels res)
        7 (cal2 (calcmp < dict pos modes) (+ pos 4) amp final channels res)
        8 (cal2 (calcmp = dict pos modes) (+ pos 4) amp final channels res)))))

(defn cal-phases2 [phases channels res]
  (doseq [i (range 5)]
    (>!! (channels i) (phases i)))
  (>!! (channels 0) 0)
  (doseq [amp (range 5)]
    (future (cal2 program 0 amp 0 channels res)))
  (<!! res))

(->> (comb/permutations (range 5 10))
     (map #(cal-phases2
            %
            (->> (for [amp (range 5)]
                   {amp (chan 10)})
                 (into {}))
            (chan 1)))
     (apply max))