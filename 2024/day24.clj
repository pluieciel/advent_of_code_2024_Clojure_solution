;advent-of-code-2024.day-24
(ns day24
  (:require [clojure.string :as str]))

(defn parse [input]
  (->> (slurp input)
       (#(str/split % #"\n\n"))
       ((fn [[inputs gates]]
          [(->> inputs
                str/split-lines
                (map #(let [[k v] (str/split % #": ")]
                        {k (read-string v)}))
                (apply merge))
           (->> gates
                str/split-lines
                (map #(let [[a op b out] (re-seq #"\w+" %)]
                        [op a b out])))]))))

(let [[inputs gates] (parse "input")]
  (def inputs inputs)
  (def gates gates))

(def promises (atom {}))

(defn run-logic-gate [op a b]
  (case op
    "XOR" (bit-xor a b)
    "OR"  (bit-or a b)
    "AND" (bit-and a b)))

(defn get-value [key]
  @(get @promises key))

(defn set-promise [key value]
  (deliver (get @promises key) value))

(defn process-gates [gates]
  (doseq [[op a b out] gates]
    (future
      (set-promise out (run-logic-gate op (get-value a) (get-value b))))))

(defn init-promises [inputs]
  (doseq [k (->> gates (map rest) (apply concat) set)]
    (swap! promises assoc k (promise)))
  (doseq [[k v] inputs]
    (deliver (get @promises k) v)))

(def pow2s (iterate #(* 2 %) 1))

(defn calculate [inputs gates]
  (reset! promises {})
  (init-promises inputs)
  (process-gates gates)
  (->>  @promises
        (keep (fn [[k p]] (when (= \z (first k)) [k @p])))
        (sort-by first)
        (map second)
        (map * pow2s)
        (reduce +)))

;part 1
(->> (calculate inputs gates) println)

;part 2
(defn updategates [gates switch]
  (->> gates
       (map (fn [v] (update v 3 #(get switch % %))))))

;init switch with {}, and gradually modify
(def switch {"hbk" "z14"
             "z14" "hbk"
             "kvn" "z18"
             "z18" "kvn"
             "dbb" "z23"
             "z23" "dbb"
             "cvh" "tfn"
             "tfn" "cvh"})
(def newgates (updategates gates switch))

(defn forward [in]
  (->> newgates
     (filter #(let [[op a b out] %] (#{a b} in)))))
(defn backward [tar]
  (->> newgates
       (filter #(let [[op a b out] %] (= out tar)))))
;(forward "mqf")
;(backward "z23")

;this loop will chech the first bad position, use forward/backward to find the final place
(loop [lst-x (map #(str "x" (format "%02d" %1)) (range 1 45))
       carry "jfw"]
  (if (empty? lst-x)
    "finish"
    (let [[[op-and i1 i2 ntt] [op-xor i3 i4 gnj]] (sort-by first (forward (first lst-x)))
          final (str/replace (first lst-x) #"x" "z")
          [fo now] (sort-by first (forward gnj))
          [[op-or i5 i6 ndd]] (forward ntt)]
      (if (and (= (forward i5) (backward ndd))
               (= now (first (backward final)))
               (= (forward ntt) (backward ndd)))
        (recur (rest lst-x) ndd)
        [carry (forward (first lst-x))]))))

(->> switch
     keys
     sort
     (#(str/join "," %))
     println)