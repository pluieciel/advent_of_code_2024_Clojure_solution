;advent-of-code-2022.day17
(ns day17)

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

(def data (parse "./2022/in17"))
(def lenjets (count data))
(def jets (cycle data))

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

(defn height [n]
  (loop [rocks rocks jets jets cnt 0 Map []]
    (if (= cnt n)
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
        (recur (rest rocks) newjets (inc cnt) newMap)))))

;part 1
(height 2022)

;part 2
;find period
(let [mem (atom {})]
  (loop [rocks rocks jets jets cnt 0 Map [] idxr 0 idxj 0]
    (let [[newjets newidxj newMap]
          (loop [rock (first rocks)
                 njs jets
                 bpos (+ (count Map) 3)
                 nM (into Map (repeat (+ 3 (count rock)) walls))
                 alter 0
                 idxj idxj]
            (if (zero? alter);jet/fall
              (let [nr (dojet rock bpos nM (first njs))]
                (recur nr (rest njs) bpos nM 1 (inc idxj)))
              (if (dofall rock bpos nM)
                (recur rock njs (dec bpos) nM 0 idxj)
                [njs idxj (->> (reduce (fn [v [i r]]
                                         (assoc v i (bit-or (get v i) r)))
                                       nM (map-indexed #(vector (+ %1 bpos) %2) rock))
                               (#(loop [m %]
                                   (if (= walls (last m))
                                     (recur (butlast m))
                                     (vec m)))))])))
          newidxr (mod (inc idxr) 5)
          newidxj (mod newidxj lenjets)]
      (if-let [precnt (get @mem [newidxr newidxj (take-last 8 newMap)])]
        [precnt (inc cnt)]
        (do
          (swap! mem assoc [newidxr newidxj (take-last 8 newMap)] (inc cnt))
          (recur (rest rocks) newjets (inc cnt) newMap newidxr newidxj))))))

;; example period [17 52]
;; (mod (- 1000000000000 17) (- 52 17))
;; (quot (- 1000000000000 17) (- 52 17))
;; (height 17)
;; (height 52)
;; (height (+ 17 33))
;; (+ 78 (* 28571428570 (- 82 29)))

;; input period [319 2014]
;; (mod (- 1000000000000 319) (- 2014 319))
;; (quot (- 1000000000000 319) (- 2014 319))
;; (height 319)
;; (height 2014)
;; (height (+ 319 486))
;; (+ 1250 (* 589970501 (- 3140 506)))