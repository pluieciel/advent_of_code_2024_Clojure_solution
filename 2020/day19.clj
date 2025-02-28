;advent-of-code-2020.day19
(ns day19
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (str/split (slurp i) #"\n\n")
       (#(let [[rules strs] %]
           [(->> rules
                 str/split-lines
                 (map (fn [rule]
                        (let [[r-idx r-content] (str/split rule #": ")
                              ab (re-find #"[ab]" r-content)
                              r-idx (read-string r-idx)]
                          (if ab
                            {r-idx (first ab)}
                            {r-idx (->> (str/split r-content #" \| ")
                                        (map (fn [r] (map read-string (re-seq #"\d+" r)))))}))))
                 (into {}))
            (->> strs str/split-lines)]))))

(let [[rules strs] (parse "2020/in19")]
  (def rules rules)
  (def strs strs))

(defn matchone [letter lst-rule]
  (mapcat
   #(let [to-expand (get rules (first %))]
      (if (#{\a \b} to-expand)
        (if (= letter to-expand) [(rest %)] '())
        ((partial matchone letter)
         (for [newr to-expand]
           (concat newr (rest %)))))) lst-rule))

(defn check [str]
  (loop [remain str todo [(first (get rules 0))]]
    (if (empty? remain)
      (some empty? todo)
      (let [to-match (first remain)
            newtodo (matchone to-match todo)]
        (recur (rest remain) newtodo)))))

;part 1
(->> (keep check strs)
     count)

;part 2
(def rules (-> rules (assoc 8 '((42) (42 8))) (assoc 11 '((42 31) (42 11 31)))))

(->> (keep check strs)
     count)