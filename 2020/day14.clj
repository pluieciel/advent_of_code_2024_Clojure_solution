;advent-of-code-2020.day14
(ns day14
  (:require [clojure.string :as str]))

(defn parse [i]
  (->> (slurp i)
       str/split-lines))

(def mask (atom ""))
(def mem (atom {}))

;part 1
(doseq [line (parse "2020/in14")]
  (if (str/starts-with? line "mask")
    (reset! mask (second (str/split line #" = ")))
    (let [[_ addr val] (re-matches #"mem\[(\d+)\] = (\d+)" line)
          str-val (str/replace
                   (format "%36s" (Long/toString (read-string val) 2))
                   " " "0")
          masked-val (apply str (map #(if (= % \X) %2 %1) @mask str-val))]
      (swap! mem assoc (read-string addr) (Long/parseLong masked-val 2)))))

(apply + (vals @mem))

;part 2
(reset! mask "")
(reset! mem {})

(defn gen-addrs [masked-addr]
  (cond
    (empty? masked-addr) ['()]
    (= \X (first masked-addr)) (let [lst (gen-addrs (rest masked-addr))]
                                 (concat (map #(conj % \0) lst) (map #(conj % \1) lst)))
    :else (map #(conj % (first masked-addr)) (gen-addrs (rest masked-addr)))))

(doseq [line (parse "2020/in14")]
  (if (str/starts-with? line "mask")
    (reset! mask (second (str/split line #" = ")))
    (let [[_ addr val] (re-matches #"mem\[(\d+)\] = (\d+)" line)
          str-addr (str/replace
                    (format "%36s" (Long/toString (read-string addr) 2))
                    " " "0")
          masked-addr (map #(if (= \0 %1) %2 %1) @mask str-addr)
          addrs (map #(apply str %) (gen-addrs masked-addr))]
      (doseq [addr addrs] (swap! mem assoc addr (read-string val))))))

(apply + (vals @mem))