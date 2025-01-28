;advent-of-code-2022.day07
(ns day07
  (:require [clojure.string :as str]))

(let [data (str/split-lines (slurp "./2022/in07"))
      dict (atom {})
      path (atom [])
      res (atom {})]
  (doseq [line data]
    (let [lst (str/split line #" ")]
      (case (first lst)
        "$" (case (second lst)
              "cd" (if (= ".." (last lst))
                     (swap! path pop)
                     (swap! path conj (last lst)))
              "ls" nil)
        "dir" (swap! dict assoc-in (conj @path (second lst)) {})
        (swap! dict assoc-in (conj @path (second lst)) (read-string (first lst))))))
  (defn caldir [path]
    (let [lst (get-in @dict path)]
      (if (number? lst)
        lst
        (let [size (reduce + (map (fn [[k _]] (caldir (conj path k))) lst))] 
          (swap! res assoc path size)
          size))))
  (caldir ["/"])
  ;part 1
  (->> @res
       vals
       (filter #(<= % 100000))
       (reduce +))
  ;part 2
  (let [total 70000000
        target 30000000
        curr (@res ["/"])
        need (- target (- total curr))]
    (->> @res
         vals
         sort
         (remove #(< % need))
         first)))