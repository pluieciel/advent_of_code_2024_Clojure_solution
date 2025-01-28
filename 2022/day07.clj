;advent-of-code-2022.day07
(ns day07
  (:require [clojure.string :as str]))
;part 1
(let [data (str/split-lines (slurp "./2022/in07"))
      dict (atom {})
      path (atom [])
      res (atom [])]
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
          (when (<= size 100000)
            (swap! res conj size))
          size))))
  (caldir ["/"])
  (->> @res
       (reduce +)))
;part 2
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
        (let [num (read-string (first lst))]
          (swap! dict assoc-in (conj @path (second lst)) num))))) 
  (defn caldir [path]
    (let [lst (get-in @dict path)]
      (if (number? lst)
        lst
        (let [size (reduce + (map (fn [[k _]] (caldir (conj path k))) lst))]
          (swap! res assoc path size)
          size))))
  (caldir ["/"])
  (let [total 70000000
        target 30000000
        curr (@res ["/"])
        need (- target (- total curr))]
    (->> @res
         vals
         sort
         (remove #(< % need))
         first)))