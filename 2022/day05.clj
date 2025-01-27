;advent-of-code-2022.day05
(ns day05
  (:require [clojure.string :as str]))

(defn parse [i]
  (str/split (slurp i) #"\n\n"))

;part 1
(let [[init lines] (parse "./2022/in05")
      lines (->> lines str/split-lines (map #(map read-string (re-seq #"\d+" %))))
      stacks (->> init
                  str/split-lines
                  butlast
                  (map #(re-seq #".{4}|.{3}$" %))
                  (apply map vector)
                  (mapv (fn [stack]
                          (vec
                           (into '()
                                 (keep #(if-let [[a] (re-seq #"\w" %)] a nil) stack)))))
                  atom)]
  (doseq [[n src dst] lines]
    (let [src (dec src) dst (dec dst)
          srcstack (@stacks src)
          l (count srcstack)
          nl (- l n)
          newsrc (vec (take nl srcstack))
          dststack (@stacks dst)
          toadd (take n (reverse srcstack))
          newdst (into dststack toadd)]
      (swap! stacks assoc src newsrc)
      (swap! stacks assoc dst newdst)))
  (->> @stacks
       (map last)
       (str/join "")))

;part 2
(let [[init lines] (parse "./2022/in05")
      lines (->> lines str/split-lines (map #(map read-string (re-seq #"\d+" %))))
      stacks (->> init
                  str/split-lines
                  butlast
                  (map #(re-seq #".{4}|.{3}$" %))
                  (apply map vector)
                  (mapv (fn [stack]
                          (vec
                           (into '()
                                 (keep #(if-let [[a] (re-seq #"\w" %)] a nil) stack)))))
                  atom)]
  (doseq [[n src dst] lines]
    (let [src (dec src) dst (dec dst)
          srcstack (@stacks src)
          l (count srcstack)
          nl (- l n)
          newsrc (vec (take nl srcstack))
          dststack (@stacks dst)
          toadd (drop nl srcstack)
          newdst (into dststack toadd)]
      (swap! stacks assoc src newsrc)
      (swap! stacks assoc dst newdst)))
  (->> @stacks
       (map last)
       (str/join "")))