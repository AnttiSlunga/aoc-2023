(ns aoc-2023.day3
  (:require [clojure.java.io :as io]))


(defn safe-parse-int
  ([x]
   (try
     (Integer/parseInt x)
     (catch Exception e
       nil))))

(defn engine-part? [value]
  (and (not (= "." value))
       (not (number? (safe-parse-int value)))))

(defn part-number? [value]
  (number? (safe-parse-int value)))

(defn adjacents [point]
  [[]])


(defn part1 []
  (let [input (-> (slurp (io/resource "day3_1"))
                  (clojure.string/split #"\n"))
        input (->> input
                   (mapv #(clojure.string/split % #"\r")))]
    (->> input
         (map-indexed
           (fn [row-id row]
             (let [matcher (re-matcher #"\d+" (first row))
                   end 0]
               (loop [parts []
                      end end]
                 (println "end-index" end)
                 (if (every? #(= % ".") (clojure.string/split (apply str (last (split-at end (first row)))) #""))
                   parts
                   (let [matchi (re-find matcher)
                         _ (println "matchi " matchi)
                         start (.start matcher)
                         end (.end matcher)
                         _ (println "s " start " e " end)]
                     (recur (conj parts matchi)
                            end))))))))))