(ns aoc-2023.day3
  (:require [clojure.java.io :as io]))


(defn safe-parse-int
  ([x]
   (try
     (Integer/parseInt x)
     (catch Exception e
       nil))))

(defn engine-part? [value]
  (and (not (nil? value))
       (not (= "." value))
       (not (number? (safe-parse-int value)))))

(defn gear? [value]
  (= "*" value))

(defn adjacents [[start end] row-id]
  (let [start (max 0 (dec start))
        end (min 140 (inc end))]
    (concat
      (if (>= row-id 1)
        (mapv
          (fn [v] [(dec row-id) v])
          (range start end)))
      (if (>= start 1)
        [[row-id start]])
      [[row-id (dec end)]]
      (mapv
        (fn [v] [(inc row-id) v])
        (range start end)))))

(defn from [input row column]
  (try
    (-> (nth input row)
        first
        (clojure.string/split #"")
        (nth column))
    (catch Exception e
      nil)))

;539637
(defn part1 []
  (let [input (-> (slurp (io/resource "day3"))
                  (clojure.string/split #"\n"))
        input (->> input
                   (mapv #(clojure.string/split % #"\r")))]
    (->> input
         (map-indexed
           (fn [row-id row]
             (let [matcher (re-matcher #"\d+" (first row))
                   smatcher (re-matcher #"\d+" (first row))]
               (loop [parts []]
                 (if (not (.find smatcher))
                   parts
                   (let [matchi (re-find matcher)
                         start (.start matcher)
                         end (.end matcher)
                         add (some #(engine-part? %)
                                   (mapv
                                     (fn [[row column]] (from input row column))
                                     (adjacents [start end] row-id)))]
                     (recur (if add (conj parts (safe-parse-int matchi)) parts))))))))
         (remove empty?)
         flatten
         (apply +))))

(defn part2 []
  (let [input (-> (slurp (io/resource "day3_1"))
                  (clojure.string/split #"\n"))
        input (->> input
                   (mapv #(clojure.string/split % #"\r")))]
    (->> input
         (map-indexed
           (fn [row-id row]
             (let [matcher (re-matcher #"\d+" (first row))
                   smatcher (re-matcher #"\d+" (first row))]
               (loop [parts []]
                 (if (not (.find smatcher))
                   parts
                   (let [matchi (re-find matcher)
                         start (.start matcher)
                         end (.end matcher)
                         add (some #(gear? %)
                                   (mapv
                                     (fn [[row column]] (from input row column))
                                     (adjacents [start end] row-id)))
                         gear-loc (->> (map
                                         (fn [[r c]] (if (gear? (from input r c)) [r c]))
                                         (adjacents [start end] row-id))
                                       (remove nil?)
                                       first)
                         foo (if (and add gear-loc)
                               (adjacents [(last gear-loc) (last gear-loc)] (first gear-loc)))
                         bar (map
                               (fn [[r c]] (from input r c))
                               foo)
                         baz (apply str bar)
                         _ (println "gl " gear-loc " bar " (mapv
                                                             (fn [v] (clojure.string/split v #"\."))
                                                             (clojure.string/split baz #"\*")))
                         countti (count (remove empty? (flatten (remove empty? (mapv
                                                                                 (fn [v] (clojure.string/split v #"\."))
                                                                                 (clojure.string/split baz #"\*"))))))
                         _ (println "countti " countti)]
                     (recur (if (>= countti 2) (conj parts (safe-parse-int matchi)) parts))))))))
         (remove empty?)
         flatten
         (apply +))))