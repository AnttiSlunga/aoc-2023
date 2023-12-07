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

(defn gear-numbers [row]
  (let [row-matcher  (re-matcher #"\d+" (apply str row))
        row-matcher-2 (re-matcher #"\d+" (apply str row))
        foundit (loop [founds []]
                  (if (not (.find row-matcher-2))
                    founds
                    (let [number (re-find row-matcher)
                          match-start (.start row-matcher)
                          match-end (.end row-matcher)
                          correct-number? (some (set (range match-start match-end)) (range 2 5))]
                      (recur (if correct-number? (conj founds (safe-parse-int number)) founds)))))]
    (flatten foundit)))

(defn part2 []
  (let [input (-> (slurp (io/resource "day3"))
                  (clojure.string/split #"\n"))
        input (->> input
                   (mapv #(clojure.string/split % #"\r")))]
    (->> input
         (map-indexed
           (fn [row-id row]
             (let [matcher (re-matcher #"\*" (first row))
                   smatcher (re-matcher #"\*" (first row))]
               (loop [parts []]
                 (if (not (.find smatcher))
                   parts
                   (let [start (.start matcher)
                         end (.end matcher)
                         digi-start (max 0 (- start 3))
                         digi-end (min 140 (+ end 3))
                         row-range (range digi-start digi-end)
                         upper-row (->> (mapv
                                          (fn [[row column]] (from input row column))
                                          (mapv (fn [v] [(dec row-id) v]) row-range))
                                        gear-numbers
                                        flatten)

                         same-row (->> (mapv
                                         (fn [[row column]] (from input row column))
                                         (mapv (fn [v] [row-id v]) row-range))
                                       gear-numbers
                                       flatten)
                         lower-row (->> (mapv
                                          (fn [[row column]] (from input row column))
                                          (mapv (fn [v] [(inc row-id) v]) row-range))
                                        gear-numbers)
                         ratio (if (= 2 (count (->> [upper-row same-row lower-row]
                                                    flatten)))
                                 (->> [upper-row same-row lower-row]
                                      flatten
                                      (apply *)))]
                     (recur (if ratio (conj parts ratio) parts))))))))
         (remove empty?)
         flatten
         (apply +))))