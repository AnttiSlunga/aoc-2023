(ns aoc-2023.day9
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))


; 1708206096
(defn part1 []
  (let [input (-> (slurp (io/resource "day9"))
                  (clojure.string/split #"\r\n"))
        input (->> input
                   (mapv (fn [value] (->> (clojure.string/split value #" ")
                                          (mapv #(Long/parseLong %))))))
        sequence
        (mapv
          #(loop
             [row %
              all-rows [%]]
             (if (every? zero? row)
               all-rows
               (let
                 [next-row
                  (loop
                    [row row
                     second-row []]
                    (if (= 1 (count row))
                      second-row
                      (recur (rest row)
                             (conj second-row (- (second row) (first row))))))]
                 (recur next-row
                        (conj all-rows next-row)))))
          input)]
    (->> sequence
         (mapv
           #(loop
             [rows (reverse %)
              history-number nil]
             (if (= 1 (count rows))
               history-number
               (let
                 [f-last (+ (last (first rows)) (last (second rows)))
                  new-row (conj (second rows) f-last)]
                 (recur (-> rows rest rest (conj new-row))
                        f-last)))))
         (apply +))))

(defn part2 []
  (let [input (-> (slurp (io/resource "day9"))
                  (clojure.string/split #"\r\n"))
        input (->> input
                   (mapv (fn [value] (->> (clojure.string/split value #" ")
                                          (mapv #(Long/parseLong %))))))
        sequence
        (mapv
          #(loop
             [row %
              all-rows [%]]
             (if (every? zero? row)
               all-rows
               (let
                 [next-row
                  (loop
                    [row row
                     second-row []]
                    (if (= 1 (count row))
                      second-row
                      (recur (rest row)
                             (conj second-row (- (second row) (first row))))))]
                 (recur next-row
                        (conj all-rows next-row)))))
          input)]
    (->> sequence
         (mapv
           #(loop
              [rows (reverse %)
               history-number nil]
              (if (= 1 (count rows))
                history-number
                (let
                  [f-last (- (first (second rows)) (first (first rows)))
                   new-row (into [f-last] (second rows))]
                  (recur (-> rows rest rest (conj new-row))
                         f-last)))))
         (apply +))))