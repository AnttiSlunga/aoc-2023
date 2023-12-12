(ns aoc-2023.day11
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math :as math]))

(defn insert-at [s idx add]
  (str (subs s 0 idx) add (subs s idx)))

(defn columns [input column]
  (mapv
    #(-> input (nth %) (str/split #"") (nth column))
    (range 0 (count input))))

(defn expand [input]
  (let [horizontal-expanded
        (loop
          [input input
           expanded-input []]
          (if (empty? input)
            expanded-input
            (recur (rest input)
                   (if (str/includes? (first input) "#")
                     (conj expanded-input [(first input)])
                     (conj expanded-input [(first input)] [(first input)])))))
        _ (clojure.pprint/pprint horizontal-expanded)
        horizontal-expanded (mapv #(first %) horizontal-expanded)
        width (count (first horizontal-expanded))
        vertical-expanded
        (loop
          [column 0
           exp 0
           expanded-universe horizontal-expanded]
          (if (>= column (+ width exp))
            expanded-universe
            (let [expand? (every? #(= % ".") (columns expanded-universe column))]
              (recur (if expand?
                       (inc (inc column))
                       (inc column))
                     (if expand?
                       (inc exp)
                       exp)
                     (if (every? #(= % ".") (columns expanded-universe column))
                       (->> expanded-universe
                            (mapv #(insert-at % column ".")))
                       expanded-universe)))))]
    vertical-expanded))

(defn distance [x1 y1 x2 y2]
  (+ (abs (- x2 x1)) (abs (- y2 y1))))

(defn part1 []
  (let [input (-> (slurp (io/resource "day11_test"))
                  (str/split #"\r\n"))
        universe (expand input)
        height (count universe)
        galaxies
        (->> universe
             (map-indexed
               (fn [row-id row]
                 (->> row
                      (map-indexed
                        (fn [column value]
                          (if (= (str value) "#")
                            [column (- height row-id)])))
                      (remove nil?)
                      )))
             (remove empty?)
             flatten
             (partition 2))]
    galaxies
    (->> (loop
           [galaxies galaxies
            paths []]
           (if (empty? galaxies)
             paths
             (let [shortest-path (->> (rest galaxies)
                                      (mapv (fn [galaxy] (distance (first (first galaxies))
                                                                   (second (first galaxies))
                                                                   (first galaxy)
                                                                   (second galaxy)))))]
               (recur (rest galaxies)
                      (conj paths shortest-path)))))
         (remove empty?)
         flatten
         (apply +))))

(defn count-expand [input]
  (let [y-indeksit
        (->> input
             (map-indexed
               (fn [idx row]
                 (if (not (clojure.string/includes? row "#")) idx)))
             (remove nil?)
             (into []))
        width (count (first input))
        x-indeksit
        (loop
          [column 0
           indeksit []]
          (if (>= column width)
            indeksit
            (recur (inc column)
                   (if (every? #(= % ".") (columns input column))
                     (conj indeksit column)
                     indeksit))))]
    {:x x-indeksit :y y-indeksit}))


(defn multiplier [x range]
  (loop
    [range range
     cc 0]
    (if (empty? range)
      cc
      (recur (rest range)
             (if (>= x (first range))
               (inc cc)
               cc)))))

;; kasvaa {:x 11, :y 8}
;; {:x [24 46 55 73 74 80 84 89 113 117 135], :y [16 53 57 58 60 78 91 130]}
;; y - 1
;; too high 703572203422 703796203198
;; 702152204842
(defn part2 []
  (let [input (-> (slurp (io/resource "day11"))
                  (str/split #"\r\n"))
        universe input
        height (count universe)
        expansions (count-expand input)
        galaxies
        (->> universe
             (map-indexed
               (fn [row-id row]
                 (->> row
                      (map-indexed
                        (fn [column value]
                          (if (= (str value) "#")
                            [(+ (* 999999 (multiplier column (:x expansions))) column)
                             (+ (* 999999 (multiplier (- height row-id) (:y expansions))) (- height row-id))])))
                      (remove nil?)
                      )))
             (remove empty?)
             flatten
             (partition 2))]
    galaxies
    (->> (loop
           [galaxies galaxies
            paths []]
           (if (empty? galaxies)
             paths
             (let [shortest-path (->> (rest galaxies)
                                      (mapv (fn [galaxy] (distance
                                                           (first (first galaxies))
                                                           (second (first galaxies))
                                                           (first galaxy)
                                                           (second galaxy)))))]
               (recur (rest galaxies)
                      (conj paths shortest-path)))))
         (remove empty?)
         flatten
         (apply +))
    ;expansions
    ))