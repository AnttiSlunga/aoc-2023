(ns aoc-2023.day10
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def width 140)
(def height 140)

(defn find-start [input]
  (->> input
       (map-indexed
         (fn [row v]
           (map-indexed
             (fn [column hill]
               (if (= "S" (str hill))
                 [row column])) v)))
       flatten
       (remove nil?)
       (into [])))

(defn from [input [row column]]
  (try
    (-> (nth input row)
        first
        (clojure.string/split #"")
        (nth column))
    (catch Exception e
      nil)))

(defn adjacents [[row-id column]]
  (->> (concat
         [(if (< column (dec height))
            [row-id (inc column)])
          (if (>= column 1)
            [row-id (dec column)])
          (if (< row-id (dec width))
            [(inc row-id) column])
          (if (>= row-id 1)
            [(dec row-id) column])])
       (remove nil?)))

(defn next-step [[r c] pipe-shape]
  (case pipe-shape
    "|" [[(max 0 (dec r)) c] [(min height (inc r)) c]]
    "-" [[r (max 0 (dec c))] [r (min width (inc c))]]
    "L" [[(max 0 (dec r)) c] [r (min width (inc c))]]
    "J" [[(max 0 (dec r)) c] [r (max 0 (dec c))]]
    "7" [[(min height (inc r)) c] [r (max 0 (dec c))]]
    "F" [[(min height (inc r)) c] [r (min width (inc c))]]))

;; first 20
;; second 14060
;; rest second 14056
;; last 14060
(defn part1 []
  (let [input (-> (slurp (io/resource "day10"))
                  (str/split #"\r\n"))
        start (find-start input)
        _ (println "start " start)
        input (->> input
                   (mapv #(clojure.string/split % #" ")))
        start-ad (->> (map
                        #(if-not (= "." (from input %))
                           [% (from input %)])
                        (adjacents start))
                      (remove nil?))
        first-step (last start-ad)]
    (loop
      [current-step (first first-step)
       last-step start
       moves 1]
      (if (= (from input current-step) "S")
        moves
        (let [pipe-shape (from input current-step)
              next-step (first (remove #((set [current-step last-step]) %) (next-step current-step pipe-shape)))]
          (recur next-step
                 current-step
                 (inc moves)))))))