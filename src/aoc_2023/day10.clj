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
         [(if (< column (dec width))
            [row-id (inc column)])
          (if (>= column 1)
            [row-id (dec column)])
          (if (< row-id (dec height))
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
  (let [input (-> (slurp (io/resource "day10_test3"))
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
        _ (println "start add?" start-ad)
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

;all 526
;max 479
; 250 too low
; 300 too high
; not 258
; 285
(defn part2 []
  (let [input-raw (-> (slurp (io/resource "day10"))
                  (str/split #"\r\n"))
        start (find-start input-raw)
        _ (println "start " start)
        input (->> input-raw
                   (mapv #(clojure.string/split % #" ")))
        start-ad (->> (map
                        #(if-not (= "." (from input %))
                           [% (from input %)])
                        (adjacents start))
                      (remove nil?))
        _ (println "start add?" start-ad)
        first-step (last start-ad)
        pipe-points
        (set (loop
               [current-step (first first-step)
                last-step start
                pipe-points [start]]
               (if (= (from input current-step) "S")
                 pipe-points
                 (let [pipe-shape (from input current-step)
                       next-step (first (remove #((set [current-step last-step]) %) (next-step current-step pipe-shape)))]
                   (recur next-step
                          current-step
                          (conj pipe-points current-step))))))
        _ (println "putki " (sort pipe-points))
        _ (println "putki rivi 4" (sort (filter #(= (first %) 4) pipe-points)))
        _ (def pipe* pipe-points)
        #_ (clojure.pprint/pprint (loop
                                   [current-step (first first-step)
                                    last-step start
                                    pipe-points [start]]
                                   (if (= (from input current-step) "S")
                                     pipe-points
                                     (let [pipe-shape (from input current-step)
                                           next-step (first (remove #((set [current-step last-step]) %) (next-step current-step pipe-shape)))]
                                       (recur next-step
                                              current-step
                                              (conj pipe-points current-step))))))]
    (->> input-raw
         (map-indexed
           (fn [row v]
             (->> v
                  (map-indexed
                    (fn [column ground]
                      (if (and (not (zero? column))
                               (not (zero? row))
                               (not (= (dec width) column))
                               (not (= (dec height) row))
                               (empty? (clojure.set/intersection pipe-points (set [[row column]]))))
                        (let [line-to-edge
                              (mapv
                                (fn [c] [row c])
                                (range column width))
                              line-to-another-edge
                              (mapv
                                (fn [c] [row c])
                                (range 0 column))
                              line-to-bottom
                              (mapv
                                (fn [r] [r column])
                                (range row 139))
                              line-to-up
                              (mapv
                                (fn [r] [r column])
                                (range 0 row))

                              #_ (println [row column] "line  to edge : " line-to-edge)
                              #_ (println "sisällä " (odd? (count (clojure.set/intersection pipe-points (set line-to-edge)))))
                              #_ (println "rivi takaisin " line-to-another-edge)]
                          (if (and (not (>= 0 (count (clojure.set/intersection pipe-points (set line-to-another-edge)))))
                                   (odd? (count (clojure.set/intersection pipe-points (set line-to-edge))))
                                   (not (>= 0 (count (clojure.set/intersection pipe-points (set line-to-up)))))
                                   ;(odd? (count (clojure.set/intersection pipe-points (set line-to-bottom))))
                                   )
                            [row column])))))
                  (remove nil?)
                  )))
         (remove empty?)
         flatten
         (partition 2)
         count)))