(ns aoc-2023.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn safe-parse-int
  ([x]
   (try
     (Integer/parseInt x)
     (catch Exception e
       nil))))

;; 23441
(defn part1 []
  (let [input (-> (slurp (io/resource "day4"))
                  (str/split #"\n"))
        input (mapv #(str/split % #"\n") input)]
    (->> input
         (mapv
           (fn [row]
             (let [numbers
                   (-> row
                       first
                       (str/split #":")
                       last
                       (str/split #"\|"))
                   winning-numbers (remove empty? (set (str/split (str/trim (first numbers)) #" ")))
                   players-numbers (remove empty? (set (str/split (str/trim (last numbers)) #" ")))
                   osumat (->> (set players-numbers)
                               (map #((set winning-numbers) %))
                               (remove nil?))
                   osumat-lkm (count osumat)
                   score (if (>= osumat-lkm 1)
                           (java.lang.Math/pow 2 (dec osumat-lkm))
                           0)]
               score)))
         (apply +))))

(defn part2 []
  (let [input (-> (slurp (io/resource "day4_1"))
                  (str/split #"\n"))
        input (mapv #(str/split % #"\n") input)
        _ (def in* input)]
    (loop
      [cards input
       score 0]
      (if (empty? cards)
        score
        (let [game (-> (first cards)
                       first
                       (str/split #":")
                       first
                       (str/split #" ")
                       last
                       safe-parse-int)
              _ (println "game " game)
              numbers
              (-> (first cards)
                  first
                  (str/split #":")
                  last
                  (str/split #"\|"))
              winning-numbers (remove empty? (set (str/split (str/trim (first numbers)) #" ")))
              players-numbers (remove empty? (set (str/split (str/trim (last numbers)) #" ")))
              osumat (->> (set players-numbers)
                          (map #((set winning-numbers) %))
                          (remove nil?))
              osumat-lkm (count osumat)
              new-score (if (>= osumat-lkm 1)
                          (range game (+ game osumat-lkm))
                          nil)]
          (recur (rest cards)
                 (inc score)))))))