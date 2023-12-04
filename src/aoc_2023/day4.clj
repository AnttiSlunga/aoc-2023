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

(defn new-cards [all-cards [game score]]
  (when game
    (if (= 0 score)
      1
      (->> (range (inc game) (+ (inc game) score))
           (map #(new-cards all-cards [% (all-cards %)]))
           (reduce +)
           (+ 1)))))

;; 5923918
(defn part2 []
  (let [input (-> (slurp (io/resource "day4"))
                  (str/split #"\n"))
        input (mapv #(str/split % #"\n") input)
        start (System/currentTimeMillis)
        all-cards
        (->> input
             (mapv
               (fn [row]
                 (let [game (-> row
                                first
                                (str/split #":")
                                first
                                (str/split #" ")
                                last
                                safe-parse-int)
                       numbers
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
                       osumat-lkm (count osumat)]
                   [game osumat-lkm])))
             (into {}))
        foo
        (loop
          [cards all-cards
           card-count 0]
          (if (empty? cards)
            card-count
            (let [new-cards (new-cards all-cards (first cards))]
              (recur (rest cards)
                     (+ card-count new-cards)))))

        _ (println "took" (- (System/currentTimeMillis) start))]
    foo))