(ns aoc-2023.day2
  (:require [clojure.java.io :as io]))

;; only 12 red cubes, 13 green cubes, and 14 blue cubes
(defn safe-parse-int
  ([x]
   (try
     (Integer/parseInt x)
     (catch Exception e
       nil))))

;; 2512
(defn part1 []
  (let [input (-> (slurp (io/resource "day2"))
                  (clojure.string/split #"\n"))
        input (mapv #(clojure.string/split % #"\n") input)]
    (->> input
         (mapv (fn [gems]
                 (let [id (-> gems
                              first
                              (clojure.string/split #":")
                              first
                              (clojure.string/split #" ")
                              last
                              safe-parse-int)
                       gems (-> gems
                                first
                                (clojure.string/split #":")
                                last
                                (clojure.string/split #";"))
                       gem (loop [results []
                                  gems (->> gems
                                            (mapv #(clojure.string/split % #","))
                                            flatten)]
                             (if (empty? gems)
                               results
                               (recur (let [[value color] (clojure.string/split
                                                            (clojure.string/trim (first gems)) #" ")
                                            value (safe-parse-int value)]
                                        (case color
                                          "blue" (conj results (<= value 14))
                                          "red" (conj results (<= value 12))
                                          "green" (conj results (<= value 13))
                                          results))
                                      (rest gems))))]
                   (if (every? true? gem)
                     id))))
         (remove nil?)
         (apply +))))

;;67335
(defn part2 []
  (let [input (-> (slurp (io/resource "day2"))
                  (clojure.string/split #"\n"))
        input (mapv #(clojure.string/split % #"\n") input)]
    (->> input
         (mapv (fn [gems]
                 (let [gems (-> gems
                                first
                                (clojure.string/split #":")
                                last
                                (clojure.string/split #";"))
                       gem (loop [results {:blue 0
                                           :red 0
                                           :green 0}
                                  gems (->> gems
                                            (mapv #(clojure.string/split % #","))
                                            flatten)]
                             (if (empty? gems)
                               results
                               (recur (let [[value color] (clojure.string/split
                                                            (clojure.string/trim (first gems)) #" ")
                                            value (safe-parse-int value)]
                                        (case color
                                          "blue" (update results :blue #(if (> value %) value %))
                                          "red" (update results :red #(if (> value %) value %))
                                          "green" (update results :green #(if (> value %) value %))
                                          results))
                                      (rest gems))))]
                   (->> gem vals (apply *)))))
         (remove nil?)
         (apply +))))