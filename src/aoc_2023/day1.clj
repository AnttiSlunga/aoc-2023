(ns aoc-2023.day1
  (:require [clojure.java.io :as io])
  (:import java.util.regex.Pattern))


(defn safe-parse-int
  ([x]
   (try
     (Integer/parseInt x)
     (catch Exception e
       nil))))

(defn first-digit [ip]
  (->> (mapv
         (fn [value]
           (->> (mapv
                  #(safe-parse-int %)
                  (clojure.string/split value #""))
                (remove nil?)))
         ip)
       flatten
       first))

(defn part1 []
  (let [input (-> (slurp (io/resource "day1"))
                  (clojure.string/split #"\n"))
        input (mapv #(clojure.string/split % #"\n") input)]
    (->> input
         (mapv (fn [vv] (safe-parse-int (str (first-digit vv) (first-digit [(clojure.string/reverse (first vv))])))))
         (apply +))))

(def words-to-digit
  {"one" "1"
   "two" "2"
   "three" "3"
   "four" "4"
   "five" "5"
   "six" "6"
   "seven" "7"
   "eight" "8"
   "nine" "9"})

(defn rep-digit [value]
  (let [words (->> (mapv
                     (fn [x]
                       (loop [result []
                              rest value]
                         (if (empty? rest)
                           result
                           (recur (let [id (clojure.string/index-of rest x)]
                                      (if (some? id)
                                        (merge result
                                              {:id id
                                               :digit (safe-parse-int (get words-to-digit x))})
                                        result))
                                  (if (some? (clojure.string/index-of rest x))
                                    (clojure.string/replace-first rest (Pattern/compile x) "xxxx")
                                    nil)))))
                     (keys words-to-digit))
                   flatten
                   (remove nil?))
        digits (->> (map-indexed
                      (fn [idx y] (if (not (nil? (safe-parse-int (str y))))
                                {:id idx
                                 :digit (safe-parse-int (str y))}))
                      value)
                    (remove nil?))]
    (->> (into words digits)
         (sort-by :id)
         (map :digit)
         (apply str))))

;; correct 54845
(defn part2 []
  (let [input (-> (slurp (io/resource "day1"))
                  (clojure.string/split #"\n"))
        input (mapv #(clojure.string/split % #"\n") input)]
    (->> input
         (mapv (fn [line]
                 (let [parsed-line (rep-digit (first line))]
                   (safe-parse-int (str (first parsed-line) (last parsed-line))))))
         (apply +))))