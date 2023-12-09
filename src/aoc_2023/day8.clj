(ns aoc-2023.day8
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn part1 []
  (let [input (-> (slurp (io/resource "day8"))
                  (str/split #"\r\n")
                  first
                  (str/split #"\n"))
        original-directions (first input)
        network  (->> (map
                        (fn [vv]
                          (let [[key value] (str/split vv #"=")]
                            [(clojure.string/trim key)
                             (-> value
                                 (str/trim)
                                 (str/replace #"\(" "")
                                 (str/replace #"\)" "")
                                 (str/split #", "))]))
                        (rest input))
                      (into {}))]
    (loop
      [directions original-directions
       moves 0
       position "AAA"]
      (if (= position "ZZZ")
        moves
        (let
          [dir (first directions)
           next-pos (if (= "L" (str dir))
                      (first (get network position))
                      (last (get network position)))
           next-directions (if (empty? (apply str (rest directions)))
                             original-directions
                             (apply str (rest directions)))]
          (recur next-directions
                 (inc moves)
                 next-pos))))))