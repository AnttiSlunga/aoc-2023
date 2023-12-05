(ns aoc-2023.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; seeds: 858905075 56936593 947763189 267019426 206349064 252409474 660226451 92561087 752930744 24162055 75704321 63600948 3866217991 323477533 3356941271 54368890 1755537789 475537300 1327269841 427659734

(defn find-destination [input seed]
  (let [seed-to-soil
        (->> input
             (mapv (fn [value]
                     (let [[destination source range-lenght] (mapv #(Long/parseLong %) (str/split (str/trim (first value)) #" "))]
                       (if (and (>= seed source) (<= seed (+ source range-lenght)))
                         [destination source range-lenght]))))
             (remove nil?)
             flatten
             (into []))
        next-step (if (empty? seed-to-soil)
                    seed
                    (+ (- seed (nth seed-to-soil 1)) (first seed-to-soil)))]
    next-step))

(defn read-resource [name]
  (mapv #(clojure.string/split % #"\n")
        (-> (slurp (io/resource name)) (clojure.string/split #"\n"))))

(defn part1 []
  (let [seeds [858905075 56936593 947763189 267019426 206349064 252409474 660226451 92561087 752930744 24162055 75704321 63600948 3866217991 323477533 3356941271 54368890 1755537789 475537300 1327269841 427659734]
        seed-soil (read-resource "d5_seed_to_soil")
        soil-ferti (read-resource "soil_to_ferti")
        ferti-water (read-resource "ferti_to_water")
        water-light (read-resource "water_to_light")
        light-temp (read-resource "light_to_temp")
        temp-humi (read-resource "temp_to_humi")
        humi-location (read-resource "humi_to_location")

        location-id
        (mapv
          #(->> %
               (find-destination seed-soil)
               (find-destination soil-ferti)
               (find-destination ferti-water)
               (find-destination water-light)
               (find-destination light-temp)
               (find-destination temp-humi)
               (find-destination humi-location))
          seeds)]
    (sort location-id)))
