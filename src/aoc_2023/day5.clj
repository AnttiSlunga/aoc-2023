(ns aoc-2023.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; seeds: 858905075 56936593 947763189 267019426 206349064 252409474 660226451 92561087 752930744 24162055 75704321 63600948 3866217991 323477533 3356941271 54368890 1755537789 475537300 1327269841 427659734

;; seeds: 79 14 55 13
;
;seed-to-soil map:
;50 98 2
;52 50 48
;
;soil-to-fertilizer map:
;0 15 37
;37 52 2
;39 0 15
;
;fertilizer-to-water map:
;49 53 8
;0 11 42
;42 0 7
;57 7 4
;
;water-to-light map:
;88 18 7
;18 25 70
;
;light-to-temperature map:
;45 77 23
;81 45 19
;68 64 13
;
;temperature-to-humidity map:
;0 69 1
;1 0 69
;
;humidity-to-location map:
;60 56 37
;56 93 4

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

(def test-seeds [[79 14] [55 13]])

(defn find-destination2 [input [seed srange]]
  (let [fou
        (loop
          [seeds [seed srange]
           foundit []]
          (if (empty? seeds)
            foundit
            (let [_ (println "seed " seeds)
                  found-it
                  (->> input
                       (mapv (fn [value]
                               (let [[seed srange] seeds
                                     [destination source range-lenght] (mapv #(Long/parseLong %) (str/split (str/trim (first value)) #" "))]
                                 (cond
                                   (and (and (>= seed source) (<= seed (+ source range-lenght))) ;; osuus kokonaan rangeen
                                        (and (>= (+ seed srange) source) (<= (+ seed srange) (+ source range-lenght))))
                                   [[destination source] nil]

                                   (and (and (>= seed source) (<= seed (+ source range-lenght))) ;; alku osuu, loppu yli
                                        (> (+ seed srange) (+ source range-lenght)))
                                   [[destination source] [(+ source range-lenght) (+ seed srange 1)]]

                                   (and (< seed source)     ;; loppu osuu, alku alle
                                        (and (>= (+ seed srange) source) (<= (+ seed srange) (+ source range-lenght))))
                                   [[destination source] [seed (- source 1)]]

                                   :default
                                   nil))))
                       (remove nil?))
                  [fou seed] found-it
                  _ (println "found it " found-it)
                  _ (println "new seed " seed)]
              (recur seed
                     (conj foundit fou)))))
        _ (println "fou" fou)
        seed-to-soil
        (->>  fou
             (remove nil?)
             flatten
             (into []))
        next-step (if (empty? seed-to-soil)
                    seed
                    (+ (- seed (nth seed-to-soil 1)) (first seed-to-soil)))]
    next-step))

(defn part2 []
  (let [seeds [[858905075 56936593]
               [947763189 267019426]
               [206349064 252409474]
               [660226451 92561087]
               [752930744 24162055]
               [75704321 63600948]
               [3866217991 323477533]
               [3356941271 54368890]
               [1755537789 475537300]
               [1327269841 427659734]]
        seeda-with-ranges (->> [[858905075 56936593]]
                               (mapcat (fn [[start end]]
                                         (range start (+ start end))))
                               (take 100))
        seeds test-seeds
        seed-soil (read-resource "d5_seed_to_soil")
        soil-ferti (read-resource "soil_to_ferti")
        ferti-water (read-resource "ferti_to_water")
        water-light (read-resource "water_to_light")
        light-temp (read-resource "light_to_temp")
        temp-humi (read-resource "temp_to_humi")
        humi-location (read-resource "humi_to_location")

        start (System/currentTimeMillis)

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
          seeda-with-ranges)
        _ (println "took" (- (System/currentTimeMillis) start))]
    (sort location-id)))
