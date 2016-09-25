(ns demo.demo_genetic_alg
    (:gen-class)
    (:require [incanter.datasets :refer :all]
            [incanter.core :refer :all]
            [incanter.io :refer :all]
            [incanter.stats :refer :all]
            [genetic_algorithm.bitarray :refer :all]
            [clojure.core.reducers :as r]
            [genetic_algorithm.gen-alg :refer :all]
            [incanter.charts :refer :all]
            [clojure.pprint :refer :all]
            )

    )
; Luciano P. Sabenca
;
; Demo for genetic algorithm
; This demo is a implementation for a stocks recomendation under a restricted budget and minimizing the risks
; We create some risk/estability metrics and use genetic algorithms to find the best possible portfolio.
; NOTE: Just a simple demo. Not designed to be used in real enviroment nor to be performatic. Academic purpose only!


;;;;;;;;;;;;
;;;;;;;;;;;;
;;;;;;;;;;;;
;; Pre-processing functions - Reads the data and put it on a convenient way
;;;;;;;;;;;;
;;;;;;;;;;;;
;;;;;;;;;;;;

(def chart (atom 0))
(def total-budget 1000)                                     ;; Defines the total budget available
(def data (read-dataset "resources/sp500hst_sample.csv" :header true))

(defn calculate-benchmark-returns [data]
  (let [a (sel data :cols 4 :rows (range 0 (- (nrow data) 1)))
        b (sel data :cols 4 :rows (range 1 (nrow data)))]
    (minus a b)
    ))

(def benchmark (read-dataset "resources/benchmark.csv" :header true))

(def benchmark-returns (calculate-benchmark-returns (to-matrix benchmark)))

(def group-by-stock ($group-by :stock data))

(def stock-list (reduce (fn [assoc x]
                          (conj assoc (get (key x) :stock)))
                        []
                        group-by-stock))



(defn calculate-returns [data]
  (let [a (sel data :cols 2 :rows (range 0 (- (nrow data) 1)))
        b (sel data :cols 2 :rows (range 1 (nrow data)))]
    (minus a b)
    ))



(defn calculate-beta [data]
  (let [n (min (nrow data) (nrow benchmark-returns))]
    (/ (covariance (sel data :rows (range n))
                   (sel benchmark-returns :rows (range n)))
       (variance benchmark-returns))))

(defn calculate-stock-data [data]
  {:std-dev (sd (sel data :cols 2))
   :price   (second (apply max-key first (sel data :cols [0 2])))
   :beta    (abs (calculate-beta (calculate-returns data)))
   }
  )



(def data-by-stock
  (reduce
    (fn [x y]
      (assoc x
        (get (key y) :stock)
        (calculate-stock-data
          (to-matrix (val y)))))
    {} group-by-stock))



;;;;;;;;;;;;
;;;;;;;;;;;;
;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;
;;;;;;;;;;;;
;;;;;;;;;;;;
(defn into-improved ([] []) ([x y] (into x y)))


(defn calculate-risk [stock]
  (let [std-dev (get (get data-by-stock stock) :std-dev)
        beta (get (get data-by-stock stock) :beta)]
    ($= 1 / (beta * std-dev)))
  )

(defn calculate-estability [stock]
  (let [std-dev (get (get data-by-stock stock) :std-dev)
        price (get (get data-by-stock stock) :price)
        beta (get (get data-by-stock stock) :beta)]
    ($= price - beta * std-dev))
  )


(defn get-price [stock]
  (get (get data-by-stock stock) :price))

(defn sum-price-and-estability
  ([] {:total-price 0, :estability 0})
  ([{total-price-A :total-price estability-A :estability}
    {total-price-B :total-price estability-B :estability}]

   (assoc
     (assoc {} :total-price (+ total-price-A total-price-B))
     :estability
     (+ estability-A
        estability-B))))

;;;;;;;;;;;;
;;;;;;;;;;;;
;;;;;;;;;;;;
;; Mapping function! It maps the genome into a set of stocks.
;;;;;;;;;;;;
;;;;;;;;;;;;
;;;;;;;;;;;;

(defn map-bit-array [bitarray]
  "Mapping function. Given an bitarray maps into a stock portfolio"
  (r/fold
    into-improved
    (fn ([] [])
      ([acc x] (if (= 0 (get-bit bitarray x))
                 acc
                 (conj acc (nth stock-list x))
                 )))
    (range (get bitarray :bits)))
  )

;;;;;;;;;;;;
;;;;;;;;;;;;
;;;;;;;;;;;;
;; First Fitness function
;;;;;;;;;;;;
;;;;;;;;;;;;
;;;;;;;;;;;;


(defn calculate-price-and-estability
  ([] {:total-price 0, :estability 0})
  ([{total-price :total-price estability :estability} x]
   (assoc
     (assoc {} :total-price (+ total-price (get-price x)))
     :estability
     (+ estability
        (calculate-estability x)))))

(defn fitness [stocks]
  (let [result (r/fold
                 sum-price-and-estability
                 calculate-price-and-estability
                 stocks)
        total-cost (get result :total-price)
        estability (get result :estability)]

    (if (< total-budget total-cost)
      0
      estability
      )
    )
  )
;;;;;;;;;;;;
;;;;;;;;;;;;
;;;;;;;;;;;;
;; Second Fitness function
;;;;;;;;;;;;
;;;;;;;;;;;;
;;;;;;;;;;;;


(defn sum-price-and-risk
  ([] {:total-price 0, :risk 0})
  ([{total-price-A :total-price risk-A :risk}
    {total-price-B :total-price risk-B :risk}]

   (assoc
     (assoc {} :total-price (+ total-price-A total-price-B))
     :risk
     (+ risk-A
        risk-B))))

(defn calculate-price-and-risk
  ([] {:total-price 0, :risk 0})
  ([{total-price :total-price risk :risk} x]
   (assoc
     (assoc {} :total-price (+ total-price (get-price x)))
     :risk
     (+ risk
        (calculate-risk x)))))

(defn fitness-with-price [stocks]
  (let [result (r/fold
                 sum-price-and-risk
                 calculate-price-and-risk
                 stocks)
        total-cost (get result :total-price)
        risk (get result :risk)]

    (if (< total-budget total-cost)
      0
      ($= total-cost - risk)
      )
    )
  )
;;;;;;;;;;;;
;;;;;;;;;;;;
;;;;;;;;;;;;
;; Charts
;;;;;;;;;;;;
;;;;;;;;;;;;
;;;;;;;;;;;;

(defn plot-individuals [[size-of-population
                         mutation-rate
                         crossover-rate
                         generations]
                        good-individuals bad-individuals]
  "Saves a plot with the best and worst indivuduals from a generation"
  (let [title
        (format "%d individuals - Mutation rate: %.2f - Crossover rate %.2f - Generations %d"
                size-of-population mutation-rate crossover-rate generations)
        a @chart]
    (save (line-chart
            (into (apply vector (map first good-individuals))
                  (apply vector (map first bad-individuals)))
            (into (apply vector (map second good-individuals))
                  (apply vector (map second bad-individuals)))
            :group-by (into (apply vector (take (length good-individuals)
                                                (cycle ["Best Individual"])))
                            (apply vector (take (length bad-individuals)
                                                (cycle ["Worst Individual"]))))
            :title title
            :x-label "Generations"
            :y-label "Fitness Value"
            :legend true
            )
          (format "%d.png" a)
          )
    (swap! chart inc)

    )
  )


;;;;;;;;;;;;
;;;;;;;;;;;;
;;;;;;;;;;;;
;; Main function
;;;;;;;;;;;;
;;;;;;;;;;;;
;;;;;;;;;;;;
(def params [[100 0.2 0.7 100]
             [100 0.2 0.9 100]
             [100 0.5 0.7 100]
             [100 0.8 0.7 100]
             [100 0.2 0.7 300]
             [100 0.2 0.9 300]
             [300 0.2 0.7 100]
             [300 0.2 0.7 300]])

(defn -main
  "Main function. It is possible use as arguments a list with the genetic algorithm parameters. If such list is empty, use the parameters above."
  [& args]
  (let [parameters (if (empty? args) params args)
        sample-results-fitness-with-price (reduce (fn [acc [population mutation crossover generations]]
                                                    (conj acc
                                                          [[population mutation crossover generations]
                                                           (genetic-alg population
                                                                        mutation
                                                                        crossover
                                                                        (length stock-list)
                                                                        map-bit-array
                                                                        fitness-with-price
                                                                        generations plot-individuals)]))
                                                  []
                                                  parameters
                                                  )
        sample-results-fitness-simple (reduce (fn [acc [population mutation crossover generations]]
                                                (conj acc
                                                      [[population mutation crossover generations]
                                                       (genetic-alg population
                                                                    mutation
                                                                    crossover
                                                                    (length stock-list)
                                                                    map-bit-array
                                                                    fitness
                                                                    generations
                                                                    plot-individuals)]))
                                              []
                                              parameters
                                              )]
    (reduce (fn [_ [params result]]
              (do
                (println params result)
                (println (fitness-with-price result))
                (println (r/fold
                           sum-price-and-risk
                           calculate-price-and-risk
                           result))
                ))
            []
            sample-results-fitness-with-price)

    (reduce (fn [_ [params result]]
              (do
                (println params result)
                (println (fitness result))
                (println (r/fold
                           sum-price-and-estability
                           calculate-price-and-estability
                           result))
                ))
            []  sample-results-fitness-simple)

    )
  )
