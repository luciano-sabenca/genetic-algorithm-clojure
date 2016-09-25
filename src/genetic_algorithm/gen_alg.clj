(ns genetic_algorithm.gen-alg
  (:require [genetic_algorithm.bitarray :refer :all]
            [clojure.core.reducers :as r]

            ))
; Luciano P. Sabenca
;
; This namespace implements - in a generic way - the basic functionalities of a genetic algorithm
; Not meant to be used in real env - Just academic purpose


(defn crossover [father mother]
  "Given two cromossomes, it returns the two others that are the crossover between the given ones."
  (let [size-of-cromossome (get father :bits)
        crossover-point (rand-int size-of-cromossome)
        a (copy-of father)
        b (copy-of mother)]

    (reduce (fn [[individualA individualB] x]
              (do
                (set-bit! individualA x (get-bit mother x))
                (set-bit! individualB x (get-bit father x)))
              [individualA individualB])
            [a b]
            (range crossover-point))
    )
  )



(defn mutate [individual mutation-rate]
  "Implements the mutation of an individual"
  (let [bits-number (get individual :bits)
        return (bit-array bits-number)]
    (reduce (fn [return x]
              (do
                (set-bit! return x (get-bit individual x))
                (when (<= (rand) mutation-rate)
                  (flip-bit! return x)
                  )
                return
                ))
            return
            (range bits-number))

    ))

(defn roulette [population-with-fitness sum]
  "Implements the roullette selection"
  (loop [n (* (rand) sum)
         [[fit current-guy] & cdr] population-with-fitness
         pop []]
    (if (or (<= n 0) (empty? current-guy))
      pop
      (recur (- n fit)
             cdr
             (conj pop current-guy))
      )

    ))


;;;
;;; Initial Population
;;;

(defn generate-random-individual [cromossome-size]
  (let [individual (bit-array cromossome-size)]
    (dotimes [n cromossome-size]
      (set-bit! individual n (rand-int 2)))
    individual
    )
  )

(defn generate-initial-population [size-of-population cromossome-size]
  (reduce (fn [acc _]
            (conj acc (generate-random-individual cromossome-size)))
          []
          (range size-of-population))

  )

;;;
;;; Next population
;;;


(defn generate-new-population [size-of-population selecteds crossover-rate mutation-rate]
  (take size-of-population
        (reduce
          (fn
            [res [father mother]]
            (if (< (rand) crossover-rate)
              (into res (crossover father mother))
              (conj res (mutate father mutation-rate))

              ))
          (set selecteds)
          (take size-of-population (partition 2 (cycle selecteds))))))






;;;
;;; Main Genetic function
;;;

(defn genetic-alg
 "Main function. It receives as parameter the size-of-population, its mutation rate and crossover rate. It receives also the size of cromossome a mapping function, a fitness function and number of generations. It might receive also some functions to plot the system's evolution."
  ([size-of-population mutation-rate
    crossover-rate cromossome-size
    map-func fitness-func generations]
   (genetic-alg size-of-population
                mutation-rate
                crossover-rate
                cromossome-size
                map-func
                fitness-func
                generations
                nil))
  ([size-of-population mutation-rate
    crossover-rate cromossome-size
    map-func fitness-func
    generations
    plot-graphs]
   (loop [population
          (generate-initial-population size-of-population cromossome-size)
          n 0
          best-individuals-fitness []
          worst-individual-fitness []
          best-individual [Integer/MIN_VALUE []]]
     (if (> n generations)
       (do
         (when (not (nil? plot-graphs))
           (plot-graphs [size-of-population
                         mutation-rate
                         crossover-rate
                         generations]
                        best-individuals-fitness
                        worst-individual-fitness)
           )

         (map-func (second best-individual)))
       (let [fitness-values (into [] (r/map (fn [x] [(fitness-func (map-func x)) x]) population)) ;calculate fitness value
             sum-fitness (r/fold + (fn ([] 0) ([x y] + x (first y))) fitness-values) ; sum fitness
             sorted-pop (reverse (sort-by first (filter #(< 0 (first %1)) fitness-values)))
             selecteds (roulette sorted-pop sum-fitness)    ; select some
             new-population (if (empty? selecteds)
                              (generate-new-population size-of-population (reduce #(conj %1 (second %2)) [] sorted-pop) crossover-rate mutation-rate)
                              (generate-new-population size-of-population selecteds crossover-rate mutation-rate))
             best-individual (if (> (first (first sorted-pop)) (first best-individual)) (first sorted-pop) best-individual)]
         (do
           (println (format "Generation %d. Best individual fitness %s" n (first best-individual)))
           (recur new-population
                  (inc n)
                  (conj best-individuals-fitness [n (first (first sorted-pop))])
                  (conj worst-individual-fitness [n (first (last sorted-pop))])
                  best-individual)
           )

         )

       )
     ))
  )

