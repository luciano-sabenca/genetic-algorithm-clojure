# Genetic Algorithm in Clojure

A very simple implementation of a genetic algorithm in Clojure.
It was created for academic purpose only, so it was not designed to have a great performance.

The implementation is under the namespace `genetic_algorithm`. 
You can see a simple demo at `demo` folder.



## Demo

The demo is a stock recomendation problem: given a limited budget, how can I invest it on stocks for long-term (i.e. stable stocks)?

The data used to simulate is from S&P ranking from 2010 and the benchmarks and stability metrics were created just to test the algorithm.

You can find an academic report about this implementation and demo on "relatorio.pdf" **(portuguese only!!)**


## Execution 

This project uses Leinigen, so it is very recommended its use. 
To run a series of basic examples just run:
`lein run`


To run with custom parameters you should run the following commands:


`lein repl`

And

`(-main [100 0.2 0.8 10])`


The first parameter is the size of population, the second is the mutation rate, the third is the crossover rate and the last ont the population' size.

## Implementation by

Luciano P. Sabenca (lucianosabenca at gmail.com) - https://linkedin.com/in/luciano-sabença-9162b149 

It was an done as a academic undergraduate assignment at University of Campinas (UNICAMP) 


