# Parallel Functional Programming Benchmarks
Language Comparison on absolute speeds and relative speedups.

## Introduction
TODO

## The Competitors
The languages considered are Haskell, Erlang and Java. Now, each of these languages contains several different concepts of parallelism, and we should start by clearing up which ones we are using for this comparison. 
* For the case of Haskell, we use Simon Marlows par monad [[1]](http://dl.acm.org/citation.cfm?id=2034685). Of the many different concepts to express deterministic parallel computations in Haskell, we feel that the par monad is the easiest to use, as it avoids the pitfalls that occur when parallel computations clash with Haskell's lazy evaluation. Further, from our experience the par monad usually leads to better results in benchmarks from the get go, i.e. without excessive parameter tuning. However, one could argue that using strategies is a more idiomatic way to express parallel functional programs, as they separate more clearly the algorithm from the evaluation strategy.
* In Erlang there is really only one notion of parallelism: processes. But these can be run on the same node or on different nodes. As we benchmark parallel computations on a single machine, we only make use of intra-node parallelism. Of course one of the biggest advantages of Erlang is that it was built to scale for distributed computations, but that is not the point of this experiment.
* Finally, we use parallel streams for parallelism in Java (and sequential streams as a baseline). We limit ourselves to streams, as they are the only way to write idiomatic, parallel, functional code in Java. In particular we ignore all parallelism means using threads or likewise constructs.

## Parallel Sorting
The first benchmark we will consider is the (in our opinion) most idiomatic implementation of a parallel, functional, generic sorting algorithm in each of the languages. As a baseline we compare this to an idiomatic sequential sorting algorithm.
For Haskell and Erlang, we implemented a basic mergesort algorithm, where at first both halves of the lists are recursively sorted in parallel, and then the two sorted sublists are merged sequentially. The granularity is controlled by a _cutoff_ parameter **k**: lists of length shorter than **k** are sorted by a sequential mergesort implementation. 
For Java, we simply invoke the `sorted()` function of Java's (parallel) streams, which internally also operates using parallel mergesort with a _cutoff_ parameter, as it delegates to `Arrays.parallelSort()`[[2]](https://docs.oracle.com/javase/8/docs/api/java/util/Arrays.html#parallelSort-T:A-).

### Results
TODO

## Another Problem
TODO

## References
1. Marlow, Simon, Ryan Newton, and Simon Peyton Jones. "A monad for deterministic parallelism." ACM SIGPLAN Notices. Vol. 46. No. 12. ACM, 2011.
2. "Arrays (Java Platform SE 8)" https://docs.oracle.com/javase/8/docs/api/java/util/Arrays.html#parallelSort-T:A-
