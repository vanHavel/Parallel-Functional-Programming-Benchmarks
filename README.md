# Parallel Functional Programming Benchmarks
Language Comparison on absolute speeds and relative speedups.

## Introduction
In this project we compare the functional programming languages Haskell, Erlang and Java 8 in terms of performance on parallel computations.
Our goal is to compare both raw speeds as well as **speedup** (scaling factor compared to a sequential solution) of all languages involved. Depending on the environment, the latter one might be even more important than the first, as machines with many cores need excellent scalability to yield good performance.
One could probably argue extensively whether these benchmarks lead to a _fair_ comparison of the languages' performance, or about what a _fair_ comparison even means.
We dot not strive to find the fastest implementation possible for each problem, as this would for sure entail a lot of hacking and caring about low level details. Instead, we will compare implementations that are simple and idiomatic for the respective languages, and probably close to the first implementation a programmer would come up with, before performing any advanced optimization. This also means that we spend a comparable amount of time for implenting the algorithms in each language, which forms our definition of a somewhat _fair_ comparison. 
Still, implementing parallel functional algorithms in Java using streams is very different from Haskell and Erlang, as the programmer basically only has access to a high level library, while the control in the other langauges is much more fine-grained. We will continue this discussion in the final section.

## The Competitors
The languages considered are Haskell, Erlang and Java. Now, each of these languages contains several different concepts of parallelism, and we should start by clearing up which ones we are using for this comparison. 
* For the case of Haskell, we use Simon Marlows par monad [[1]](http://dl.acm.org/citation.cfm?id=2034685). Of the many different concepts to express deterministic parallel computations in Haskell, we feel that the par monad is the easiest to use, as it avoids the pitfalls that occur when parallel computations clash with Haskell's lazy evaluation. Further, from our experience the par monad usually leads to better results in benchmarks from the get go, i.e. without excessive parameter tuning. However, one could argue that using strategies is a more idiomatic way to express parallel functional programs, as they separate more clearly the algorithm from the evaluation strategy.
* In Erlang there is really only one notion of parallelism: processes. But these can be run on the same node or on different nodes. As we benchmark parallel computations on a single machine, we only make use of intra-node parallelism. Of course one of the biggest advantages of Erlang is that it was built to scale for distributed computations, but that is not the point of this experiment.
* Finally, we use parallel streams for parallelism in Java (and sequential streams as a baseline). We limit ourselves to streams, as they are the only way to write idiomatic, parallel, functional code in Java. In particular we ignore all parallelism means using threads or likewise constructs.

## Cluster Assignment
Our first benchmark is an _embarassingly parallel_ problem: we try to parallelize a simple map function. In particular, we take a look at the **cluster assignment** step of the popular **k-means** clustering algorithm. The concept is simple: we are given a set of **n** points in a plane, and a set of **k** _cluster centres_ (**k** << **n**), and we simply assign each point to its nearest center. 
TODO: pic
In Haskell and Erlang we split the list of data points up into chunks, and compute the nearest centers for each chunk in parallel. In Java, we simply invoke the `map` method on a parallel stream of the data points. Internally, this will process the stream in a similar strategy as our implementations of the other languages. [[2]](https://docs.oracle.com/javase/tutorial/collections/streams/parallelism.html#executing_streams_in_parallel)

### Results
TODO

## Parallel Sorting
The next benchmark we will consider is the (in our opinion) most idiomatic implementation of a parallel, functional, generic sorting algorithm in each of the languages. As a baseline we compare this to an idiomatic sequential sorting algorithm.
For Haskell and Erlang, we implemented a basic mergesort algorithm, where at first both halves of the lists are recursively sorted in parallel, and then the two sorted sublists are merged sequentially. The granularity is controlled by a _cutoff_ parameter **k**: lists of length shorter than **k** are sorted by a sequential mergesort implementation. 
For Java, we simply invoke the `sorted()` function of Java's (parallel) streams, which internally also operates using parallel mergesort with a _cutoff_ parameter, as it delegates to `Arrays.parallelSort()`[[3]](https://docs.oracle.com/javase/8/docs/api/java/util/Arrays.html#parallelSort-T:A-).

### Results
TODO

## Bitonic Sorting
TODO

### Results
TODO

## Discussion

## References
1. Marlow, Simon, Ryan Newton, and Simon Peyton Jones. "A monad for deterministic parallelism." ACM SIGPLAN Notices. Vol. 46. No. 12. ACM, 2011.
2. "Executing Streams in Parallel" https://docs.oracle.com/javase/tutorial/collections/streams/parallelism.html#executing_streams_in_parallel
3. "Arrays (Java Platform SE 8)" https://docs.oracle.com/javase/8/docs/api/java/util/Arrays.html#parallelSort-T:A-
