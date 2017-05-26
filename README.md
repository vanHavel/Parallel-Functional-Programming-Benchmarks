# Parallel Functional Programming Benchmarks
Language Comparison on absolute speeds and relative speedups.

## Introduction
In this project we compare the functional programming languages Haskell, Erlang and Java 8 in terms of performance on parallel computations. All implementations are available in this repository.

Our goal is to compare both raw speeds as well as **speedup** (scaling factor compared to a sequential solution) of all languages involved. Depending on the environment, the latter one might be even more important than the first, as machines with many cores need excellent scalability to yield good performance.

One could probably argue extensively whether these benchmarks lead to a _fair_ comparison of the languages' performance, or about what a _fair_ comparison even means.

We dot not strive to find the fastest implementation possible for each problem, as this would for sure entail a lot of hacking and caring about low level details. Instead, we will compare implementations that are simple and idiomatic for the respective languages, and probably close to the first implementation a programmer would come up with, before performing any advanced optimization. This also means that we spend a comparable amount of time for implenting the algorithms in each language, which forms our definition of a somewhat _fair_ comparison. 

Still, implementing parallel functional algorithms in Java using streams is very different from Haskell and Erlang, as the programmer basically only has access to a high level library, while the control in the other langauges is much more fine-grained. We will continue this discussion in the final section.

## The Competitors
The languages considered are Haskell, Erlang and Java. Now, each of these languages contains several different concepts of parallelism, and we should start by clearing up which ones we are using for this comparison. 
* For the case of **Haskell**, we use Simon Marlows par monad [[1]](http://dl.acm.org/citation.cfm?id=2034685). Of the many different concepts to express deterministic parallel computations in Haskell, we feel that the par monad is the easiest to use, as it avoids the pitfalls that occur when parallel computations clash with Haskell's lazy evaluation. Further, from our experience the par monad usually leads to better results in benchmarks from the get go, i.e. without excessive parameter tuning. However, one could argue that using strategies is a more idiomatic way to express parallel functional programs, as they separate more clearly the algorithm from the evaluation strategy.
* In **Erlang** there is really only one notion of parallelism: processes. But these can be run on the same node or on different nodes. As we benchmark parallel computations on a single machine, we only make use of intra-node parallelism. Of course one of the biggest advantages of Erlang is that it was built to scale for distributed computations, but that is not the point of this experiment.
* Finally, we use parallel streams for parallelism in **Java 8** (and sequential streams as a baseline). We limit ourselves to streams, as they are the only way to write idiomatic, parallel, functional code in Java. In particular we ignore all parallelism means using threads or likewise constructs.

## The Testing Environment
The experiments were conducted on a 2016 MacBookPro with 8GB RAM running OS X 10.12.4. The processor is a dual-core Intel i5-6360U with 2.9 GHz and hyper-threading (4 threads). We compared java version 1.8.0_121, ghc version 8.0.1 and BEAM version 8.3.

## Benchmark I: Cluster Assignment
Our first benchmark is an _embarassingly parallel_ problem: we try to parallelize a simple map function. In particular, we take a look at the **cluster assignment** step of the popular **k-means** clustering algorithm. The concept is simple: we are given a set of **n** points in a plane, and a set of **k** _cluster centres_ (**k** << **n**), and we simply assign each point to its nearest center. 

![kmeans](https://github.com/vanHavel/Parallel-Functional-Programming-Benchmarks/blob/master/kmeans.jpg)

In Haskell and Erlang we split the list of data points up into chunks, and compute the nearest centers for each chunk in parallel. In Java, we simply invoke the `map` method on a parallel stream of the data points. Internally, this will process the stream in a similar strategy as our implementations of the other languages. [[2]](https://docs.oracle.com/javase/tutorial/collections/streams/parallelism.html#executing_streams_in_parallel)

## Benchmark II: Parallel Sorting
The next benchmark we will consider is the (in our opinion) most idiomatic implementation of a parallel, functional, generic sorting algorithm in each of the languages. As a baseline we compare this to an idiomatic sequential sorting algorithm.

For Haskell and Erlang, we implemented a basic mergesort algorithm, where at first both halves of the lists are recursively sorted in parallel, and then the two sorted sublists are merged sequentially. The granularity is controlled by a _cutoff_ parameter **k**: lists of length shorter than **k** are sorted by a sequential mergesort implementation. 

For Java, we simply invoke the `sorted()` function of Java's (parallel) streams, which internally also operates using parallel mergesort with a _cutoff_ parameter, as it delegates to `Arrays.parallelSort()`[[3]](https://docs.oracle.com/javase/8/docs/api/java/util/Arrays.html#parallelSort-T:A-).

## Bonus Benchmark: Bitonic Sorting
A somewhat more sophisticated parallel sorting algorithm is _bitonic mergesort_[[4]](). In this algorithm, not only the sorting, but also the merging step is parallelized, splitting up the linear time sequential work of the merge. As such, massively parallel architectures can get better speedups than on sequential mergesort. The downside is that the total number of operations is higher than in usual mergesort (although the asymptotic complexity is the same). The algorithm is studied in detail in the reference [[4]](http://www.iti.fh-flensburg.de/lang/algorithmen/sortieren/bitonic/bitonicen.htm).

![bitonic](https://github.com/vanHavel/Parallel-Functional-Programming-Benchmarks/blob/master/binetzen.gif)

We implemented this algorithm in Haskell and Erlang only. The reason is that we do not believe that it is possible to implement this algorithm elegantly using Java parallel streams. One could probably do achieve this more easily using another, non-functional concept of parallelism in Java, but this is outside the scope of this experiment.

## Results
All times are the in given in miliseconds. The times are average runtimes of at least 10 executions for each parallel benchmark. The speedup is in relation to a baseline sequential version of the same algorithm.

For the sorting problems, the task at hand was sorting **n** = 2^17 numbers. In the Haskell and Erlang implementations we used a chunksize of 2^12. 

For the cluster assignment problem, we assigned **n** = 2^20 points to **k** = 10 different clusters. In Haskell and Erlang, the chosen chunksize was 2^15.

. | Haskell | Erlang | Java
---|---:|---:|---:
Kmeans time | 996.2 | 1425.667 | 177.227
Kmeans speedup | 1.062 | 4.843 | 2.147
Mergesort time | 163.2 | 65.695 | 31.796
Mergesort speedup | 2.409 | 1.668629 | 1.594
Bitonic sort time | 1491 | 360.185 | N.A.
Bitonic sort speedup | 1.635 | 4.734 | N.A.

## Discussion
Concerning raw speed, we have a clear winner: Java greatly outperforms both Haskell and Erlang on the kmeans example by being more than 5 times as fast, and is still twice as fast as Erlang in the sorting task. 

Haskell seems to have quite a few problems with spending too much time on garbage collection. By playing around with the runtime flags a bit and assigning a large nursery size of 100MB to each HEC, the Haskell programs run 1.5-2 times as fast, but still do not come close to the Erlang performance of sorting. For some reason the kmeans example runs somewhat faster in Haskell than in Erlang. However, all in all the performance of the only compiled language in this comparison is somewhat disappointing.

Concerning speedup / scalability, Erlang seems to be the winner in the competition. In two tasks it achieves greater than quadruple speedup, which is impressive given the test machine's processor. Erlang undermines its claim of having chosen "the right concurrency model". In the simple mergesort the speedup is not quite as good as Haskells Par monad, but still better than Java. 

Haskell speedups show mixed results: from almost no gain in kmeans to a unexpectedly great performance on parallel mergesort. This somewhat coincides with our previous experience of writing parallel Haskell: achieving speedups is tricky and rarely works "out of the box". 

During implementation, we found that working with Java was quite different from working with Haskell or Erlang. On the one hand, the parallel stream operations are very easy to use, and given this simplicity the speedups achieved wothout any further tweaking are impressive. On the other hand, it is harder to obtain a more finegrained control of parallelism - in a real life application, one might not want to throw all of a System's resources at each computation of a parallel stream. Further, Java is really missing some convenience functions for streams such as `zip` or `mapWithIndex` which e.g. made the kmeans `closest` implementation a bit more tedious than necessary. As streams and high level functional concepts are still a relatively new addition to Java, we believe that future improvenments could mitigate some of these issues.

## References
1. Marlow, Simon, Ryan Newton, and Simon Peyton Jones. "A monad for deterministic parallelism." ACM SIGPLAN Notices. Vol. 46. No. 12. ACM, 2011.
2. "Executing Streams in Parallel" https://docs.oracle.com/javase/tutorial/collections/streams/parallelism.html#executing_streams_in_parallel
3. "Arrays (Java Platform SE 8)" https://docs.oracle.com/javase/8/docs/api/java/util/Arrays.html#parallelSort-T:A-
4. "Bitonic Sort" http://www.iti.fh-flensburg.de/lang/algorithmen/sortieren/bitonic/bitonicen.htm

Images were taken from:

kmeans: https://www-m9.ma.tum.de/material/felix-klein/clustering/Methoden/kmeans_schlechte_anfangszentren.jpg

bitonic sort: http://www.iti.fh-flensburg.de/lang/algorithmen/sortieren/bitonic/bitonicen.htm
