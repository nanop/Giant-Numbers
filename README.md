Giant-Numbers-FOSS
==================

From Google Code: tree-based number representation for gigantic numbers and efficient arithmetic

This Scala package tests out ideas about a tree-based number representation that can host gigantic numbers, for instance "towers of exponents" and sparse combinations of them, in a compressed form, while being able to perform arrithmetic operations on them, efficently.

It also provides compact representation for numbers in their close neighborhoud like Mersenne and Fermat numbers as well as numbers derived from them like the "perfect numbers", equal to the sum of their proper divisors. For instance, the largest known prime number (at the end of year 2012), the Mersenne prime

2^43112609 − 1

is represented as a small tree, in contrast with the millions of digits needed by its conventional representation.

Towards this end, we are using a "run-length" compressed bijective base-2 representation for both 0 and 1 symbols.

We see adding a 0 (represented as O) and 1 (represented as I) as the application of a function. Numbers in bijective base 2 can be seen as finite strings made of the symbols O and I. O and I are built as generalized "constructors/extractors" using Scala's apply/unapply methods. The key intuition is that each application O(x) corresponds to the function x->2x+1 and each application I(x) to the function x->2x+2. Our representation can be seen as describing natural numbers in terms of itarated applications of these functions. In terms of the data type

AlgT = T | V AlgT AlgT? | W AlgT AlgT?

    - T represents the empty sequence 

    - V x xs represents an alternation of Os and Is where the first is an O, such x and each element of xs are counters initially T 

    - W x xs represents an alternation of Is and Os where the first is an I, such x and each element of xs are counters intially T 

The "counters" are recursively represented the same way, such that the resulting trees/DAGs have internal nodes of type V or W and leaves of type T.

We first implement various arithmetic operations on unbounded natural numbers and then derive, using their Calkin-Wilf bijection to rational numbers, and an extension to signed rationals, a possibly practical complete set of operations on them.

The bijection with naturals ensures that rational numbers are stored at their information theoretical miniumum.

The main open question is if, by using our representations, there are significantly faster algorithms for some interesting arithmetic functions and if interesting numbers or sequences have significantly more compact forms. 
Type "gcompile" to compile the files using the "scalac" compiler (available from typesafe.com or scala-lang.org).
That should create directory bin where class files are placed.

Type "nrun" to run the tests in file Main.

To try out other examples edit Main.scala, recompile with "ncompile" and test with "nrun".

The directory "doc" contains the scaladoc generated documentation - but you will need to take a look at the source in directory "src" for any meaningful details on various
algorithms.

The code has been tested on a Mac with OS X Lion using Scala version 2.9.2.

Enjoy,

Paul Tarau
http://www.cse.unt.edu/~tarau/
