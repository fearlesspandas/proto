Typical is a series Experiments in typesafe data processing using ideas pulled 
from category theory, distributed logics/TLA+ , boolean algebras and contextual computing.
Various examples using Typical can be found in the Test.scala files across the various
branches of this repo



## -----------Building Typical-----------------
To build and use Typical in another project, currently you must clone this repo,
into a subdirectory of your existing project. If you just want to try out Typical,
you can clone this repository and start modifying the Test.scala file in the root
folder, or build your own main class (see Test.scala as an example). To build Typical
you should have the following installed:

    - sbt
    - scala 2.12.x


## -----------Typical Use Case---------------------
Typical is designed to enforce type safety across
all levels of data processing, and will do so unless
it's paradigms are heavily violated. It's primary way
of achieving this is by forcing the programmer to
specify transformations on types bound to data in an
unambiguous manner. 

The style in which these transformations
are defined is very similar to how one would define a TLA spec,
or implement a state monad, where all valid transformations
defined in typical essentially take the form of a map CTX => next T
that defines what the next valid state of a type T would be, given
some valid context.

The benefits of enforcing this structure on transformations are as follows:

    1.Transformations are Minimal in their definitions.
    2.Transformations are highly composable
    3.Transformations are verifiable at compile time 


Most notably point 3 is often an issue when refactoring monumental Spark jobsfor example
with unclear relationships between columns/dataframes. Typical makes
it easy to take existing structures that normally have no inherrent notion
of type at compile time (like a spark dataframe column that only gets typechecked at
run time), and wrap typesafe structure around it with no loss in throughput.
This is possible because Typical itself is not in any way an engine for
data processing. You as the programmer still have all the control
over how data is processed, using whatever toolkit you'd like. What Typical
does instead is wrap your data transformations in a grammar that will enforce type
safe processing at compile time. That means, for example, if you defined a spark
job within Typical, at compile time you'll see issues with data dependencies, but
at run time spark will still be doing all of the data processing as it normally
would if not using Typical.


## ----High-Level Overview of Typical data flows----
Typical is designed to easily build up stateful behavior in a clean
encapsulated way without side effects. To do this, in general the process
is to start with some initial dataset 'dat', run a calculation on it to produce new data,
then encapsulate those results in an updated version of dat. Those results are then propagated
forward to later calculations like so

```
    dat ----some calc ----> dat2 ------some calc ----> dat3 ...
```   
 
Typical can verify at compile time for every calculation step all the data dependencies for
that calculation are met.

The programmer has two main jobs when using typical. Binding data to types, and defining the
calculation functions. Those are discussed in more detail in the next section, and more complex
examples can be found throughout this repo (see see Test.scala or KnapSack/Orders packages).

It should be noted that because of how state encapsulation works, the way Typical builds
state through the chaining described above, is immediately tail recursive in nature. In
general this is a useful mental model to describe what a chain of Typical calculations is doing.
Like tail recursion, Typicals calculations reduce to running the programmer-defined calculations
on a previous iteration of data, then (possibly) updating some of that data in place. In further
calculations down the chain, the updated data will be available and feed into future results.

The other piece of the puzzle Typical handles for us, is verifying whether we can actually
execute a particular calculation on a given dataset. What that means is, loosely speaking,
 if we had a calculation defined as follows
 
 ```
        dataset__A -----f:Calc_With_dependency_X ----> dataset_B
 ```

Where we're transforming dataset_A into dataset_B through f, where f has a dependency on some data X.
Suppose then we had a concrete dataset 'dat' which does not contain data for X. Then if we try to transform dat through f, we will get a compile time error.


## --------Getting Started---------------
Typical uses 3 main structures to define it's calculations.
They are: dataset ,axiom and model. These types in their full functionality
are imported like so:

```

import Typical.core._
import grammar._
import dataset._

```
### Dataset-------
```dataset[+A<:dataset[_]]``` is the foundational type for all structure defined in typical. ```dataset``` is
little more than a monadic container, where the bounds on its type parameter amount to the following behavior:
if ```A:dataset[_]``` and ```B:dataset[_]``` then ```A with B:dataset[_]```. This means intuitively that a
```dataset[A with B with C]``` can be thought of as representing the lattice created by the base types ```A,B,C```.


### Axioms----------
An 
```
axiom[+A<:axiom[A]] extends dataset[A]
```
 is a typical managed type that requires no processing.
They're a generally convenient way of throwing any base data into typical to be referenced by other
types within typical transformations.


```
class intData(value:Int) extends axiom[myaxiom]
```

```
class stringData(value:String) extends axiom[otheraxiom]
```

### Models------------
In contrast to axioms which require no processing, we also have 
```
model[-A<:dataset[_],+B<:dataset[_]] extends dataset[_]
```
which defines transformation from dataset ````A```` to dataset ```B```. Recall that because datasets can be composed through
the ```with``` keyword, either ```A``` or ```B``` can be of the form ```X with Y with Z``` where ```X,Y,Z :dataset[_]```

some examples of valid models would then include
```
//a model that defines how to transform/produce itself
class TimesTwo(value:Int) extends model[intData,TimesTwo]{
   override def iterate(src:dataset[intData]]):dataset[TimesTwo] = for{
        currInt <- src.fetch[intData] //fetch is part of the Typical transformation grammar
    }yield TimesTwo(currInt.value * 2)
}
```

```
//also a model that produces itself, with multiple dependencies
class MakeAString(value:String) extends model[intData with stringData,MakeAString]{
    override def iterate(src:dataset[intData with stringData]):dataset[MakeAString] = for {
        currInt <- src.fetch[intData] //fetch is part of the Typical transformation grammar
        currString <- src.fetch[stringData]
    }yield MakeAString(currString.value + currInt.value.toString)
}
```

```
//a model with multiple dependencies, that transforms multiple other types/values
class UpdateMultipleValues extends model[intData with stringData,MakeAString with stringData]{
    override def iterate(src:dataset[intData with stringData]):dataset[MakeAString with stringData] = for{
        newctx <- src.calc[MakeAString] //calc is part of the Typical transformation grammar
        currString <- src.fetch[stringData]
    }yield newctx.include[stringData](currString.value + "New stuff")
}
```

### Running our calculations through chaining----------------
We've already seen two operations from the Typical transformation grammar in our above examples, those
being ```fetch``` and ```calc```. As demonstrated above, Typical's transformation operators are designed
to allow you to access/modify/run your Typical structures from a context, while being amenable to standard
monadic control flows such as for comprehensions. Here we provide a bit more granularity on what some of
the (most fundamental) operations are doing. 

While this may not be a fully exhaustive list of operations defined in the Typical grammar, these are
pretty fundamental, and overlap with concepts from other frameworks built around the same contextual 
computing problem space, such as ZIO.

#### Operations:
Suppose that 
 ```
dat:dataset[CTX<:dataset[_]]
```
then we have the following behavior

    - fetch
        dat.fetch[A<:dataset[_]]
            returns dataset A with the context of dat. If dat is not of type dataset[A], i.e. dat does not contain A,
            you will see a compile time error.
     -derive
        dat.derive[A<:model[CTX,A]]
        returns a dataset[A] calculated using the values from dat, and A's iterate method. Essentially
        A.iterate(dat). If dat does not have the required dependencies for A, you will see a compile time error
    - calc
        dat.calc[A<:model[CTX,A]]
            returns a dataset[CTX with A]. 
             If dat does not have the dependencys required for A's transformation
            you will see a compile time error
     - run
        dat.run[A<:model[CTX,_]]
            returns a dataset[CTX] where all values produced in the output of A's transformation are updated.
            If dat does not have the required dependencies, or A's output produces a bigger context type than CTX
            you will see a compile time error.
    - include
        dat.include[A<:dataset[_]](a:A)
            returns a dataset[CTX with A] where A's contextual value becomes the passed in value a. If A is already
            within the CTX lattice then you will still get a dataset[CTX] back with A's value updated.
        