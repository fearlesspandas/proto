Typical is a series Experiments in typesafe data processing using category theory.
Various examples using Typical can be found in the Test.scala files across the various
branches of this repo



## -----------Building Typical-----------------
To build and use Typical in another project, currently you must clone this repo,
into a subdirectory of your existing project. If you just want to try out Typical,
you can clone this repository and start modifying the Test.scala file in the root
folder, or build your own main class (see Test.scala as an example). To build Typical
you should have the following installed:
    -sbt
    -scala 2.12.x


## -----------Typical Use Case---------------------
Typical is designed to enforce type safety across
all levels of data processing, and will do so unless
it's paradigms are heavily violated. It's primary way
of achieving this is by forcing the programmer to
specify transformations on types bound to data in an
unambiguous manner.

If in these transformations, type structure is not respected
and produces side effects, or becomes ambiguous, Typical will
not allow the programmer to invoke these transformations
in downstream processes. In practice what that amounts to
is, if a calculation/transformation of data has dependencies
on other data but that data is not guaranteed to be present,
you will see these missing dependencies in your data transformation
at compile time, where normally they would appear as a runtime error
in most existing frameworks.

Most notably this is often an issue when refactoring monumental Spark jobs
with unclear relationships between columns/dataframes. Typical makes
it easy to take existing structures that normally have no inherrent notion
of type at compile time (like a spark column that only gets typechecked at
run time), and wrap typesafe structure around it with no loss in throughput.
This is possible because Typical itself is not in any way an engine for
data processing. You as the programmer still have all the control
over how data is processed, using whatever toolkit you'd like. What Typical
does instead is wrap your data transformations in a grammar that will enforce type
safe processing at compile time. That means, for example, if you defined a spark
job within Typical, at compile time you'll see issues with data dependencies, but
at run time spark will still be doing all of the data processing as it normally
would if not using Typical.


It should be noted that typesafe processing in the real world, has both advantages
and disadvantages for the lay-programmer, which should be weighed carefully

### Advantages:
    * near complete reduction of runtime errors
    * rigorous application structure with no side effects
    * complete encapsulation of state
    * extremely modular application structure which detects
        inconsistencies at compile time
    * very clean solutions for problems that are obviously recursive in nature
    * provides a paradigm for distributed processing that's very intuitive to reason about
### Disadvantages:
    * Can be difficult to implement a 'loose', overly general api structure
    * Supporting data injection/realtime data processing is not always obvious
        when typesafety is enforced
    * type-safe processing fundamentally requires different design paradigms
        to arrive at clean solutions

## --------High-Level Overview of Typical data flows----------
Typical is designed to easily build up stateful behavior in a clean
encapsulated way without side effects. To do this, in general the process
is to start with some initial dataset 'dat', run a calculation on it to produce new data,
then encapsulate those results in an updated version of dat. Those results are then propagated
forward to later calculations like so
``
    dat ----some calc ----> dat2 ------some calc ----> dat3 ...
``   
 
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
Suppose then we had a concrete dataset 'dat' which does not contain data for X. Then if somewhere in our
application we try to invoke f on dat (either within a Typical calculation defining further data transformations,
or outside of one such as an application entry point), we will get a compile time error.


## --------Getting Started---------------
Typical uses 3 main structures to define it's calculations.
They are: axiom, sim, and rsim.

Each of these structures are extensions of the more general type, dataset.
In general every transformation defined in typical takes the form of

``
dataset[sometype] => dataset[othertype]
``

### Axioms----------
axiom's are simply typed data that requires no processing, or is processed outside of Typical.

Their type paramater structure follows:
```
axiom[<datatype>,selfType]
```
where selfType should always be the class extending axiom.

For exaample, building an axiom that holds a Double is done like so:
```
class myaxiom extends axiom[Double,myaxiom](1d)
```
This axiom will always have the value 1 in any calculations that reference it and cannot be changed
by a transformation.

### Sims------------
sim's are data that have dependencies on other data via some transformation, but the transformation is
not recursive (in contrast to rsims' with are recursive). They require you to specify their dependencies
(a dataset or some combination of datasets) as well as the transformation from the dependency data.

Their type parameter structure is as follows:
```
sim[<datatype>,dependencyType,selfType]
```
where selfType should always be the class extending sim, and dependencyType is the combination of the types
of our data dependencies. The base dependencies can be any axiom,sim,or rsim

 For example, if we wanted to build a calculation with our above axiom as a dependency we would do so as follows:
```
class mySim extends sim[Double,myaxiom,mySim](
    ((src:dataset[myaxiom]) => {
        val ax = src.fetch[Double,myaxiom].value
        ax*1000 + 25
    }).set[mySim]
)(0d)

```
The function defined within mySim must be a function of type 
``
f:dataset[myaxiom] => Double.
``
In general for sim's holding data of type <datatype>, you would provide a function of type
``
 dataset[dependencies] => <datatype>.
``
The datatype for sim can be any type at all and does not have to match the datatype of it's dependencies. For example we could define
a sim like so
``
class isEven extends sim[Boolean,myaxiom,isEven](
    ((src:dataset[myaxiom]) => {
        val ax = src.fetch[Double,myaxiom].value
        (ax%2) == 0
    }).set[isEven]
)(0d)
``
Additionally, we can include multiple dependencies in a sim by combining their types using 'with'
``
class otherAxiom extends axiom[Double,otheraxiom](20d)
``
``
class multiDependencySim extends sim[Double, myaxiom with otheraxiom, multiDependencySim](
   ((src:dataset[myaxiom with otheraxiom]) => {
        val myax = src.fetch[Double,myaxiom].value
        val otherax = src.fetch[Double,otherax].value
        (myax + otherax)/2
    }).set[multiDependendencySim]
   )(0d)
``
In this example we've used two pieces of Typical's grammar to build our calculations. The first is the 'fetch' method
which allows us to retrieve data from state, without transforming it. This is in contrast with the 'calc' method which
transforms data, discussed below.

We also use the 'set' method on the sim's function argument to convert the function provided, which is just of type 
``
dataset[myaxiom with otheraxiom] => Double,
``
to a function of type 
``
dataset[myaxiom with otheraxiom] => dataset[multiDependencySim].
``
### RSims-------------------------------
rsim's (or recursive sims) are essentially the same as sims, but their transformations are
recursive in nature, meaning, they have themselves as a dependency.

Their type parameter structure is as follows:
``
rsim[<datatype>,dependencyType,selfType]
``
where selfType should always be the class extending sim, and dependencyType is the combination of the types
of our data dependencies. The base dependencies can be any axiom,sim,or rsim, and must include the selfType
``
class multiDependencyRecSim extends rsim[Double, myaxiom with mysim with multiDependencyRecSim, multiDependencyRecSim](
   ((src:dataset[myaxiom with mysim with multiDependencyRecSim]) => {
        val myax = src.fetch[Double,myaxiom].value
        val mysim = src.fetch[Double,otherax].value
        val recsimval = src.fetch[Double,multiDependencyRecSim].value
        (myax + otherax)*recsimval
    }).set[multiDependencyRecSim]
   )(1d)
``


### Running our calculations through chaining----------------
Now that we have our data transformations defined, we can provide an entry point and process some data!

Recall that we already know of two methods from Typical for data interaction at our disposal; 'calc' and 'fetch'.
There is also a third method, 'include' which allows the overwrite of state.

They are used as follows:

Suppose that 
 ``
dat:dataset[<some combination of datasets through 'with'>]
``
####Operations:
    * fetch:
        dat.fetch[<datatype_A>,A<:dataset[_]] -returns dataset A with the context of dat. It's 'value' paramater will
        be the most recent value for A in dat's context. If dat is not of type dataset[A], i.e. dat does not contain A,
        you will see a compile time error.
    * calc:
        dat.calc[<datatype_A>,A<:sim[<datatype_a>,_>:dat,A]) -returns A after it's transformation function is applied
        on dat. Conceptually can be thought of f_A(dat).fetch[<datatype_a>,A] where f_a is the function provided at A's
        definition (as a axiom,sim,recsim,etc...). If dat does not have the dependencies to calculate A you will see a compile time error.
        If used within a transformation, on a src dataset, the state produced by calc is encapsulated in the transformation.
    * include:
        dat.include[<datatype_A>,A<:dataset[_]](a:datatype_A) -returns a dataset with the state of dat, but with
        the value for type A overridden, if present, or newly included, if not present. If dat was not of type dataset[A]
        i.e. dat did not contain A, the return type will be of type dataset[A] and include A's specified value.
        If used within a transformation, on a src dataset, the state produced by calc is encapsulated in the transformation.
        

It should be noted that all three methods, 'calc','fetch', and 'include' can be used within the definitions of sims or rsims (like how we retrieved
our values from state above through fetch). Using any of these within a transformation will not cause side effects, as the
only value that will ever be changed after a transformation function is called is the one it's defined for. In this way, we can run
sims within sims using various parameters without side effects. Fundamentally choosing when to use which, where, is up to the programmer
and how they want to structure the flow of their data transformation exactly.

So while both 'fetch' and 'calc' could be chained together like so:
``
    dat.fetch.fetch.fetch
    dat.calc.calc.calc
``
When we chain fetch, our result will contain the same values as our original dataset. But when we chain calc, if
we have a recursive sim type in our dataset, then we may see it's value change. This is the method by which Typical
encapsulates state. We begin with an initial dataset with some data, then we chain together calc calls to build up our
state in a natural way.

So in the context of our above example that would look like the following:

Use the convenient data class to build an initial dataset from our types defined previously
Start by including any data you want to calc plus it's dependencies
``
val dat = data[
        myaxiom with
        otheraxiom with
        mySim with
        multiDependencySim with
        multiDependencyRecSim with
        ](
        baseprovider
            .register[myaxiom]
            .register[otheraxiom]
            .register[multiDependencyRecSim]
        )
        ``
        And add any datasets for which we want to include the initial values in the context.

Suppose we want to calculate a value for multiDependencyRecSim. It has multiDependencySim as a dependency which is
itself a sim type. Because we decided to use 'fetch' instead of 'calc' in multiDependencyRecSim's function definition,
to retrieve the value for multiDependencySim, we need to calc multiDependencySim first, otherwise when it's fetched in
multiDependencyRecSim it will just contain it's default starting value.
If we wanted to avoid this necessity, we could replace the 'fetch' call on multiDependencySim to a 'calc' call, in the
definition of multiDependencyRecSim. Recall however that 'calc' calls within a transformation are encapsulated and do not
update state for any type that isn't the one they're defined for. So if multiDependencySim was itself recursive, and had
values that changed after calls to 'calc', we would not be able to rely on this internal call to update it's global state.
``
val updatedDataset = dat.calc[Double,multiDependencySim].calc[Double,multiDependencyRecSim]
val updatedRecSimVal = updatedDataset.value //value of multiDependencyRecSim after transformation
``
We can then read values from our dataset with updated state with fetch, or we can continue to chain calc calls to further modify state.


And that should be all you need start building in Typical. So long as you understand axiom, sim, rsim, dataset,
calc and fetch you can start building complex systems. Remember type-safe data processing comes with it's own hurtles
and is not necessarily intuitive in every context. For more complex examples, look across this repo for implementations of sim
and recSim. Good starting points are any of the Test.scala files across the various branches of this repo, or on master there are
the KnapSack and Orders package which showcase more complex behavior, such as data input and real time processing.

