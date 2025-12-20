# Chapter 4 - Handling errors without exceptions

* Throwing exceptions is a side effect. If exceptions aren't used in functional code, what is used instead?
    - The big idea is that we can represent failures and exceptions with ordinary values, and we can write higher-order functions that abstract out common patterns of error handling and recovery. The functional solution, of returning errors as values, is safer and retains referential transparency, and through the use of higher-order functions, we can preserve the primary benefit of exceptions—consolidation of error-handling logic.
* We inherently reject returning/writing bogus/sentinel values instead of throwing exceptions.
    - this can cause errors to silently propagate
    - results in a fair amount of boilerplate code at call sites
    - sentinel values might not be possible when writing polymorphic code
* Forcing the caller to supply an argument that tells us what to do in case we don't know how to handle an input is better but has drawbacks.
    - immediate callers must have direct knowledge of how to handle the undefined case and limits them
* The solution is to represent explicitly in the return type that a function may not always have an answer. `Option` accomplishes this.
    - `Option` has two cases: it can be defined, in which case it will be a `Some`, or it can be undefined, in which case it will be `None`.
    - We can use `Option` in the return type of functions to reflect the possibility that the result may not always be defined.
* `Option` can be thought of like a `List` that can contain at most one element, and many of the `List` functions we saw earlier have analogous functions on `Option`.
    - `map()` can be used to transform the result inside an `Option`, if it exists. We can think of it as proceeding with a computation on the assumption that an error hasn't occurred
    - with `flatMap()` we can construct a computation with multiple stages, any of which may fail, and the computation will abort as soon as the first failure is encountered, since `None.flatMap(f)` will immediately return `None`, without running `f()`
    - `filter()` can be used to convert successes into failures if the successful values don't match the given predicate
    - `getOrElse()` is often used to do error handling at the end.
    - `orElse()` is similar to `getOrElse` except that we return another `Option` if the first is undefined. This is often useful when we need to chain together possibly failing computations, trying the second if the first hasn't succeeded.
* We can "lift" ordinary functions to become functions that operate on Option
```
def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

// Example
val absO: Option[Double] => Option[Double] = lift(math.abs)
```
* Since lifting functions is so common in Scala, Scala provides a syntactic construct called the for-comprehension that it expands automatically to a series of flatMap and map calls
```
// Instead of nested flatMap/map calls:
def calculateInsuranceRateQuote(age: Option[Int], tickets: Option[Int]): Option[Double] =
  age.flatMap(a =>
    tickets.map(t =>
      insuranceRateQuote(a, t)))

// We can use for-comprehension syntax:
def calculateInsuranceRateQuote(age: Option[Int], tickets: Option[Int]): Option[Double] =
  for {
    a <- age
    t <- tickets
  } yield insuranceRateQuote(a, t)

// The compiler automatically transforms this into the flatMap/map version
```
* The `Either` data type extends the concept of `Option` by allowing us to track information about failures
    - `Option` can only tell us that something failed (`None`) but not why it failed
    - `Either[E, A]` has two cases: `Left(error)` for failures and `Right(value)` for successes
    - By convention, `Right` is used for success (pun on "right" meaning correct) and `Left` for errors
    - This allows us to return meaningful error messages or exception details instead of just `None`
```
// Instead of losing error information:
def mean(xs: IndexedSeq[Double]): Option[Double] =
  if (xs.isEmpty) None else Some(xs.sum / xs.length)

// We can provide specific error details:
def mean(xs: IndexedSeq[Double]): Either[String, Double] =
  if (xs.isEmpty)
    Left("mean of empty list!")
  else
    Right(xs.sum / xs.length)
```