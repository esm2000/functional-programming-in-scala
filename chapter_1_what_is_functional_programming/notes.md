# Chapter 1 - What is Functional Programming


* We should aim to remove side effects from our programs
* Functional programming refers to programming with pure functions
    - Pure functions are functions that lack side effects
    - A function `f` with input type `A` and output type `B` (written in Scala as a single type, A => B) is a computation that relates every value a of type `A` to exactly one value `b` of type `B` such that `b` is determined solely by the value of `a`. Ay changing state of an internal or external process is irrelevant to computing the result `f(a)`
    - Referential transparency - For an expression to be considered referential transparent, the expression can be replaced by its result without changing the menaing or result of the program. When expressions are referentially tranpsarent, we can image that computation proceeds much like the wya we'd solve an alegraic expression.
    - Referenctial transparency and the accompanying "substitution model" is simple to reason about since understanding code only requires local understanding. We need not mentally track all the state changes that may occur before or after our function's execution to understand what our function will do. 
* Functional programming is more modular