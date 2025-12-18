# Chapter 3 - Functional Data Structures

* A functional data structure is operated on using only pure functions. Therefore functional data structures are by definition immutable
* Pattern matching works a lot like a fancy `switch` statement that may descend into the structure of the expression it examines and extract subexpressions of that structure. Each case in the match consists of a *pattern* to the left of the `=>`. If the target *matches* the pattern in a case, the result of that case becomes the result of the entire match expression. If multiple patterns match the target, Scala chooses the first matching case.
* We'll often declare a *companion* object in addition to our data type and its data constructors. This is an object with the same name as the data type where we put various convenience functions for creating or working with values of the data type.
* A *variadic function* is a function that accepts zero or more arguments of type A
    - For data types, it's a common idiom to have a variadic `apply` method in the companion object to conveniently construct instances of the data type. By calling this function `apply` and placing it in the companion object we can invoke it with syntax like `List(1, 2, 3, 4)` or `List("hi", "bye")`, with as many values as we want separated by commas.
    - Variadic functions are just providing a little syntactic sugar for creating and passing a `Seq` of elements explicitly.
    - Ex: 
    ```
    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
    ```
* When data is immutable, to write functions that add or remove elements from a list, we simply add or remove an element and return a new list. Since lists are immutable we don't need to copy the original list; we can just reuse it. This is called *data sharing* (where the input object and output object share the same data in memory but with distinct references).
    - Sharing of immutable data often lets us implement functions more efficiently; we can always return immutable data structures without having to worry about subsequent code modifying our data.
* To achieve recursion over a list, `foldRight()` is often the best, most functional way to do so.
* `List` is just one example of what's called an *algebraic data type (ADT)*. An ADT is just a data type defined by one or more data constuctors, each of which may contain zero or more arguments.
* Tuples are algebraic data types but have special syntax:
```
scala> val p = ("Bob", 42)
p: (java.lang.String, Int) = (Bob,42)

scala> p._1
res0: java.lang.String = Bob

scala> p._2
res1: Int = 42

scala> p match { case (a,b) => b }
res2: Int = 42
```
* Algebraic data types can be used to define other data structures like the binary tree data structure. 
```
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
```