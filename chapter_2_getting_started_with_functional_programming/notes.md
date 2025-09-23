# Chapter 2 - Getting Started with Functional Programming

* Scala code has to be in an object or class
* The `object` keyword creates a new *singleton type*, which is like a class that only has a single named instance
* The part of the declaration that goes before the equals sign is referred to the *left-hand side* or *signature*, and the code that comes after the equals sign as the *right-hand side* or *declaration*
* When writing purely functional programs, we'll often find it useful to write a funciton that accepts other functions as arguments. This is called a *higher-order function (HOF)*. This is very useful when writing purely functional programs.
* In Scala instead of loops we tend to go with recursion like the example below. The recursive function is compiled to the same sort of bytecode as would be emitted for a while loop (as long as the recursive call is in *tail poisiton*).
    -  A call is said to be in *tail position* if the caller does nothing other than return the value of the recusrive call
```
def factorial(n: Int): Int = {
        def go(n: Int, acc: Int): Int =
            if (n<=0) acc
            else go(n-1, n*acc)
            // if we did 1 + go(n-1, n*acc), the resursive call would no longer be in tail position
        go(n, 1)
}
```
* Often, and especially when writing HOFs, we want to write code that works for any type it's given. These are called *polymorphic functions* (this is different from how it's usually referred to in object-oriented programming... this kind of polymorphism is sometimes called parametric polymorphism). Look at the example below:
```
def findFirst[A](as: Array[A], p: A => Boolean): Int = {
        @annotation.tailrec
        def loop(n: Int): Int = 
            if (n >= as.length) -1
            else if (p(as(n))) n
            else loop(n + 1)
        
        loop(0)
    }
```
* It is often convenient to be able to call these functions with *anonymous functions* or *function literals*, rather than having to suppply some existing named function.
```
scala> findFirst(Array(7, 9, 13), (x: Int) => x == 9)
res2: Int = 1
```
* When we define a function literal, what is actually being defined in Sclaa is an object with a method called `apply`. Scala has a special rule for this method name, so that objects that have an `apply` method can be called as if they were themselves methods.
```
val lessThan = new Function2[Int, Int, Boolean] {
  def apply(a: Int, b: Int) = a < b
}
```
```
scala> val b = lessThan.apply(10, 20)
b: Boolean = true
```