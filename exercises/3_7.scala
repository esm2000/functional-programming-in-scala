// product() implemented with foldRight() can
// immediately halt the recursion and return 0.0.

// You can modify the function f passed in to 
// foldRight(ns, 1.0)(f) to be based on match statement
// where if x in Cons(x, xs) is nonzero use x + y otherwise
// just return 0. This should elimintat the need
// to evaluate xs (which would be the rest of the list)