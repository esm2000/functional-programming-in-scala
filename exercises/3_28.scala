sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
        tree match {
            case Branch(left, right) => Branch(map(left)(f), map(right)(f))
            case Leaf(value) => Leaf(f(value))
        }
    }
}

object Exercise_3_28 {
    // Test trees for map() function

    // Single leaf node
    val singleLeaf: Tree[Int] = Leaf(5)

    // Simple tree
    val simpleTree: Tree[Int] = Branch(
        Leaf(10),
        Leaf(20)
    )

    // Tree with strings
    val stringTree: Tree[String] = Branch(
        Branch(
            Leaf("hello"),
            Leaf("world")
        ),
        Leaf("scala")
    )

    // Larger numeric tree
    val numericTree: Tree[Int] = Branch(
        Branch(
            Leaf(1),
            Branch(
                Leaf(2),
                Leaf(3)
            )
        ),
        Branch(
            Leaf(4),
            Leaf(5)
        )
    )

    // Tree with doubles
    val doubleTree: Tree[Double] = Branch(
        Leaf(1.5),
        Branch(
            Leaf(2.5),
            Leaf(3.5)
        )
    )

    def main(args: Array[String]): Unit = {
        // Test mapping numbers to their squares
        println("Original single leaf: " + singleLeaf)
        println("Squared: " + Tree.map(singleLeaf)(x => x * x))

        // Test mapping numbers to strings
        println("Original simple tree: " + simpleTree)
        println("To strings: " + Tree.map(simpleTree)(_.toString))

        // Test mapping strings to lengths
        println("Original string tree: " + stringTree)
        println("String lengths: " + Tree.map(stringTree)(_.length))

        // Test mapping numbers to boolean (even/odd)
        println("Original numeric tree: " + numericTree)
        println("Even/odd: " + Tree.map(numericTree)(_ % 2 == 0))

        // Test mapping doubles to integers
        println("Original double tree: " + doubleTree)
        println("Rounded to int: " + Tree.map(doubleTree)(_.toInt))
    }
}