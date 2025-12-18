sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    def fold[A, B](tree: Tree[A])(f: (B, B) => B, g: A => B): B =
        tree match {
            case Leaf(value) => g(value)
            case Branch(left, right) => f(fold(left)(f, g), fold(right)(f, g))
        }
    
    def size[A](tree: Tree[A]): Int = 
        fold(tree)((x: Int, y: Int) => x + y + 1, x => 1)

    def maximum(tree: Tree[Int]): Int =
        fold(tree)((x: Int, y: Int) => math.max(x, y), x => x)

    def depth[A](tree: Tree[A]): Int =
        fold(tree)((x: Int, y: Int) => math.max(x, y) + 1, x => 1) - 1

    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
        fold(tree)(
            (l: Tree[B], r: Tree[B]) => Branch(l, r),
            x => Leaf(f(x))
        )
}

object Exercise_3_29 {
    // Test trees for size() function

    // Single leaf node - should have size 1
    val sizeSingleLeaf: Tree[Int] = Leaf(42)

    // Simple tree with 3 nodes - should have size 3
    val sizeSmallTree: Tree[Int] = Branch(
        Leaf(1),
        Leaf(2)
    )

    // Larger tree with 9 nodes - should have size 9
    val sizeLargerTree: Tree[String] = Branch(
        Branch(
            Leaf("a"),
            Leaf("b")
        ),
        Branch(
            Leaf("c"),
            Branch(
                Leaf("d"),
                Leaf("e")
            )
        )
    )

    // Unbalanced tree with 7 nodes - should have size 7
    val sizeUnbalancedTree: Tree[Int] = Branch(
        Leaf(10),
        Branch(
            Branch(
                Leaf(20),
                Leaf(30)
            ),
            Leaf(40)
        )
    )

    // Test trees for maximum() function

    // Single leaf node
    val maximumSingleLeaf: Tree[Int] = Leaf(42)

    // Simple tree with 3 nodes
    val maximumSmallTree: Tree[Int] = Branch(
        Leaf(10),
        Leaf(25)
    )

    // Tree where maximum is at root level
    val maximumMaxAtRoot: Tree[Int] = Branch(
        Leaf(5),
        Leaf(3)
    )

    // Tree where maximum is deeply nested on left
    val maximumMaxOnLeft: Tree[Int] = Branch(
        Branch(
            Leaf(100),
            Leaf(15)
        ),
        Branch(
            Leaf(20),
            Leaf(30)
        )
    )

    // Tree where maximum is deeply nested on right
    val maximumMaxOnRight: Tree[Int] = Branch(
        Branch(
            Leaf(10),
            Leaf(15)
        ),
        Branch(
            Leaf(20),
            Branch(
                Leaf(5),
                Leaf(99)
            )
        )
    )

    // Tree with negative numbers
    val maximumNegativeTree: Tree[Int] = Branch(
        Leaf(-10),
        Branch(
            Leaf(-5),
            Leaf(-20)
        )
    )

    // Tree with mixed positive and negative numbers
    val maximumMixedTree: Tree[Int] = Branch(
        Branch(
            Leaf(-15),
            Leaf(7)
        ),
        Branch(
            Leaf(-3),
            Leaf(12)
        )
    )

    // Test trees for depth() function

    // Single leaf node - should have depth 0
    val singleLeaf: Tree[Int] = Leaf(42)

    // Simple tree with depth 1
    val depthOne: Tree[Int] = Branch(
        Leaf(10),
        Leaf(20)
    )

    // Tree with depth 2 - balanced
    val depthTwoBalanced: Tree[String] = Branch(
        Branch(
            Leaf("a"),
            Leaf("b")
        ),
        Branch(
            Leaf("c"),
            Leaf("d")
        )
    )

    // Tree with depth 3 - left-heavy
    val depthThreeLeft: Tree[Int] = Branch(
        Branch(
            Branch(
                Leaf(1),
                Leaf(2)
            ),
            Leaf(3)
        ),
        Leaf(4)
    )

    // Tree with depth 4 - right-heavy zigzag
    val depthFourZigzag: Tree[Int] = Branch(
        Leaf(1),
        Branch(
            Leaf(2),
            Branch(
                Leaf(3),
                Branch(
                    Leaf(4),
                    Leaf(5)
                )
            )
        )
    )

    // Tree with depth 3 - unbalanced
    val depthThreeUnbalanced: Tree[Double] = Branch(
        Leaf(1.5),
        Branch(
            Branch(
                Leaf(2.5),
                Leaf(3.5)
            ),
            Leaf(4.5)
        )
    )

    // Test trees for map() function

    // Single leaf node
    val mapSingleLeaf: Tree[Int] = Leaf(5)

    // Simple tree
    val mapSimpleTree: Tree[Int] = Branch(
        Leaf(10),
        Leaf(20)
    )

    // Tree with strings
    val mapStringTree: Tree[String] = Branch(
        Branch(
            Leaf("hello"),
            Leaf("world")
        ),
        Leaf("scala")
    )

    // Larger numeric tree
    val mapNumericTree: Tree[Int] = Branch(
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
    val mapDoubleTree: Tree[Double] = Branch(
        Leaf(1.5),
        Branch(
            Leaf(2.5),
            Leaf(3.5)
        )
    )


    def main(args: Array[String]): Unit = {
        println("\nsize()")
        println(s"Single leaf size (1): ${Tree.size(sizeSingleLeaf)}")
        println(s"Small tree size (3): ${Tree.size(sizeSmallTree)}")
        println(s"Larger tree size (9): ${Tree.size(sizeLargerTree)}")
        println(s"Unbalanced tree size (7): ${Tree.size(sizeUnbalancedTree)}")
    
        println("\nmaximum()")
        println(s"Single leaf maximum (42): ${Tree.maximum(maximumSingleLeaf)}")
        println(s"Small tree maximum (25): ${Tree.maximum(maximumSmallTree)}")
        println(s"Max at root maximum (5): ${Tree.maximum(maximumMaxAtRoot)}")
        println(s"Max on left maximum (100): ${Tree.maximum(maximumMaxOnLeft)}")
        println(s"Max on right maximum (99): ${Tree.maximum(maximumMaxOnRight)}")
        println(s"Negative tree maximum (-5): ${Tree.maximum(maximumNegativeTree)}")
        println(s"Mixed tree maximum (12): ${Tree.maximum(maximumMixedTree)}")

        println("\ndepth")
        println(s"Single leaf depth: ${Tree.depth(singleLeaf)}")
        println(s"Depth one tree: ${Tree.depth(depthOne)}")
        println(s"Depth two balanced: ${Tree.depth(depthTwoBalanced)}")
        println(s"Depth three left-heavy: ${Tree.depth(depthThreeLeft)}")
        println(s"Depth four zigzag: ${Tree.depth(depthFourZigzag)}")
        println(s"Depth three unbalanced: ${Tree.depth(depthThreeUnbalanced)}")

        println("\nmap()")
        // Test mapping numbers to their squares
        println("Original single leaf: " + mapSingleLeaf)
        println("Squared: " + Tree.map(mapSingleLeaf)(x => x * x))

        // Test mapping numbers to strings
        println("Original simple tree: " + mapSimpleTree)
        println("To strings: " + Tree.map(mapSimpleTree)(_.toString))

        // Test mapping strings to lengths
        println("Original string tree: " + mapStringTree)
        println("String lengths: " + Tree.map(mapStringTree)(_.length))

        // Test mapping numbers to boolean (even/odd)
        println("Original numeric tree: " + mapNumericTree)
        println("Even/odd: " + Tree.map(mapNumericTree)(_ % 2 == 0))

        // Test mapping doubles to integers
        println("Original double tree: " + mapDoubleTree)
        println("Rounded to int: " + Tree.map(mapDoubleTree)(_.toInt))
    }
}