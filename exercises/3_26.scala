sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    def maximum(tree: Tree[Int]): Int = {
        def go(root: Tree[Int], currentMax: Option[Int]): Int = {
            root match {
                case Leaf(value) => currentMax match {
                    case None => value
                    case Some(max) => math.max(max, value)
                }
                case Branch(left, right) => math.max(go(left, currentMax), go(right, currentMax))
            }
        }
        go(tree, None)

    }
}

object Exercise_3_26 {
    // Test trees for maximum() function

    // Single leaf node
    val singleLeaf: Tree[Int] = Leaf(42)

    // Simple tree with 3 nodes
    val smallTree: Tree[Int] = Branch(
        Leaf(10),
        Leaf(25)
    )

    // Tree where maximum is at root level
    val maxAtRoot: Tree[Int] = Branch(
        Leaf(5),
        Leaf(3)
    )

    // Tree where maximum is deeply nested on left
    val maxOnLeft: Tree[Int] = Branch(
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
    val maxOnRight: Tree[Int] = Branch(
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
    val negativeTree: Tree[Int] = Branch(
        Leaf(-10),
        Branch(
            Leaf(-5),
            Leaf(-20)
        )
    )

    // Tree with mixed positive and negative numbers
    val mixedTree: Tree[Int] = Branch(
        Branch(
            Leaf(-15),
            Leaf(7)
        ),
        Branch(
            Leaf(-3),
            Leaf(12)
        )
    )

    def main(args: Array[String]): Unit = {
        println(s"Single leaf maximum: ${Tree.maximum(singleLeaf)}")
        println(s"Small tree maximum: ${Tree.maximum(smallTree)}")
        println(s"Max at root maximum: ${Tree.maximum(maxAtRoot)}")
        println(s"Max on left maximum: ${Tree.maximum(maxOnLeft)}")
        println(s"Max on right maximum: ${Tree.maximum(maxOnRight)}")
        println(s"Negative tree maximum: ${Tree.maximum(negativeTree)}")
        println(s"Mixed tree maximum: ${Tree.maximum(mixedTree)}")
    }
}