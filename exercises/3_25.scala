sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    def size[A](tree: Tree[A]): Int = {
        tree match {
            case Leaf(_) => 1
            case Branch(left, right) => 1 + size(left) + size(right)
        }
    }
}

object Exercise_3_25 {
    // Test trees for size() function

    // Single leaf node - should have size 1
    val singleLeaf: Tree[Int] = Leaf(42)

    // Simple tree with 3 nodes - should have size 3
    val smallTree: Tree[Int] = Branch(
        Leaf(1),
        Leaf(2)
    )

    // Larger tree with 9 nodes - should have size 9
    val largerTree: Tree[String] = Branch(
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
    val unbalancedTree: Tree[Int] = Branch(
        Leaf(10),
        Branch(
            Branch(
                Leaf(20),
                Leaf(30)
            ),
            Leaf(40)
        )
    )

    def main(args: Array[String]): Unit = {
        println(s"Single leaf size: ${Tree.size(singleLeaf)}")
        println(s"Small tree size: ${Tree.size(smallTree)}")
        println(s"Larger tree size: ${Tree.size(largerTree)}")
        println(s"Unbalanced tree size: ${Tree.size(unbalancedTree)}")
    }
}