sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
    def depth[A](tree: Tree[A]): Int = {
        def go(root: Tree[A], currentDepth: Int): Int = {
            root match {
                case Leaf(value) => currentDepth
                // case None => currentDepth - 1
                case Branch(left, right) => math.max(go(left, currentDepth+1), go(right, currentDepth+1))
            }
        }
        tree match {
            // case None => 0
            case _ => go(tree, 0)
        }
    }
}

object Exercise_3_27 {
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

    def main(args: Array[String]): Unit = {
        println(s"Single leaf depth: ${Tree.depth(singleLeaf)}")
        println(s"Depth one tree: ${Tree.depth(depthOne)}")
        println(s"Depth two balanced: ${Tree.depth(depthTwoBalanced)}")
        println(s"Depth three left-heavy: ${Tree.depth(depthThreeLeft)}")
        println(s"Depth four zigzag: ${Tree.depth(depthFourZigzag)}")
        println(s"Depth three unbalanced: ${Tree.depth(depthThreeUnbalanced)}")
    }
}