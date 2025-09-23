object Exercise_2_2 {
    def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
        @annotation.tailrec
        def loop(n: Int): Boolean = {
            if (n+1 >= as.length) true
            else if (!ordered(as(n), as(n+1))) false
            else loop(n+1)
        }
        loop(0)
    }

    def isOrdered(n: Int, m: Int): Boolean =
        n <= m
    
    def isOrdered(n: String, m: String): Boolean =
        n <= m

    def main(args: Array[String]): Unit = {
        println(isSorted(Array[Int](), (n: Int, m: Int) => isOrdered(n, m)))
        println(isSorted(Array(1, 2, 3), (n: Int, m: Int) => isOrdered(n, m)))
        println(isSorted(Array(1, 2, 3, 7, 0), (n: Int, m: Int) => isOrdered(n, m)))

        println(isSorted(Array[String](), (n: String, m: String) => isOrdered(n, m)))
        println(isSorted(Array("a", "b", "c"), (n: String, m: String) => isOrdered(n, m)))
        println(isSorted(Array("a", "z", "c"), (n: String, m: String) => isOrdered(n, m)))
    }
}