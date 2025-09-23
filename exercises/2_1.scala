object Exercise_2_1 {
    def fib(n: Int): Int = {
        @annotation.tailrec
        def go(itr: Int, curr: Int, prev: Int): Int = {
            if (n == 1) 0
            else if (itr == n-1) curr
            else go(itr+1, curr+prev, curr)
        }
        if (n <= 0) throw new Exception("n must be greater than 0")
        go(1, 1, 0)
    }

    def main(args: Array[String]): Unit = {
        @annotation.tailrec
        def go(n: Int, curr: Int): Unit = {
            if (curr <= n) {
                val msg = "%d: %d".format(curr, fib(curr))
                println(msg)
                go(n, curr+1)
            }
        }
        go(7, 1)
    }
}