sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
        def go1(a: List[A], b: List[A]): Boolean = {
            def go2(c: List[A], d: List[A]): Boolean = {
                (c, d) match {
                    case (Cons(cHead, cTail), Cons(dHead, dTail)) if cHead == dHead => go2(cTail, dTail)
                    case (Cons(cHead, cTail), Cons(dHead, dTail)) if cHead != dHead => false 
                    case (Nil, Cons(dHead, dTail)) => false
                    case (Cons(cHead, cTail), Nil) => true
                    case (Nil, Nil) => true
                }
            }
            (a, b) match {
                case (Cons(aHead, aTail), Cons(bHead, bTail)) if aHead == bHead => if (go2(aTail, bTail)) true else go1(aTail, b)
                case (Cons(aHead, aTail), Cons(bHead, bTail)) => go1(aTail, b)
                case (Nil, Cons(bHead, bTail)) => false
                case (Cons(aHead, aTail), Nil) => true
                case (Nil, Nil) => false
            }
        }
        if (sup == Nil && sub == Nil) {
            true
        }
        go1(sup, sub)
    }
}



object Exercise_3_24 {
    def main(args: Array[String]): Unit = {
        println(List.hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
        println(List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
        println(List.hasSubsequence(List(1, 2, 3, 4), List(4)))

        println(List.hasSubsequence(List(1, 2, 3, 4), List(5)))
        println(List.hasSubsequence(List(1, 2, 3, 4), List(0, 1, 2, 3, 4)))
    }
}