sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    def startsWith[A](sup: List[A], sub: List[A]): Boolean = {
        (sup, sub) match {
            case (Cons(aHead, aTail), Cons(bHead, bTail)) if aHead == bHead => startsWith(aTail, bTail)
            case (Cons(aHead, aTail), Nil) => true
            case _ => false
        }
    }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
        (sup, sub) match {
            case (Cons(aHead, aTail), Cons(bHead, bTail)) => startsWith(sup, sub) || hasSubsequence(aTail, sub)
            case (Nil, Cons(bHead, bTail)) => false
            case (Cons(aHead, aTail), Nil) => true
            case (Nil, Nil) => true
        }

    }
}



object Exercise_3_24 {
    def main(args: Array[String]): Unit = {
        println(List.hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
        println(List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
        println(List.hasSubsequence(List(1, 2, 3, 4), List(4)))
       
        println(List.hasSubsequence(List(1, 2, 3, 4), List(1, 3, 4)))
        println(List.hasSubsequence(List(1, 2, 3, 4), List(5)))
        println(List.hasSubsequence(List(1, 2, 3, 4), List(0, 1, 2, 3, 4)))
    }
}