sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

}

object Exercise_3_4 {
    def tail[A](list: List[A]): List[A] = 
        list match {
            case Cons(x, y) => y
            case _ => Nil
        }

    def drop[A](l: List[A], n: Int): List[A] = {
        @annotation.tailrec
        def go(itr: Int, o: List[A]): List[A] = {
            if(itr == 0 || o == Nil || o == Cons(Nil, Nil)) o
            else go(itr-1,tail(o))
        }
        go(n, l)
    }

    def main(args: Array[String]): Unit = {
        println(drop(List(1, 2, 3), 2))
        println(drop(List(1, 2, 3, 4), 1))
        println(drop(List(1, 2, 3, 4), 2))
        println(drop(List(1, 2, 3, 4), 4))
        println(drop(List(1, 2, 3, 4), 5))
        println(drop(Nil, 1))
    }
}
