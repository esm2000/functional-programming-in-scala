sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

}

object Exercise_3_5 {
    def tail[A](list: List[A]): List[A] = 
        list match {
            case Cons(x, y) => y
            case _ => Nil
        }

    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
        @annotation.tailrec
        def go(o: List[A]): List[A] = {
            val h: A = o match {
                case Cons(head, tail) => head
                case _ => Nil.asInstanceOf[A]
            }

            if (h == Nil || h == Cons(Nil, Nil)) List()
            else if(!f(h)) o
            else go(tail(o))
        }
        go(l)
    }

    def main(args: Array[String]): Unit = {
        println(dropWhile(List(1, 2, 3), f = (a: Int) => a <= 2))
        println(dropWhile(List(1, 2, 3), f = (a: Int) => a > 5))
        println(dropWhile(List(1, 2, 3), f = (a: Int) => a < 5))
        println(dropWhile(Nil, f = (a:Int) => a < 5))
        println(dropWhile(List(), f = (a:Int) => a < 5))
    }
}
