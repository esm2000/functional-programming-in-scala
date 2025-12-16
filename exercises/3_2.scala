sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

}

object Exercise_3_2 {
    def tail[A](list: List[A]): List[A] = 
        list match {
            case Cons(x, y) => y
            case _ => Nil
        }

    def main(args: Array[String]): Unit = {
        println(tail(List(1, 2, 3)))
        println(tail(List(1, 2)))
        println(tail(List("a", "b")))
        println(tail(Nil))
    }
}
