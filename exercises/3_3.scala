sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

}

object Exercise_3_3 {
    def setHead[A](list: List[A], newHead: A): List[A] = 
        list match {
            case Cons(x, y) => Cons(newHead, y)
            case _ => Cons(newHead, Nil)
        }

    def main(args: Array[String]): Unit = {
        println(setHead(List(1, 2, 3), 5))
        println(setHead(List(1, 2), 2))
        println(setHead(List("a", "b"), "z"))
        println(setHead(Nil, "something"))
    }
}