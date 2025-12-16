sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    def map[A,B](as: List[A])(f: A => B): List[B] = {
        def go(l: List[A]): List[B] = {
            l match {
                case Cons(head, tail) => Cons(f(head), go(tail))
                case Nil => Nil.asInstanceOf[List[B]]
            }
        }
        go(as)
    }
}

object Exercise_3_18 {
    def main(args: Array[String]): Unit = {
        println(List.map(List(1, 2, 3, 4, 5, 6))(f = (a: Int) => a+1))
        println(List.map(List(1))(f = (a: Int) => a+1))
        println(List.map(Nil)(f = (a: Int) => a+1))
    }
}