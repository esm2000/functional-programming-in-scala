sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    def filter[A](as: List[A])(f: A => Boolean): List[A] = {
        def go(l: List[A]): List[A] = {
            l match {
                case Cons(head, tail) if f(head) => Cons(head, go(tail))
                case Cons(head, tail) if !f(head) => go(tail)
                case Nil => Nil.asInstanceOf[List[A]]
            }
        }
        go(as)
    }
}

object Exercise_3_19 {
    def main(args: Array[String]): Unit = {
        println(List.filter(List(1, 2, 3, 4, 5, 6))(f = (a: Int) => a % 2 == 0))
        println(List.filter(List(1))(f = (a: Int) => a % 2 == 0))
        println(List.filter(List(2))(f = (a: Int) => a % 2 == 0))
        println(List.filter(Nil)(f = (a: Int) => a % 2 == 0))
    }
}