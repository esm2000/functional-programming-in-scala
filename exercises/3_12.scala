
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    // not tail recursive because the recursive call is not
    // the last thing done by the function
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }

    // List(x0, x1, x2): C(x0, C(x1, C(x2, N)))
    // foldRight:        f(x0, f(x1, f(x2, z)))
    // foldLeft:         f(f(f(z, x0), x1), x2)))
    @annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => foldLeft(xs, f(x, z))(f)
        }

    def reverse[A](as: List[A]): List[A] =
        foldLeft(as, Nil: List[A])(Cons(_,_))
}

object Exercise_3_12 {
    def main(args: Array[String]): Unit = {
        println(List.reverse(List(1, 2, 3, 4)))
        println(List.reverse(List(1)))
        println(List.reverse(List()))
    }
}