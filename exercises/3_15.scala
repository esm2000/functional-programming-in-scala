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

    def append[A](as: List[A], x: A): List[A] = 
        (as,x) match {
            case (Nil, Nil) => Nil
            case (Nil, _) => Cons(x, Nil)
            case (_, Nil) => as
            case (_, _) => foldRight(as, Nil:List[A])((x_, y_) => y_ match {
                case Nil => Cons(x_, Cons(x, Nil))
                case _ => Cons(x_, y_)
            })
        }

    def concat[A](as: List[List[A]]): List[A] =
        foldLeft(as, Nil:List[A])((x, y) => {
            List.foldRight(
                y, x
            )(Cons(_,_))
        })
}

object Exercise_3_15 {
    def main(args: Array[String]): Unit = {
        println(List.concat(List(List(1, 2, 3), List(4, 5, 6))))
        println(List.concat(List(List(1, 2), List(3, 4), List(5, 6))))
        println(List.concat(List(Nil, List(1, 2, 3))))
        println(List.concat(List(List(1, 2, 3), Nil, List(4, 5, 6))))
    }
}