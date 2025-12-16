sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }

    @annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
        as match {
            case Nil => z
            case Cons(x, xs) => foldLeft(xs, f(x, z))(f)
        }

    def concat[A](as: List[List[A]]): List[A] =
        foldLeft(as, Nil:List[A])((x, y) => {
            List.foldRight(
                y, x
            )(Cons(_,_))
        })

    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
        def go(l: List[A]): List[B] = {
            l match {
                case Cons(head, tail) => concat(List(f(head), go(tail)))
                case Nil => Nil.asInstanceOf[List[B]]
            }
        }
        go(as)
    }
}

object Exercise_3_20 {
    def main(args: Array[String]): Unit = {
        println(List.flatMap(List(1,2,3))(i => List(i,i)))
        println(List.flatMap(List(1))(i => List(i,i)))
        println(List.flatMap(List(Nil))(i => List(i,i)))
    }
}