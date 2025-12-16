
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
    
    def weirdFoldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
        foldLeft(foldLeft(as, Nil: List[A])(Cons(_,_)), z)(f)
    
    def reverse[A](l: List[A]): List[A] = {
        @annotation.tailrec
        def loop(l: List[A], acc: List[A]): List[A] =
        l match {
            case Nil        => acc
            case Cons(h, t) => loop(t, Cons(h, acc))
        }

        loop(l, Nil)
    }

    def weirdFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
        foldRight(reverse(as), z)(f)
}

object Exercise_3_13 {
    def main(args: Array[String]): Unit = {
        println(List.weirdFoldRight(List(1,2,3), 0)((x, y) => x+y))
        println(List.weirdFoldRight(List(): List[Int], 0)((x, y) => x+y))
        println(List.weirdFoldRight(List(1,2,3,4,5), 0)((x, y) => x+y))
        println(List.weirdFoldLeft(List(1,2,3), 0)((x, y) => x+y))
        println(List.weirdFoldRight(List(1, 2, 3), "")((x, y) => y+x.toString()))

    }
}