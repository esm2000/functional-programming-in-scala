sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = {
        def go(l1: List[A], l2:List[A]): List[A] = {
            (l1, l2) match {
                case (Cons(head1, tail1), Cons(head2, tail2)) => Cons(f(head1, head2), go(tail1, tail2))
                case (Cons(head1, tail1), Nil) => Cons(head1, go(tail1, Nil))
                case (Nil, Cons(head2, tail2)) => Cons(head2, go(Nil, tail2))
                case (Nil, Nil) => Nil.asInstanceOf[List[A]]
            }
        }
        go(l1, l2)
    }

    def reverse[A](l: List[A]): List[A] = {
        @annotation.tailrec
        def loop(l: List[A], acc: List[A]): List[A] =
        l match {
            case Nil        => acc
            case Cons(h, t) => loop(t, Cons(h, acc))
        }

        loop(l, Nil)
    }

    // memory efficient
    def zipWithBornAgain[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = {
        @annotation.tailrec
        def go(l1: List[A], l2:List[A], acc: List[A]): List[A] = {
            (l1, l2) match {
                case (Cons(head1, tail1), Cons(head2, tail2)) => go(tail1, tail2, Cons(f(head1, head2), acc))
                case (Cons(head1, tail1), Nil) => go(tail1, Nil, Cons(head1, acc))
                case (Nil, Cons(head2, tail2)) => go(Nil, tail2, Cons(head2, acc))
                case (Nil, Nil) => acc
            }
        }
        reverse(go(l1, l2, Nil.asInstanceOf[List[A]]))
    }
}


object Exercise_3_23 {
    def main(args: Array[String]): Unit = {
        println(List.zipWith(List(1, 2, 3), List(4, 5, 6))(f = (a: Int, b: Int) => a + b))
        println(List.zipWith(List(1, 2), List(4, 5, 6))(f = (a: Int, b: Int) => a + b))
        println(List.zipWith(List(), List(4, 5, 6))(f = (a: Int, b: Int) => a + b))


        println(List.zipWith(List(1, 2, 3), List(4, 5, 6))(f = (a: Int, b: Int) => a * b))
        println(List.zipWith(List(1, 2), List(4, 5, 6))(f = (a: Int, b: Int) => a * b))
        println(List.zipWith(List(), List(4, 5, 6))(f = (a: Int, b: Int) => a * b))

        println(List.zipWithBornAgain(List(1, 2, 3), List(4, 5, 6))(f = (a: Int, b: Int) => a + b))
        println(List.zipWithBornAgain(List(1, 2), List(4, 5, 6))(f = (a: Int, b: Int) => a + b))
        println(List.zipWithBornAgain(List(), List(4, 5, 6))(f = (a: Int, b: Int) => a + b))
    }
}