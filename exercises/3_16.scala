sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    def addOne(list: List[Int]): List[Int] = {
        def go(l: List[Int]): List[Int] = {
            // if there is another element ahead call function recursively on it
            // if there is not and the current element is an integer return integer + 1
            // otherwise return l
            l match {
                case Cons(head, tail) => Cons(head + 1, go(tail))
                case _ => l
            }
        }
        go(list)
    }
}

object Exercise_3_16 {
    def main(args: Array[String]): Unit = {
        println(List.addOne(List(1, 2, 3, 4, 5, 6)))
        println(List.addOne(List(1)))
        println(List.addOne(Nil))
    }
}