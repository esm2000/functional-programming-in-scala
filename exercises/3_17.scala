sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

    def convertToString(list: List[Double]): List[String] = {
        def go(l: List[Double]): List[String] = {
            l match {
                case Cons(head, tail) => Cons(head.toString(), go(tail))
                case Nil => Nil.asInstanceOf[List[String]]
            }
        }
        go(list)
    }

}

object Exercise_3_17 {
    def main(args: Array[String]): Unit = {
        println(List.convertToString(List(1.0, 2.0, 3.0)))
        println(List.convertToString(List(1.0)))
        println(List.convertToString(Nil))
    }
}