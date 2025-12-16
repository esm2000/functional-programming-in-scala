sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))

}

object Exercise_3_6 {
    def tail[A](list: List[A]): List[A] = 
        list match {
            case Cons(x, y) => y
            case _ => Nil
        }

    def init[A](l: List[A]): List[A] =
        l match {
            case Nil => l
            case Cons(x, Nil) => Nil
            case Cons(x, Cons(y, Nil)) => Cons(x, Nil)
            case Cons(x, y) => Cons(x, init(y))
        }
    

    def main(args: Array[String]): Unit = {
        println(init(List(1, 2, 3, 4, 5)))
        println(init(List(1, 2)))
        println(init(List(1)))
        println(init(List()))
        println(init(Nil))
    }
}