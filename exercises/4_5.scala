sealed trait Option[+A]
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
        a.foldRight[Option[List[B]]](Some(List())) { (x, acc) =>
            f(x) match {
                case None => None
                case Some(y) => acc match {
                    case None => None
                    case Some(z) => Some(y :: z)
                }
            }
        }
    }
}

object Exercise_4_5 {
    def main(args: Array[String]): Unit = {
        def parseInt(s: String): Option[Int] =
            try Some(s.toInt) catch { case _: NumberFormatException => None }

        println(Option.traverse(List("1", "2", "3"))(parseInt)) // Some(List(1, 2, 3))
        println(Option.traverse(List("1", "abc", "3"))(parseInt)) // None
        println(Option.traverse(List[String]())(parseInt)) // Some(List())
        println("")
        println(Option.traverse(List(1, 2, 3))(x => Some(x * 2))) // Some(List(2, 4, 6))
        println(Option.traverse(List(1, 2, 3))(x => if (x % 2 == 0) Some(x) else None)) // None
    }
}