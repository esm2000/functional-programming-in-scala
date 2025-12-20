sealed trait Option[+A]
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
    def sequence[A](a: List[Option[A]]): Option[List[A]] = {
        // we choose to foldRight (right -> left) so that when we prepend with :: we process the list in order
        a.foldRight[Option[List[A]]](Some(List())) { (x, acc) =>
            x match {
                case None => None
                case Some(y) => acc match {
                    case None => None
                    case Some(z) => Some(y :: z)
                }
            }
        }
    }
}

object Exercise_4_4 {
    def main(args: Array[String]): Unit = {
        println(Option.sequence(List[Option[Int]]())) // List()
        println(Option.sequence(List[Option[Int]](None, None, Some(3)))) // None
        println(Option.sequence(List[Option[Int]](Some(3), Some(4), Some(5), Some(5)))) // List(3, 4, 5, 5)
    }
}