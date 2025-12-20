trait Either[+E, +A] {
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
        es.foldRight[Either[E, List[A]]](Right(List()))((x, acc) =>
            x match {
                case Left(y) => Left(y)
                case Right(y) => acc match {
                    case Left(z) => Left(z)
                    case Right(z) => Right(y :: z)
                }
            }
        )

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
        as.foldRight[Either[E, List[B]]](Right(List()))((x, acc) =>
            f(x) match {
                case Left(y) => Left(y)
                case Right(y) => acc match {
                    case Left(z) => Left(z)
                    case Right(z) => Right(y :: z)
                }
            }
        )
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Exercise_4_7 {
    def main(args: Array[String]): Unit = {
        val either = new Either[String, Int] {}

        println(either.sequence(List(Right(1), Right(2), Right(3)))) // Right(List(1, 2, 3))
        println(either.sequence(List(Right(1), Left("error"), Right(3)))) // Left(error)
        println(either.sequence(List[Either[String, Int]]())) // Right(List())
        println("")

        def parseInt(s: String): Either[String, Int] =
            try Right(s.toInt) catch { case _: Exception => Left("not a number") }

        println(either.traverse(List("1", "2", "3"))(parseInt)) // Right(List(1, 2, 3))
        println(either.traverse(List("1", "abc", "3"))(parseInt)) // Left(not a number)
        println(either.traverse(List[String]())(parseInt)) // Right(List())
        println("")
        println(either.traverse(List(1, 2, 3))(x => Right(x * 2))) // Right(List(2, 4, 6))
        println(either.traverse(List(1, 2, 3))(x => if (x > 2) Left("too big") else Right(x))) // Left(too big)
    }
}

