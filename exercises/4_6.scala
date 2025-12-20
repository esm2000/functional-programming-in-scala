sealed trait Option[+A]
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]



trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] =
        this match {
            case Left(e) => Left(e)
            case Right(a) => Right(f(a))
        }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
        this match {
            case Left(e) => Left(e)
            case Right(a) => f(a)
        }
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
        this match {
            case Left(x) => b
            case Right(a) => Right(a)
        }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
        this match {
            case Left(e) => Left(e)
            case Right(a) => b match {
                case Left(e) => Left(e)
                case Right(value_of_b) => Right(f(a, value_of_b))
            }
        }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Exercise_4_6 {
    def main(args: Array[String]): Unit = {
        val right5: Either[String, Int] = Right(5)
        val left: Either[String, Int] = Left("error")

        println(right5.map(_ * 2)) // Right(10)
        println(left.map(_ * 2)) // Left(error)
        println("")

        println(right5.flatMap(x => Right(x.toString))) // Right(5)
        println(right5.flatMap(x => Left("failed"))) // Left(failed)
        println(left.flatMap(x => Right(x.toString))) // Left(error)
        println("")

        println(right5.orElse(Right(99))) // Right(5)
        println(left.orElse(Right(99))) // Right(99)
        println(left.orElse(Left("error2"))) // Left(error2)
        println("")

        println(right5.map2(Right(3))(_ + _)) // Right(8)
        println(right5.map2(Left("error2"): Either[String, Int])(_ + _)) // Left(error2)
        println(left.map2(Right(3))(_ + _)) // Left(error)
    }
}
