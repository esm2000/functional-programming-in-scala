sealed trait Option[+A]
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Exercise_4_3 {
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
        case (Some(x), Some(y)) => Some(f(x, y))
        case _ => None
    }

    def main(args: Array[String]): Unit = {
        println(map2(Some(3), Some(4))(_ + _)) // Some(7)
        println(map2(Some(3), Some(4))(_ * _)) // Some(12)
        println(map2(Some(3), None: Option[Int])(_ + _)) // None
        println(map2(None: Option[Int], Some(4))(_ + _)) // None
        println(map2(None: Option[Int], None: Option[Int])(_ + _)) // None
        println("")
        println(map2(Some("Hello"), Some("World"))(_ + " " + _)) // Some(Hello World)
        println(map2(Some(5), Some(2))(_ - _)) // Some(3)
    }
}