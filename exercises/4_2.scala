sealed trait Option[+A] {    
    def flatMap[B](f: A => Option[B]): Option[B] =
        this match {
            case None => None
            case Some(x) => f(x)
        }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Exercise_4_2 {

    def variance(xs: Seq[Double]): Option[Double] = {
    def mean(xs: Seq[Double]): Option[Double] = {
        val sum = xs.foldRight(0.0)(_ + _)
        val count = xs.length
        if (count == 0) None else Some(sum / count)
    }

    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}

    def main(args: Array[String]): Unit = {
        println(variance(Seq(1.0, 2.0, 3.0, 4.0, 5.0))) // 2
        println(variance(Seq(2.0, 2.0, 2.0, 2.0))) // 0
        println(variance(Seq())) // None
        println(variance(Seq(1.0))) // 0
    }    
}