sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] =
        this match {
            case None => None
            case Some(x) => Some(f(x))
        }
    
    def flatMap[B](f: A => Option[B]): Option[B] =
        this match {
            case None => None
            case Some(x) => f(x)
        }

    def getOrElse[B >: A](default: => B): B =
        this match {
            case None => default
            case Some(x) => x
        }

    def orElse[B >: A](ob: => Option[B]): Option[B] =
        this match {
            case None => ob
            case Some(x) => Some(x)
        }

    def filter(f: A => Boolean): Option[A] =
        this match {
            case Some(x) if f(x) => Some(x)
            case _ => None
        }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Exercise_4_1 {
    def main(args: Array[String]): Unit = {
        println(Some(2).map((x: Int) => 2 * x))
        println(None.map((x:Int) => 2 * x))
        println("")
        println(Some(2).flatMap((x: Int) => Some(x * 2)))
        println(Some(2).flatMap((x: Int) => None))
        println(None.flatMap((x: Int) => Some(x * 2)))
        println("")
        println(Some(2).getOrElse("something"))
        println(None.getOrElse("something"))
        println("")
        println(Some(2).orElse(Some(1)))
        println(None.orElse(Some(1)))
        println("")
        println(Some(2).filter((x: Int) => x % 2 == 1))
        println(Some(2).filter((x: Int) => x % 2 == 0))
        println(None.filter((x: Int) => x % 2 == 0))
    }    
}