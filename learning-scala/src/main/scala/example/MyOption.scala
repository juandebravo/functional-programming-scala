package example

sealed trait MyOption[+A] {

  def map[B](f: A => B): MyOption[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = map(f).getOrElse(None)

  def orElse[B >:A](ob: => MyOption[B]): MyOption[B] = map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): MyOption[A] = this match {
    case Some(x) if f(x) => Some(x)
    case _ => None
  }
}

case object None extends MyOption[Nothing]

case class Some[+A](get: A) extends MyOption[A]

//def mean(xs: Seq[Double]): MyOption[Double] = xs.isEmpty match {
//  case true => None
//  case _ => Some(xs.sum / xs.length)
//}

object MyOption {

}