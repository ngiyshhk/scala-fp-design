package fp.design.chapter5

import scala.annotation.tailrec

object Main extends App {

  trait Stream[+A] {
    import Stream._
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
      case _ => Empty
    }

    @tailrec
    final def drop(n: Int): Stream[A] = this match {
      case _ if n <= 0 => this
      case Cons(_, t) => t().drop(n - 1)
    }

    def takeWhile(f: A => Boolean): Stream[A] = this match {
      case Cons(h, t) =>
        if (f(h())) cons(h(), t().takeWhile(f))
        else Empty
      case _ => Empty
    }

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = {

    }

    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    }
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = {
      if (as.isEmpty) empty
      else cons(as.head, apply(as.tail: _*))
    }
  }

  def ex5_1(): Unit = {
    println(Stream(0 until 10: _*).toList)
  }

  //  ex5_1()

  def ex5_2(): Unit = {
    println(Stream(0 until 10: _*).take(3).toList)
    println(Stream(0 until 10: _*).drop(3).toList)
  }

  //  ex5_2()

  def ex5_3(): Unit = {
    println(Stream(0 until 10: _*).takeWhile(_ < 4).toList)
  }

//  ex5_3()
}
