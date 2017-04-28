package fp.design.chapter5

import scala.annotation.tailrec

object Main extends App {

  import Stream._

  trait Stream[+A] {
    def headOption1: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Option(h())
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

    def takeWhile1(f: A => Boolean): Stream[A] = this match {
      case Cons(h, t) =>
        if (f(h())) cons(h(), t().takeWhile(f))
        else Empty
      case _ => Empty
    }

    def exists1(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

    def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

    def takeWhile(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else empty[A])

    def headOption: Option[A] = foldRight[Option[A]](None)((a, b) => Option(a).orElse(b))

    def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

    def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

    def append[B >: A](x: => Stream[B]): Stream[B] = foldRight(x)((a, b) => cons(a, b))

    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a) append b)

    def find(p: A => Boolean): Option[A] = filter(p).headOption

    def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, s2)){case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))}

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

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z) match {
        case Some((a, b)) => cons(a, unfold(b)(f))
        case None => empty
      }
    }

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

  def ex5_4(): Unit = {
    println(Stream(0 until 10: _*).forAll(p => {
      println(p); p < 3
    }))
    println(Stream(0 until 10: _*).forAll(p => {
      println(p); p < 100
    }))
  }

  //  ex5_4()

  def ex5_5(): Unit = {
    println(Stream(0 until 10: _*).takeWhile(_ < 4).toList)
  }

  //  ex5_5()

  def ex5_6(): Unit = {
    println(Stream(null, 0, 1, 2, 3).headOption1)
    println(Stream(null, 0, 1, 2, 3).headOption)
  }

  //  ex5_6()

  def ex5_7(): Unit = {
    println(Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList)
    println(cons(11, Stream(2, 3, 4).map(_ + 10)).filter(_ % 2 == 0).toList)
    println(Stream(2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList)
    println(cons(12, Stream(3, 4)).map(_ + 10).filter(_ % 2 == 0).toList)
    println(12 :: Stream(3, 4).map(_ + 10).filter(_ % 2 == 0).toList)
    println(12 :: cons(13, Stream(4).map(_ + 10)).filter(_ % 2 == 0).toList)
    println(12 :: Stream(4).map(_ + 10).filter(_ % 2 == 0).toList)
    println(12 :: cons(14, Stream[Int]().map(_ + 10)).filter(_ % 2 == 0).toList)
    println(12 :: 14 :: Stream[Int]().map(_ + 10).filter(_ % 2 == 0).toList)
    println(12 :: 14 :: List())
  }

  //  ex5_7()

  //  val ones: Stream[Int] = cons(1, ones)
  //  println(ones.take(5).toList)
  //  println(ones.exists(_ % 2 != 0))

  def ex5_8(): Unit = {
    def constant[A](a: A): Stream[A] = cons(a, constant(a))
    println(constant(5).take(5).toList)
  }

//  ex5_8()

  def ex5_9(): Unit = {
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))
    println(from(1).take(20).toList)
  }

//  ex5_9()

  def ex5_10(): Unit = {
    def fibs(): Stream[Int] = {
      def lfibs(n: Int, m: Int): Stream[Int] = cons(n, lfibs(m, m + n))
      lfibs(0, 1)
    }
    println(fibs().take(10).toList)
  }

//  ex5_10()

  def ex5_11(): Unit = {
    println(unfold(1)(s => Some(s, s + 1)).take(20).toList)
  }
//  ex5_11()

  def ex5_12(): Unit = {
    def fibs: Stream[Int] = unfold((0, 1)){case (s: Int, t: Int) => Some((s, (t, s + t)))}
    println(fibs.take(20).toList)
    def from(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))
    println(from(1).take(20).toList)
    def constant[A](a: A): Stream[A] = unfold(a)(s => Some(s, s))
    println(constant(2).take(20).toList)
    def ones: Stream[Int] = unfold(1)(s => Some(s, s))
    println(ones.take(20).toList)
  }

//  ex5_12()
  def ex5_13(): Unit = {
  }
}
