package fp.design.chapter6

import scala.annotation.tailrec

object Main extends App {
  trait RNG {
    def nextInt: (Int, RNG)
  }
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def rollDie: Int = {
    val rng = new scala.util.Random
    rng.nextInt(6)
  }

//  val rng = SimpleRNG(42)
//  val (n1, rng2) = rng.nextInt
//  val (n2, rng3) = rng2.nextInt
//
//  println(s"$n1, $n2")

  // ex6_1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    if (n >= 0) (n, rng2)
    else ((n + 1) * -1, rng2)
  }

  // ex6_2
//  def double(rng: RNG): (Double, RNG) = {
//    val (n, rng2) = nonNegativeInt(rng)
//    (n.toDouble / Int.MaxValue.toDouble, rng2)
//  }

  // ex6_3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    val (d, rng3) = double(rng2)
    ((n, d), rng3)
  }
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, rng2) = double(rng)
    val (n, rng3) = nonNegativeInt(rng2)
    ((d, n), rng3)
  }
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d, d2, d3), rng4)
  }

  // ex6_4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = count match {
    case 0 => (Nil, rng)
    case _ =>
      val (n, rng2) = nonNegativeInt(rng)
      val (ns, rng3) = ints(count - 1)(rng2)
      (n::ns, rng3)
  }

  type Rand[+A] = RNG => (A, RNG)
  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  // ex6_5
  def double(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt)(i => i.toDouble / Int.MaxValue.toDouble)(rng)
  }

  // ex6_6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)
  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // ex6_7
  def sequence1[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng =>
      fs match {
        case x :: xs => map2(x, sequence1(xs))((a, b) => a :: b)(rng)
        case _ => (Nil, rng)
      }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(Nil: List[A]))((a, acc) => map2(a, acc)(_ :: _))
  }

  def sequenceInts(count: Int)(rng: RNG): (List[Int], RNG) = {
    val fill: List[Rand[Int]] = List.fill(count)(nonNegativeInt)
    sequence(fill)(rng)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) + mod >= 0) (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  // ex6_8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {rng =>
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def nonNegativeLessThan2(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + (n - 1) + mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }
  }

  // ex6_9
  def mapfm[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => (rng: RNG) => (f(a), rng))
  }

  def map2fm[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => flatMap(rb)(b => rng => (f(a, b), rng)))
  }

  def bothFm[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2fm(ra, rb)((_, _))

  val randIntDoubleFm: Rand[(Int, Double)] = bothFm(int, double)
  val randDoubleIntFm: Rand[(Double, Int)] = bothFm(double, int)

  def ex6_1(): Unit = {
    val rng = SimpleRNG(42)
    val (n1, rng2) = nonNegativeInt(rng)
    val (n2, rng3) = nonNegativeInt(rng2)

    println(s"$n1, $n2")
  }

//  ex6_1()

  def ex6_2(): Unit = {
    val rng = SimpleRNG(42)
    val (n1, rng2) = double(rng)
    val (n2, rng3) = double(rng2)

    println(s"$n1, $n2")
  }

//    ex6_2()

  def ex6_3(): Unit = {
    val ((n1, d1), rng2) = intDouble(SimpleRNG(43))
    println(s"$n1, $d1")
    val ((d2, n2), rng3) = doubleInt(rng2)
    println(s"$d2, $n2")
    val ((d3, d4, d5), rng4) = double3(rng3)
    println(s"$d3, $d4, $d5")
  }

//  ex6_3()

  def ex6_4(): Unit = {
    val (ns, rng2) = ints(10)(SimpleRNG(42))
    println(s"$ns")
  }

//  ex6_4()

  def ex6_7(): Unit = {
    println(ints(3)(SimpleRNG(42)))
    println(sequenceInts(3)(SimpleRNG(42)))
  }

//  ex6_7()

  def ex6_9(): Unit = {
    println(map(int)(a => a + 1)(SimpleRNG(42)))
    println(mapfm(int)(a => a + 1)(SimpleRNG(42)))
    println(randDoubleInt(SimpleRNG(42)))
    println(randDoubleIntFm(SimpleRNG(42)))
  }

  ex6_9()

}
