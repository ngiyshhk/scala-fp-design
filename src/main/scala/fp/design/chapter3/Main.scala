package fp.design.chapter3

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }
}

object Main extends App {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }
  def product(ints: List[Double]): Double = ints match {
    case Nil => 1
    case Cons(0.0, _) => 0.0
    case Cons(h, t) => h * product(t)
  }
  // ex3_2
//  def tail[A](xs: List[A]): List[A] = xs match {
//    case Nil => throw new IllegalArgumentException("empty list!")
//    case Cons(_, t) => t
//  }
  // ex3_3
  def setHead[A](nh: A, xs: List[A]): List[A] = xs match {
    case Nil => throw new IllegalArgumentException("empty list!")
    case Cons(_, t) => Cons(nh, t)
  }
  // ex3_4
  def drop[A](l: List[A], n: Int): List[A] = (l, n - 1) match {
    case (Nil, _) => throw new IllegalArgumentException("empty list!")
    case (Cons(_, t), 0) => t
    case (Cons(_, t), nn) => drop(t, nn)
  }
  def tail[A](xs: List[A]): List[A] = drop(xs, 1)
  // ex3_5
//  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
//    case Nil => throw new IllegalArgumentException("empty list!")
//    case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
//  }
  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }
  }
  // ex3_6
  // tailと違って引数のリストサイズ分くらいinitが呼ばれる
  def init[A](xs: List[A]): List[A] = xs match {
    case Nil => throw new IllegalArgumentException("empty list!")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) dropWhile(t)(f) else l
  }
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }
  def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)
  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)
  // ex3_9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, y) => y + 1)
  // ex3_10
  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }
  // ex3_11
  def sumLeft(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def productLeft(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
  def lengthLeft[A](ns: List[A]) = foldLeft(ns, 0)((acc, _) => acc + 1)
  // ex3_12
  def reverse[A](ns: List[A]) = foldLeft[A, List[A]](ns, Nil)((acc, x) => Cons(x, acc))
  // ex3_13
  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(as), z)((a: A, b: B) => f(b, a))
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b: B, a: A) => f(a, b))
  // ex3_14
  def appendLeft[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)((acc, o) => Cons(o, acc))
  def appendRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((o, acc) => Cons(o, acc))
  // ex3_15
  def concat[A](listList: List[List[A]]): List[A] = foldRight(listList, Nil: List[A])(append)
  // ex3_16
  def addOnes(list: List[Int]): List[Int] = list match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, addOnes(t))
  }
  // ex3_17
  def doublesToStrings(list: List[Double]): List[String] = list match {
    case Nil => Nil
    case Cons(h, t) => Cons(h.toString, doublesToStrings(t))
  }
  // ex3_18
  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }
  // ex3_19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
  }
  // ex3_20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(h, t) => append(f(h), flatMap(t)(f))
  }
  // ex3_21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)((a: A) => if (f(a)) List(a) else Nil)
  // ex3_22
  def zipAdd(a1: List[Int], a2: List[Int]): List[Int] = (a1, a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipAdd(t1, t2))
  }
  // ex3_23
  def zipWith[A, B](a1: List[A], a2: List[A])(f: (A, A) => B): List[B] = (a1, a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }
  def zipAdd2(a1: List[Int], a2: List[Int]): List[Int] = zipWith(a1, a2)((o1, o2) => o1 + o2)
  // ex3_24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Nil, _) => false
    case (_, Nil) => true
    case (Cons(_, t), _) if length(sup) >= length(sub) =>
      val zipWith1: List[Boolean] = zipWith(sup, sub)((o1, o2) => o1 == o2)
      if (length(dropWhile(zipWith1)(x => x)) == 0) true else hasSubsequence(t, sub)
    case _ => false
  }

  def ex3_1(): Unit = {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println(x)
  }

//  ex3_1()

  def ex3_2(list: List[Int]): Unit = {
    tail(list)
    println(s"time: ${System.currentTimeMillis() - executionStart}")
  }

//  ex3_2(List(1 to 1000: _*))
//  ex3_2(List(1 to 1: _*))

  def ex3_3(): Unit = {
    println(setHead(6, List(1, 2, 3)))
  }

//  ex3_3()

  def ex3_4(): Unit = {
    println(tail(List(1, 2, 3)))
  }

//  ex3_4()

  def ex3_5(): Unit = {
    println(dropWhile(List(1 to 10: _*))(_ < 4))
  }

//  ex3_5()

  def ex3_6(): Unit = {
    val list = List(1 to 1000: _*)
    val vect = Vector(1 to 1000: _*)
    val start = System.currentTimeMillis()
    init(list)
    println(s"time: ${System.currentTimeMillis() - start}")
    vect.init
    println(s"time: ${System.currentTimeMillis() - start}")
  }

  ex3_6()

  // TODO 直ちに再帰を中止して0.0を返せるか？　返せなくない？
  def ex3_7[A, B](list: List[A])(f: List[A] => B): Unit = {
    println(f(list))
    println(s"time: ${System.currentTimeMillis() - executionStart}")
  }

//  val list = Cons(0.0, List((1 to 1000).map(_.toDouble): _*))
//  ex3_7(list)(product)
//  ex3_7(list)(product2)

  // TODO foldRightとListのデータコンストラクタとの関係について何を表しているの？
  def ex3_8(): Unit = {
    println(foldRight(List(1, 2, 3), Nil: List[Int])((x, y) => Cons(x, y)))
  }

//  ex3_8()

  def ex3_9(): Unit = {
    println(length(List(1 to 100: _*)))
  }

//  ex3_9()

  def ex3_10(): Unit = {
    println(foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _)))
    println(foldLeft(List(1, 2, 3), Nil:List[Int])((acc, x) => Cons(x, acc)))
  }

//  ex3_10()

  def ex3_11() = {
    println(sumLeft(List(1 to 10: _*)))
    println(productLeft(List((1 to 10).map(_.toDouble): _*)))
    println(lengthLeft(List(1 to 10: _*)))
  }

//  ex3_11()

  def ex3_12() = {
    println(reverse(List(1 to 10: _*)))
  }

//  ex3_12()

  def ex3_13() = {
    println(foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _)))
    println(foldRight2(List(1, 2, 3), Nil:List[Int])(Cons(_, _)))
    println(foldLeft(List(1, 2, 3), Nil:List[Int])((acc, x) => Cons(x, acc)))
    println(foldLeft2(List(1, 2, 3), Nil:List[Int])((acc, x) => Cons(x, acc)))
  }

//  ex3_13()

  def ex3_14() = {
    println(appendLeft(List(1, 2, 3), List(4, 5, 6)))
    println(appendRight(List(1, 2, 3), List(4, 5, 6)))
  }

//  ex3_14()

  def ex3_15() = {
    println(concat(List(List(1, 2), List(3, 4), List(5, 6))))
  }

//  ex3_15()

  def ex3_16() = {
    println(addOnes(List(1, 2, 3)))
  }

//  ex3_16()

  def ex3_17() = {
    println(doublesToStrings(List(1.0, 2.0, 3.0)))
  }

//  ex3_17()

  def ex3_18() = {
    println(map(List(1, 2, 3))(_ + 1))
  }

//  ex3_18()

  def ex3_19() = {
    println(filter(List(1 to 10: _*))(_ % 2 == 0))
  }

//  ex3_19()

  def ex3_20() = {
    println(flatMap(List(1, 2, 3))(i => List(i, i)))
  }

//  ex3_20()

  def ex3_21() = {
    println(filter2(List(1 to 10: _*))(_ % 2 == 0))
  }

//  ex3_21()

  def ex3_22() = {
    println(zipAdd(List(1, 2, 3), List(4, 5, 6)))
  }

//  ex3_22()

  def ex3_23() = {
    println(zipAdd2(List(1, 2, 3), List(4, 5, 6)))
  }

//  ex3_23()

  def ex3_24() = {
    println(hasSubsequence(List(1,2,3,4,5), List(1,2))) // true
    println(hasSubsequence(List(1,2,3,4,5), List(1,3))) // false
    println(hasSubsequence(List(1,2,3,4,5), List(2,3))) // true
    println(hasSubsequence(List(1,2,3,4,5), List(5)))   // true
    println(hasSubsequence(List(1,2,3,4,5), List(5,3))) // false
  }

//  ex3_24()

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // ex3_25
  def size[A](ts: Tree[A]): Int = ts match {
    case Leaf(v) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }
  // ex3_26
  def maximum(ts: Tree[Int]): Int = ts match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }
  // ex3_27
  def depth[A](ts: Tree[A]): Int = ts match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }
  // ex3_28
  def map[A, B](ts: Tree[A])(f: A => B): Tree[B] = ts match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
  // ex3_29
  // TODO fold関数とListの左畳み込みおよび右畳み込みの間にある類似性を抽出することは可能かってなに？
  def fold[A, B](ts: Tree[A])(f: A => B)(g: (B, B) => B): B = ts match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
  def size2[A](ts: Tree[A]): Int = fold[A, Int](ts)(_ => 1)(_ + _ + 1)
  def maximum2(ts: Tree[Int]): Int = fold[Int, Int](ts)(x => x)(_ max _)
  def depth2[A](ts: Tree[A]): Int = fold(ts)(_ => 0)((x, y) => (x max y) + 1)
  def map2[A, B](ts: Tree[A])(f: A => B): Tree[B] = fold(ts)(x => Leaf(f(x)): Tree[B])(Branch(_, _))

  def ex3_25(): Unit = {
    println(size(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))))
  }

//  ex3_25()

  def ex3_26(): Unit = {
    println(maximum(Branch(Branch(Leaf(1), Leaf(9)), Branch(Leaf(7), Leaf(2)))))
  }

//  ex3_26()

  def ex3_27(): Unit = {
    println(depth(Branch(Leaf(0), Branch(Branch(Leaf(1), Leaf(9)), Branch(Leaf(7), Branch(Leaf(2), Leaf(3)))))))
    println(depth(Branch(Leaf(0), Leaf(1))))
  }

//  ex3_27()

  def ex3_28() = {
    println(map(Branch(Branch(Leaf(1), Leaf(9)), Branch(Leaf(7), Leaf(2))))(_ + 1))
  }

//  ex3_28()

  def ex3_29() = {
    println(size(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))))
    println(size2(Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))))
    println(maximum(Branch(Branch(Leaf(1), Leaf(9)), Branch(Leaf(7), Leaf(2)))))
    println(maximum2(Branch(Branch(Leaf(1), Leaf(9)), Branch(Leaf(7), Leaf(2)))))
    println(depth(Branch(Leaf(0), Branch(Branch(Leaf(1), Leaf(9)), Branch(Leaf(7), Branch(Leaf(2), Leaf(3)))))))
    println(depth2(Branch(Leaf(0), Branch(Branch(Leaf(1), Leaf(9)), Branch(Leaf(7), Branch(Leaf(2), Leaf(3)))))))
    println(map(Branch(Branch(Leaf(1), Leaf(9)), Branch(Leaf(7), Leaf(2))))(_ + 1))
    println(map2(Branch(Branch(Leaf(1), Leaf(9)), Branch(Leaf(7), Leaf(2))))(_ + 1))
  }

//  ex3_29()
}

