package fp.design.chapter6

object Main2 {
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

  import State._

  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))
    def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => rb.map(b => f(a, b)))
    def flatMap[B](f: A => State[S, B]): State[S, B] = State({ st =>
      val (a, st2) = run(st)
      f(a).run(st2)
    })
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))
    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
      def go
    }
  }

  type Rand[A] = State[RNG, A]

}
