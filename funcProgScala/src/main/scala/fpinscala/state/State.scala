package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
//    rng => {
//      val (a, rng2) = s(rng)
//      (f(a), rng2)
//    }
      RNG.flatMap(s){ a => unit(f(a)) }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    if (i < 0) (-1*(i+1), rng2) else (i, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    map(nonNegativeInt){ nni => nni.toDouble/(Integer.MAX_VALUE.toDouble +1) }(rng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i,d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (id, rng2) = intDouble(rng)
    ((id._2, id._1), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    def gen(rng: RNG, ds: List[Double]): ((Double,Double,Double), RNG) = {
      if (ds.size < 3) {
        val (d, rng2) = double(rng);
        gen(rng2, d :: ds);
      }
      else {
        ((ds.head, ds.tail.head, ds.tail.tail.head), rng)
      }
    }

    gen(rng, Nil)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def gen(ints: List[Int], rrng: RNG): (List[Int], RNG) = {
      if (ints.size == count) {
        (ints, rrng)
      }
      else {
        val (i , rrrng) = rrng.nextInt
        gen(i :: ints, rrrng)
      }
    }

    gen(Nil, rng)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
//    rng => {
//      val (a, rnga) = ra(rng)
//      map(rb){ b => f(a,b) }(rnga)
//    }
    RNG.flatMap(ra){ a => RNG.map(rb){ b => f(a,b) }}

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => {
      fs.foldRight((List[A](), rng)) {
        (rand, state) =>
          val (a, nextrng) = rand(state._2)
          (a :: state._1, nextrng)
      }
    }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, nrng) = f(rng)
      g(a)(nrng)
    }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
