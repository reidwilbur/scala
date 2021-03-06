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
    flatMap{ a => State.unit(f(a)) }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap{ a => sb.map{ b => f(a, b) } }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State[S,B]( s => {
      val (a, ns) = run(s)
      f(a).run(ns)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S,A](a: A): State[S,A] = State(s => (a, s))

  def sequence[S,A](fs: List[State[S,A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List()))( (f, acc) => f.map2(acc)(_ :: _))

  def sequence_r[S,A](fs: List[State[S,A]]): State[S, List[A]] = {
    def build(state: S, actions: List[State[S,A]], acc: List[A]): (List[A], S) = {
      actions match {
        case Nil => (acc.reverse, state)
        case action :: t =>
          val (a, nextState) = action.run(state)
          build(nextState, t, a :: acc)
      }
    }
    State[S,List[A]](s => { build(s, fs, List[A]()) } )
  }

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def simulateMachineCombis(inputs: List[Input]): State[Machine, (Int, Int)] =
      for {
        _ <- sequence(inputs.map(i => modify((m:Machine) => (i, m) match {
          case (_, Machine(_, 0, _)) =>
            println("machine empty "+m)
            m
          case (Coin, Machine(false, _, _)) =>
            println("machine ignoring coin "+m)
            m
          case (Turn, Machine(true, _, _)) =>
            println("machine ignoring turn "+m)
            m
          case (Coin, Machine(true, candies, coins)) =>
            val mm = Machine(false, candies, coins+1)
            println("machine unlocked "+mm)
            mm
          case (Turn, Machine(false, candies, coins)) =>
            val mm = Machine(true, candies-1, coins)
            println("machine dispense "+mm)
            mm
        })))
        s <- get
      } yield(s.coins, s.candies)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State[Machine, (Int, Int)]( s => {
        inputs.foldLeft( ((s.coins, s.candies), s) )( (acc, input) => {
          val machine = acc._2
          (input, machine) match {
            case (Coin, Machine(_, candies, coins)) =>
              val m = Machine(false, candies, coins+1)
              ((m.coins, m.candies), m)

            case (Turn, Machine(_, 0, _)) =>
              ((machine.coins, machine.candies), machine)

            case (Turn, Machine(false, candies, coins)) =>
              val m = Machine(true, candies-1, coins)
              ((m.coins, m.candies), m)

            case (_, m) =>
              ((m.coins, m.candies), m)
          }
        })
      })
}
