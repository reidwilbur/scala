package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

case class State[S,+A](run: S => (A,S)) {
  def map[B](f: A => B): State[S,B] =
    flatMap(a => State.unit(f(a)))
  def map2[B,C](s: State[S,B])(f: (A,B) => C): State[S,C] =
    flatMap(a => s.map{ b => f(a,b) })
  def flatMap[B](g: A => State[S,B]): State[S,B] = 
    State(s => {
      val (a, s1) = run(s)
      g(a).run(s1)
    })
}

object RNG {
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
    //rng => {
    //  val (a, rng2) = s(rng)
    //  (f(a), rng2)
    //}
    flatMap(s){ a => rng => (f(a), rng) }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, rng2) = rng.nextInt
    (if (i < 0) -(i + 1) else i, rng2)
  }

  def double: Rand[Double] = 
    map(nonNegativeInt)(i => i.toDouble/(Int.MaxValue.toDouble+1))

  def intDouble: Rand[(Int, Double)] = 
    map2(int, double)((_, _))

  def doubleInt: Rand[(Double, Int)] = 
    map2(double, int)((_, _))

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)

    ((d1,d2,d3), rng4)
  }

  def ints(count: Int): Rand[List[Int]] = 
  //(rng: RNG): (List[Int], RNG) = 
  //{
  //  def genInts(ccount: Int, l: List[Int], rng: RNG): (List[Int], RNG)  = 
  //    if (ccount == 0) (l, rng) 
  //    else {
  //      val (i, nrng) = rng.nextInt
  //      genInts(ccount-1, i :: l, nrng)
  //    }
  //  genInts(count, Nil, rng)
  //}
    sequence(List.fill(count)(int))

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = 
    //rng => {
    //  val (a, arng) = ra(rng)
    //  val (b, brng) = rb(arng)
    //  (f(a,b), brng)
    //}
    flatMap(ra){ a => map(rb){ b => f(a,b) } }
   
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = 
    map2(ra, rb)((_, _))

  def sequence[A](ra: List[Rand[A]]): Rand[List[A]] = 
    rng => {
      ra.foldRight((Nil:List[A], rng)){ 
        (rnd, l_rng) => 
          val (a, rng1) = rnd(l_rng._2)
          (a :: l_rng._1, rng1)
      }
    }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt){ i: Int =>
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        rng => (mod, rng)
      else
        nonNegativeLessThan(n)
    }
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
    rng => {
      val (a, rng1) = f(rng)
      g(a)(rng1)
    }
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S,A](a: A): State[S,A] =
    State(s => (a,s))
}
