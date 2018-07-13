object TupleSum extends App {

  //  Monoids aren't démodé and thursday are the new fridays

  val l = List(
    1 -> 1,
    2 -> 3,
    3 -> 3)

  //println(l.sum)
  //Could not find implicit value for parameter num: Numeric[(Int, Int)]

  /*
   *  Ok, let's build manually our sum
   */
  val sum = ((0,0) /: l){
    case ((sum_1,sum_2), (_1, _2)) => (sum_1 + _1, sum_2 + _2)
  }

  println(sum)

  /*
   *  Generic stuff please...
   *  We need a Numeric[(A, B)]
   *
   *  First, let's get rid of some out-of-bounds methods
   */

  trait LeanNumeric[T] extends Numeric[T] {
    override def fromInt(x: Int): T = ???
    override def toInt(x: T): Int = ???
    override def minus(x: T, y: T): T = ???
    override def times(x: T, y: T): T = ???
    override def negate(x: T): T = ???
    override def toLong(x: T): Long = ???
    override def toFloat(x: T): Float = ???
    override def toDouble(x: T): Double = ???
    override def compare(x: T, y: T): Int = ???
  }

  /*
   *  And now, let's define our Numeric
   */

  implicit def numeric[A, B](
    implicit nA: Numeric[A],
    nB: Numeric[B]): Numeric[(A, B)] = {
    new LeanNumeric[(A, B)]{
      override def zero = (nA.zero, nB.zero)
      override def plus(x: (A, B), y: (A, B)): (A, B) = {
        val (a1, b1) = x
        val (a2, b2) = y
        (nA.plus(a1, a2), nB.plus(b1, b2))
      }
    }
  }

  println(l.sum) //It works

  val lt = List(
    1 -> 1 -> 3,
    2 -> 3 -> -2,
    3 -> 3 -> 2)

  println(lt.sum) //It works

  /*
   * But let's take a deeper look... which features does a monoid owe?
   *
   * Semigroup ( a1 + a2) and Zero
   *
   * Those are some of the properties of a Numeric[T]!
   *
   * Let's build our monoid
   */

  {

    import scalaz._

    implicit object IntMonoid extends Monoid[Int]{
      override def zero: Int = 0
      override def append(f1: Int, f2: => Int): Int = f1 + f2
    }

    implicit def tupleMonoid[A,B](
      implicit mA: Monoid[A],
      mB: Monoid[B]): Monoid[(A,B)] = {
      new Monoid[(A, B)] {
        override def zero: (A, B) = (mA.zero, mB.zero)
        override def append(f1: (A, B), f2: => (A, B)): (A, B) = {
          val (a1, b1) = f1
          lazy val (a2, b2) = f2
          (mA.append(a1,a2), mB.append(b1, b2))
        }
      }
    }

    implicit def numeric[T](
      implicit m: Monoid[T]): Numeric[T] = {
      new LeanNumeric[T]{
        override def zero = m.zero
        override def plus(x: T, y: T): T = m.append(x, y)
      }
    }

    println(l.sum)

    println(lt.sum) //It works

  }

}