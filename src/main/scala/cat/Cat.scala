package cat

import language.higherKinds

trait Cat {

  type Obj

  type Arrow[A <: Obj, B <: Obj]

  def compose[A <: Obj, B <: Obj, C <: Obj](g: Arrow[B, C], f: Arrow[A, B]): Arrow[A, C]

  def identity[A <: Obj]: Arrow[A, A]

}

object Set extends Cat {

  type Obj = Any

  type Arrow[-A <: Obj, +B <: Obj] = (A => B)

  def compose[A <: Obj, B <: Obj, C <: Obj](g: Arrow[B, C], f: Arrow[A, B]): Arrow[A, C] =
    g.compose(f)

  def identity[A <: Obj]: Arrow[A, A] = Predef.identity[A]

}

object pSet extends Cat {

  type Obj = Any

  type Arrow[-A <: Obj, +B <: Obj] = PartialFunction[A, B]

  def compose[A <: Obj, B <: Obj, C <: Obj](g: Arrow[B, C], f: Arrow[A, B]): Arrow[A, C] = {
    /*
     * composition of Scala PartialFunctions does not return PartialFunction, not sure why...
     * `g.compose(f)` does not work... 
     */
    new PartialFunction[A, C] {

      def isDefinedAt(a: A) = if (f.isDefinedAt(a)) g.isDefinedAt(f(a)) else false

      def apply(a: A) = g(f(a))

    }
  }

  def identity[A <: Obj]: Arrow[A, A] = { case a => a }

}

case class Prod[C <: Cat, D <: Cat](c: C, d: D) extends Cat {

  type Obj = Pair[c.Obj, d.Obj]

  case class Arrow[A <: Obj, B <: Obj](_1: c.Arrow[A#_1, B#_1], _2: d.Arrow[A#_2, B#_2])

  def compose[A <: Obj, B <: Obj, C <: Obj](g: Arrow[B, C], f: Arrow[A, B]): Arrow[A, C] =
    Arrow(c.compose(g._1, f._1), d.compose(g._2, f._2))

  def identity[A <: Obj]: Arrow[A, A] = Arrow(c.identity[A#_1], d.identity[A#_2])

}

case class Dual[C <: Cat](c: C) extends Cat {

  type Obj = c.Obj

  type Arrow[A <: Obj, B <: Obj] = c.Arrow[B, A]

  def compose[A <: Obj, B <: Obj, C <: Obj](g: Arrow[B, C], f: Arrow[A, B]): Arrow[A, C] =
    c.compose(f, g)

  def identity[A <: Obj]: Arrow[A, A] = c.identity

}





