package cat

import language.higherKinds

trait Cat {

  type Obj

  type Arrow[A <: Obj, B <: Obj]

  def compose[A <: Obj, B <: Obj, C <: Obj](
    g: Arrow[B, C], 
    f: Arrow[A, B]): Arrow[A, C]

  def identity[A <: Obj]: Arrow[A, A]

}

object Set extends Cat {

  type Obj = Any

  type Arrow[A <: Obj, B <: Obj] = A => B

  def compose[A <: Obj, B <: Obj, C <: Obj](
    g: Arrow[B, C], 
    f: Arrow[A, B]): Arrow[A, C] = g compose f

  def identity[A <: Obj]: Arrow[A, A] = Predef.identity[A]

}

object pSet extends Cat {

  type Obj = Any

  type Arrow[A <: Obj, B <: Obj] = PartialFunction[A, B]

  def compose[A <: Obj, B <: Obj, C <: Obj](
    g: Arrow[B, C], 
    f: Arrow[A, B]): Arrow[A, C] = {
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

object Prod {
  // type alias at package level: type Pair[+A, +B] = Prod.Pair[A, B]
  class Pair[+A, +B](_1: A, _2: B) extends Tuple2[A, B](_1, _2) {
    type _1 = A
    type _2 = B
  }

}

case class Prod[C <: Cat, D <: Cat](c: C, d: D) extends Cat {
  
  type Obj = Pair[c.Obj, d.Obj]

  trait Arrow[A <: Obj, B <: Obj] {

    // needed to avoid infer c.Obj and d.Obj in Arrow.apply don't know why...
    type A1 = A#_1 

    type A2 = A#_2

    type B1 = B#_1

    type B2 = B#_2

    def _1: c.Arrow[A1, B1]

    def _2: d.Arrow[A2, B2]

    override def toString = s"Arrow(${_1}, ${_2})"
  }

  object Arrow {
      def apply[A1 <: c.Obj, A2 <: d.Obj, B1 <: c.Obj, B2 <: d.Obj](
        f: c.Arrow[A1, B1],
        g: d.Arrow[A2, B2]): Arrow[Pair[A1, A2], Pair[B1, B2]] = 
        new Arrow[Pair[A1, A2], Pair[B1, B2]] {

          // can't make it work without a cast... 
          def _1: c.Arrow[A1, B1] = f.asInstanceOf[c.Arrow[A1, B1]]
          
          def _2: d.Arrow[A2, B2] = g.asInstanceOf[d.Arrow[A2, B2]]

        }
    }

  def compose[A <: Obj, B <: Obj, C <: Obj](
    g: Arrow[B, C], 
    f: Arrow[A, B]): Arrow[A, C] =
    new Arrow[A, C] {
      
      def _1: c.Arrow[A#_1, C#_1] = c.compose(g._1, f._1)
      
      def _2: d.Arrow[A#_2, C#_2] = d.compose(g._2, f._2)
    
    }

  def identity[A <: Obj]: Arrow[A, A] = 
    new Arrow[A, A] {
      
      def _1: c.Arrow[A#_1, A#_1] = c.identity[A#_1]
      
      def _2: d.Arrow[A#_2, A#_2] = d.identity[A#_2]

    }

}

case class Dual[C <: Cat](c: C) extends Cat {

  type Obj = c.Obj

  type Arrow[A <: Obj, B <: Obj] = c.Arrow[B, A]

  def compose[A <: Obj, B <: Obj, C <: Obj](g: Arrow[B, C], f: Arrow[A, B]): Arrow[A, C] =
    c.compose(f, g)

  def identity[A <: Obj]: Arrow[A, A] = c.identity

}


trait PreOrd[S] {

  type LessThan[A <: S, B <: S]

  def reflex[A <: S]: LessThan[A, A]

  def trans[A <: S, B <: S, C <: S](
    a: LessThan[A, B],
    b: LessThan[B, C]): LessThan[A, C]  

}

case class PreCat[S](s: PreOrd[S]) extends Cat {

  type Obj = S

  type Arrow[A <: Obj, B <: Obj] = s.LessThan[A, B]

  def compose[A <: Obj, B <: Obj, C <: Obj](
    g: Arrow[B, C], 
    f: Arrow[A, B]): Arrow[A, C] =
    s.trans(f, g)

  def identity[A <: Obj]: Arrow[A, A] = s.reflex

}
