package cat

object Test {

  val arrow1: Set.Arrow[Int, Char] = i => i.toChar

  val arrow2: Set.Arrow[Char, String] = c => c.toString * 3

  val arrow1_2: Set.Arrow[Int, String] = Set.compose(arrow2, arrow1)

  class A
  class B
  class C

  val arrow3: Set.Arrow[A, B] = a => new B

  val arrow4: Set.Arrow[B, C] = b => new C

  val arrow3_4: Set.Arrow[A, C] = Set.compose(arrow4, arrow3)

  val sXs = Prod(Set, Set)

  val arrow13: sXs.Arrow[Pair[Int, A], Pair[Char, B]] =
    sXs.Arrow(arrow1, arrow3)

  val pId: sXs.Arrow[Pair[Char, B], Pair[Char, B]] =
    sXs.identity[Pair[Char, B]]

  val arrow13_pId: sXs.Arrow[Pair[Int, A], Pair[Char, B]] =
    sXs.compose(pId, arrow13)

  val arrow24: sXs.Arrow[Pair[Char, B], Pair[String, C]] =
    sXs.Arrow(arrow2, arrow4)

  val arrow13_24: sXs.Arrow[Pair[Int, A], Pair[String, C]] =
    sXs.compose(arrow24, arrow13)

  sealed trait PNum
  sealed trait Zero extends PNum
  sealed trait Succ[N <: PNum] extends PNum

  type One = Succ[Zero]
  type Two = Succ[One]
  type Three = Succ[Two]
  type Four = Succ[Three]

  object PNumPreOrd extends PreOrd[PNum] {

    //type S = PNum

    case class LTEq[A <: PNum, B <: PNum] private[PNumPreOrd] ()

    def trans[A <: PNum, B <: PNum, C <: PNum](
      a: LTEq[A, B],
      b: LTEq[B, C]): LTEq[A, C] = LTEq[A, C]()

    def reflex[A <: PNum]: LTEq[A, A] = LTEq[A, A]()

    implicit def ZeroLTEqAll[N <: PNum]: LTEq[Zero, N] = LTEq[Zero, N]()

    implicit def SuccPreserverLTEq[N1 <: PNum, N2 <: PNum](implicit ev: LTEq[N1, N2]): LTEq[Succ[N1], Succ[N2]] = LTEq[Succ[N1], Succ[N2]]()

    def relate[A <: PNum, B <: PNum](implicit ev: LTEq[A, B]): LTEq[A, B] = LTEq[A, B]()
  }

  import PNumPreOrd.{ LTEq, relate }

  val zeroTwo: LTEq[Zero, Two] = relate[Zero, Two]
  val twoFour: LTEq[Two, Four] = relate[Two, Four]
  
  val zeroFour: LTEq[Zero, Four] = PNumPreOrd.trans(zeroTwo, twoFour)
  
  val pcat = PreCat(PNumPreOrd)
  
  val zeroZero: pcat.Arrow[Zero, Zero] = pcat.identity[Zero]
  
  // this does not work....
  //val zeroTwoArrow: pcat.Arrow[Zero, Two] = zeroTwo
  //val twoFourArrow: pcat.Arrow[Two, Four] = twoFour
  
  // but this apparently yes.... no idea why...
  val zeroTwoArrow: pcat.Arrow[Zero, Two] = zeroTwo.asInstanceOf[pcat.Arrow[Zero, Two]]
  val twoFourArrow: pcat.Arrow[Two, Four] = twoFour.asInstanceOf[pcat.Arrow[Two, Four]]
  
  val zeroFourArrow = pcat.compose(twoFourArrow, zeroTwoArrow)
}