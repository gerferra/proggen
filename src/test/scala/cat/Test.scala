package cat


object Test {

  val arrow1: Set.Arrow[Int, Char] = i => i.toChar

  val arrow2: Set.Arrow[Char, String] = c  => c.toString * 3

  val arrow1_2: Set.Arrow[Int, String] = Set.compose(arrow2, arrow1)

  class A
  class B
  class C

  val arrow3: Set.Arrow[A, B] = a => new B

  val arrow4: Set.Arrow[B, C] = b  => new C

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

  /*
   * somewhat works for the instances `one`, `two` and `three`
   */
  object IntPreOrd extends PreOrd {
    
    type S = Integer

    sealed trait LessThan[A <: Integer, B <: Integer]
    
    val one = 1: Integer
    val two = 2: Integer
    val three = 3: Integer

    object OneTwo extends LessThan[one.type, two.type] 
    
    object TwoThree extends LessThan[two.type, three.type]

    def trans[A <: Integer, B <: Integer, C <: Integer](
        a: LessThan[A,B],
        b: LessThan[B,C]): LessThan[A,C] = new LessThan[A, C] {}
    
    def reflex[A <: Integer]: LessThan[A,A] = new LessThan[A, A] {}

  }
  
  sealed trait PNum
  object Zero extends PNum
  case class Succ[N <: PNum](pred: N) extends PNum
  
  val One = Succ(Zero)
  val Two = Succ(One)
  val Three = Succ(Two)
  val Four = Succ(Three)
  
  object PeanoNumPreOrd extends PreOrd {
    
    type S = PNum

    sealed trait LessThan[A <: PNum, B <: PNum]
    
    def trans[A <: PNum, B <: PNum, C <: PNum](
        a: LessThan[A,B],
        b: LessThan[B,C]): LessThan[A,C] = new LessThan[A, C] { }
    
    def reflex[A <: PNum]: LessThan[A,A] = new LessThan[A, A] { }

    implicit def ZeroLTEqAll[Z <: Zero.type, N <: PNum]: LessThan[Z, N] = new LessThan[Z, N] {}
    
    def lteq2[N1 <: PNum, N2 <: PNum, A <: Succ[N1], B <: Succ[N2]](implicit ev: LessThan[N1, N2]): LessThan[A, B] = new LessThan[A, B] {}

    def lteq2[N1 <: PNum, N2 <: PNum](a: Succ[N1], b: Succ[N2])(implicit ev: LessThan[N1, N2]): LessThan[Succ[N1], Succ[N2]] = new LessThan[Succ[N1], Succ[N2]] {}
    
    def lteq[A <: PNum, B <: PNum](implicit ev: LessThan[A, B]): LessThan[A, B] = ev
  }

}