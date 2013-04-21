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


  object IntPreOrd extends PreOrd[Integer] {

    sealed trait LessThan[A <: Integer, B <: Integer] {
      def _1: A
      def _2: B
    }

    val one = 1: Integer
    val two = 2: Integer
    val three = 3: Integer

    object OneTwo extends LessThan[one.type, two.type] {
      def _1 = one
      def _2 = two
    }
    
    object TwoThree extends LessThan[two.type, three.type]{
      def _1 = two
      def _2 = three
    }

    def trans[A <: Integer, B <: Integer, C <: Integer](
        a: LessThan[A,B],
        b: LessThan[B,C]): LessThan[A,C] = new LessThan[A, C] {
      def _1 = a._1
      def _2 = b._2
    }
    
    // need a way to signal single elements...
    def reflex[A <: Integer]: LessThan[A,A] = new LessThan[A, A] {
      def _1 = ???
      def _2 = ???
    }

  }

}