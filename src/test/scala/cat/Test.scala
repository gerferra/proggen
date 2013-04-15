package cat

object Test {

  val arrow1: Set.Arrow[Int, Char] = i => i.toChar

  val arrow2: Set.Arrow[Char, String] = c => c.toString * 3

  val arrow3: Set.Arrow[Int, String] = Set.compose(arrow2, arrow1)

  val setXset = Prod(Set, Set)

  val idIntString = setXset.identity[Pair[Int, String]]
  
  
  

  // esto no me convence para nada... la cosa se pone bizarra cuando los objetos no son tipos...
  trait Pre[S] {

    def lessThan(s1: S, s2: S): Boolean // hacerla entre tipos? mmmm....

  }

  case class Ord[S](implicit pre: Pre[S]) extends Cat {

    type Obj = S

    case class Arrow[A <: S, B <: S](rel: (A, B) => Boolean) {

    }

    def compose[A <: S, B <: S, C <: S](g: Arrow[B, C], f: Arrow[A, B]): Arrow[A, C] =
      Arrow((a: A, c: C) => true)

    def identity[A <: S]: Arrow[A, A] = Arrow(pre.lessThan _)
  }

}