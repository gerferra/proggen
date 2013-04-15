package cat


object Test {

  val arrow1: Set.Arrow[Int, Char] = i => i.toChar

  val arrow2: Set.Arrow[Char, String] = c => c.toString * 3

  val arrow3: Set.Arrow[Int, String] = Set.compose(arrow2, arrow1)

  
  val setXset = Prod(Set, Set)

  val idIntString = setXset.identity[Pair[Int, String]]

}