package gen

import language.{ higherKinds, implicitConversions }

import func.Functors._

object Regular {

  sealed trait Regular[A, B]

  case class K[A, B, T](t: T) extends Regular[A, B]

  final class U[A, B] extends K[A, B, Unit] {
    override def equals(other: Any) = other.isInstanceOf[U[_,_]]
  }
  object U {
    def apply[A, B](): U[A, B] = new U
    def unapply[A, B](u: U[A, B]): Option[(Unit)] = Some(())
  }

  case class Par[A, B](a: A) extends Regular[A, B]

  case class Rec[A, B](r: B) extends Regular[A, B]

  case class x[A, B](f: Regular[A, B], g: Regular[A, B]) extends Regular[A, B]

  sealed trait +[A, B, F <: Regular[A, B], G <: Regular[A, B]] extends Regular[A, B]
  case class Left[A, B, F <: Regular[A, B], G <: Regular[A, B]](f: F) extends +[A, B, F, G]
  case class Right[A, B, F <: Regular[A, B], G <: Regular[A, B]](g: G) extends +[A, B, F, G]


/*
  trait AppFunc2 {
    
    type A = Int
    type B = Int

    type F = Regular[A, B] // Regular[Int, Rose[Int]]

    type D[X] = List[X]

    def functor: Functor[D]

    def value: D[F]

    def h(f: Regular[A, B]): B

    def func: D[F] => D[B] = functor.fmap(h)

    def tmp: D[B] = func(value)

    def res: B = fold(h)(tmp)
  }

  */

  //case class AppFunc3[A, B, D[_]](functor: Functor[D], value: D[Regular[A, B]])(implicit DIsRegular: D[B] => Regular[B, D[B]]) extends Regular[A, B] {
  trait AppFunc3[A, B] extends Regular[A, B] {

    type D[X]

    def functor: Functor[D]

    def value: D[Regular[A, B]]

    def asRegularB(d: D[B]): Regular[B, D[B]]
    def asRegularA(d: D[A]): Regular[A, D[A]]

  }

  object AppFunc3 {

    def unapply[A, B](a: AppFunc3[A, B]): Option[(Functor[a.D], a.D[Regular[A,B]])] = Some(a.functor, a.value)

    def apply[A, B, D1[_]](
      f: Functor[D1], 
      v: D1[Regular[A, B]], 
      convB: D1[B] => Regular[B, D1[B]],
      convA: D1[A] => Regular[A, D1[A]]): AppFunc3[A, B] = 
      new AppFunc3[A, B] {

        type D[X] = D1[X]

        def functor: Functor[D] = f

        def value: D[Regular[A, B]] = v

        def asRegularA(d: D[A]): Regular[A, D[A]] = convA(d)
        def asRegularB(d: D[B]): Regular[B, D[B]] = convB(d)

      }
  }


/*
  case class AppFunc[A, B, F <: Regular[A, B], D[_]](f: Functor[D], d: D[F])(
      implicit val ev: D[A] => Regular[A, D[A]],
               val ev2: B => Regular[A, B]) extends Regular[A, B] {
    /* stinky ... */
    type DD[X] = D[X]
    type BB = B
    type FF = F
  }

  */



  def fold[A, D[_], B](h: Regular[A, B] => B)(r: Regular[A, D[A]])(implicit ev: D[A] => Regular[A, D[A]]): B = {
    r match {
      case U() => h(U())
      case K(t) => h(K(t))
      case Par(a) => h(Par(a))
      case Rec(r) => h(Rec(fold(h)(r)))
      case f x g  => 
        val a = fold(h)(f)
        val b = fold(h)(g)
        h(x(Rec(a), Rec(b)))

      case s : +[A @unchecked, D[A] @unchecked, _, _]  => s match {
        case Left(f) => fold(h)(f)
        case Right(g) => fold(h)(g)
      }

      //case a @ AppFunc3(f, d) =>
        // moar bizarre things ...
        //???
        /*
        implicit val ev2: a.BB => Regular[A, a.BB] = a.ev2
        val i: a.DD[B] = f.fmap{ x: Regular[A, a.BB] => fold(h)(x)(ev2)}(d)

        implicit val ev: a.DD[A] => Regular[A, a.DD[A]] = a.ev
        h(AppFunc3[A, a.BB, Regular[A, a.BB], a.DD](f, i).asInstanceOf[AppFunc3[A, B, Regular[A, B], a.DD]])
        */
    }
  }

  def Zero[X]: X => Int = x => 0

  def fmax(f: Regular[Int, Int]): Int = {
    f match {
      case U() => 0
      case K(t) => Zero(t)
      case Par(a) => a
      case Rec(r) => r
      case f x g  => math.max(fmax(f), fmax(g))
      case s : +[Int @unchecked, Int @unchecked, _, _]  => s match {
        case Left(f) => fmax(f)
        case Right(g) => fmax(g)
      }
      
      case a: AppFunc3[Int, Int] =>
        
        type A = Int
        type B = Int

        type F = Regular[A, B] // Regular[Int, Rose[Int]]

        val functor: Functor[a.D] = a.functor // Functor[List]
        
        val value: a.D[F] = a.value // List[Regular[Int, Rose[Int]]] ~~ List[Rose[Int]]

        def h(f: Regular[A, B]): B = fmax(f)

        def func: a.D[F] => a.D[B] = functor.fmap(h)

        val tmp: a.D[B] = func(value) // List[Int]

        def res: B = fold(h)(a.asRegularB(tmp))(a.asRegularA) 

        res

    }
  }

  def pmax[D[_]](r: Regular[Int, D[Int]])(implicit ev: D[Int] => Regular[Int, D[Int]]): Int = 
    fold(fmax)(r)(ev)



  implicit def ListIsRegular[A](l: List[A]): Regular[A, List[A]] = 
    l match {
      case Nil => 
        Left(U[A, List[A]]())
      case a :: as => 
        Right(x[A, List[A]](Par(a), Rec(as)))
    }

  val listPF: Regular[Int, List[Int]] = List(1,2,3)

  val nat: Int = pmax(listPF)


  sealed trait Btree[+A]
  object Empty extends Btree[Nothing]
  case class Node[A](left: Btree[A], value: A, right: Btree[A]) extends Btree[A]

  val tree = Node(Empty, 8, Node(Node(Empty, 7, Empty), 6, Empty))

  implicit def BTreeIsRegular[A](b: Btree[A]): Regular[A, Btree[A]] = 
    b match {
      case Empty => 
        Left(U[A, Btree[A]]())
      case Node(left, a, right) => 
        val r = x[A, Btree[A]](Par(a), Rec(right))
        val l = Rec[A, Btree[A]](left)
        Right(x[A, Btree[A]](l, r))
    }
  
  val tmax: Int = pmax(tree)  

  sealed trait Rose[+A]
  case class Fork[A](a: A, childs: List[Rose[A]]) extends Rose[A] 
  // R_A[B]       = A x List[B]
  // R_A[Mu[R_A]] = A x List[Mu[R_A]] = A x List[Rose[A]]
  // R_A[Int]     = A x List[Nat]

  val rose = Fork(1, List(Fork(2, Nil), Fork(4, List(Fork(5, Nil))), Fork(3, Nil)))

  object ListFunc extends Functor[List] {
    def fmap[A, B](f: A => B): List[A] => List[B] = { l =>
      l.map(f)
    }
      
  }

  implicit def RoseIsRegular[A]
    (r: Rose[A])
    (implicit 
      evA: List[A] => Regular[A, List[A]],
      evB: List[Rose[A]] => Regular[Rose[A], List[Rose[A]]]): Regular[A, Rose[A]] = 
    r match {
      case Fork(a, childs) => 
        val par = Par[A, Rose[A]](a)
        val func = 
          AppFunc3[A, Rose[A], List](
            ListFunc, 
            childs.map(x => RoseIsRegular(x)(evA, evB)), 
            evB,
            evA)
        
        val prod = x(par, func)
        prod
    }

  // don't work ... ClassCastException ... val trose: Int = pmax(rose)  
}