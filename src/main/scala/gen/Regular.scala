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




  trait AppFunc[A, B] extends Regular[A, B] {

    type D[X]

    def functor: Functor[D]

    def value: D[Regular[A, B]]

    /* can be generalized? */
    def asRegularDA(d: D[A]): Regular[A, D[A]]

    def asRegularDB(d: D[B]): Regular[B, D[B]]

  }

  object AppFunc {

    /* hit compiler bug while using this ... */
    def unapply[A, B](a: AppFunc[A, B]): Option[(Functor[a.D], a.D[Regular[A,B]])] = Some(a.functor, a.value)

    def apply[A, B, D1[_]](
      f: Functor[D1], 
      v: D1[Regular[A, B]], 
      convA: D1[A] => Regular[A, D1[A]],
      convB: D1[B] => Regular[B, D1[B]]): AppFunc[A, B] = 
      new AppFunc[A, B] {

        type D[X] = D1[X]

        def functor: Functor[D] = f

        def value: D[Regular[A, B]] = v

        def asRegularDA(d: D[A]): Regular[A, D[A]] = convA(d)
        def asRegularDB(d: D[B]): Regular[B, D[B]] = convB(d)

      }
  }



  def fold[A, D[_], B]
    (h: Regular[A, B] => B)
    (r: Regular[A, D[A]])
    (implicit 
      daIsRegular: D[A] => Regular[A, D[A]]): B = {
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

      case a: AppFunc[A, D[A]] =>
        type F = Regular[A, D[A]] // Regular[Int, Rose[Int]]

        val functor: Functor[a.D] = a.functor // Functor[List]
        
        val value: a.D[F] = a.value // List[Regular[Int, Rose[Int]]] ~~ List[Rose[Int]]

        def f(r: F): B = fold(h)(r) // Regular[Int, Rose[Int]] => Int

        def func: a.D[F] => a.D[B] = functor.fmap(f) // List[Regular[Int, Rose[Int]]] => List[Int]

        val tmp: a.D[B] = func(value) // List[Int]

        def asRegular(b: B): Regular[A, B] = Rec(b) // 

        def func2: a.D[B] => a.D[Regular[A, B]] = functor.fmap(asRegular)

        val newValue: a.D[Regular[A, B]] = func2(tmp)

        def convB(d: a.D[B]): Regular[B, a.D[B]] = Rec(d)

        val regular: Regular[A, B] = 
          AppFunc[A, B, a.D](functor, newValue, a.asRegularDA, convB)

        h(regular)
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
      
      case a: AppFunc[Int, Int] =>
        
        type A = Int
        type B = Int

        type F = Regular[A, B] 

        val functor: Functor[a.D] = a.functor 
        
        val value: a.D[F] = a.value 

        def h(f: Regular[A, B]): B = fmax(f)

        def func: a.D[F] => a.D[B] = functor.fmap(h)

        val tmp: a.D[B] = func(value) 

        def res: B = fold(h)(a.asRegularDB(tmp))(a.asRegularDA) 

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

  val rose = Fork(1, List(Fork(2, Nil), Fork(40, List(Fork(5, Nil))), Fork(3, Nil)))

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
          AppFunc[A, Rose[A], List](
            ListFunc, 
            childs.map(x => RoseIsRegular(x)(evA, evB)), 
            evA,
            evB)
        
        val prod = x(par, func)
        prod
    }

  val rmax: Int = pmax(rose)  




  def fdepth[A](f: Regular[A, Int]): Int = {
    f match {
      case U() => 0
      case K(t) => Zero(t)
      case Par(a) => 1
      case Rec(r) => if (r > 0) r + 1 else 0
      case f x g  => math.max(fdepth(f), fdepth(g))
      case s : +[Int @unchecked, Int @unchecked, _, _]  => s match {
        case Left(f) => fdepth(f)
        case Right(g) => fdepth(g)
      }
      
      case a: AppFunc[A, Int] =>
        
        type B = Int

        type F = Regular[A, B] 

        val functor: Functor[a.D] = a.functor 
        
        val value: a.D[F] = a.value 

        def h(f: Regular[A, B]): B = fdepth(f)

        def func: a.D[F] => a.D[B] = functor.fmap(h)

        val tmp: a.D[B] = func(value) 

        def res: B = pmax(a.asRegularDB(tmp))(a.asRegularDB)

        res

    }
  }

  def depth[A, D[_]](r: Regular[A, D[A]])(implicit ev: D[A] => Regular[A, D[A]]): Int = 
    fold(fdepth[A])(r)(ev)
}