package func

import scala.language.higherKinds


object Functors {
 
  trait Functor[F[_]] {
    def fmap[A, B](f: A => B): F[A] => F[B]
  } 

  case class C[A](i: Int, a: A)


  object CFunc extends Functor[C] {
    def fmap[A, B](f: A => B) = 
      {
        case C(n, a) => C(n, f(a))
      }
  }


  trait Functor2[F[_, _]] {
    def fmap2[A, C, B, D](f: A => C)(g: B => D): F[A, B] => F[C, D]
  } 

  object ProdFunc extends Functor2[Tuple2] {
    def fmap2[A, C, B, D](f: A => C)(g: B => D) = 
      {
        case (a, b) => (f(a), g(b))
      }
  }


  object SumFunc extends Functor2[Either] {
    def fmap2[A, C, B, D](f: A => C)(g: B => D) = 
      {
        case Left(x) => Left(f(x))
        case Right(y) => Right(g(y))
      }
  }


  case class Const[A, B](a: A)

  class ConstFunc[A] extends Functor[({ type C[B] = Const[A, B] })#C] {
    def fmap[B, C](f: B => C) = {
      case Const(a) => Const(a)
    }
  }
}
