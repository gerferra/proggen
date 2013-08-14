package func

import scala.language.{ higherKinds, implicitConversions }


object Functors {

  trait Functor[F[_]] {
    def fmap[A, B](f: A => B): F[A] => F[B]
  }

  trait Functor2[F[_, _]] {
    def fmap2[A, C, B, D](f: A => C)(g: B => D): F[A, B] => F[C, D]
  }
  
  object Func1 {

    case class C[A](i: Int, a: A)

    object CFunc extends Functor[C] {
      def fmap[A, B](f: A => B): C[A] => C[B] = 
        {
          case C(n, a) => C(n, f(a))
        }
    }


    object ProdFunc extends Functor2[Tuple2] {
      def fmap2[A, C, B, D](f: A => C)(g: B => D): Tuple2[A, B] => Tuple2[C, D] = 
        {
          case (a, b) => (f(a), g(b))
        }
    }

    object SumFunc extends Functor2[Either] {
      def fmap2[A, C, B, D](f: A => C)(g: B => D): Either[A, B] => Either[C, D] = 
        {
          case Left(x) => Left(f(x))
          case Right(y) => Right(g(y))
        }
    }


    case class Const[A, B](a: A)

    class ConstFunc[A] extends Functor[({ type C[B] = Const[A, B] })#C] {
      def fmap[B, C](f: B => C): Const[A, B] => Const[A, C] = {
        case Const(a) => Const(a)
      }
    }


  } 

  object Func2 {
    import gen.Regular._ 

    object UFunc extends Functor2[U] {
      def fmap2[A, C, B, D](f: A => C)(g: B => D): U[A, B] => U[C, D] = { 
        case U() => U()
      }
    }

    class KFunc[T] extends Functor2[({ type l[A, B] = K[A, B, T] })#l] {
      def fmap2[A, C, B, D](f: A => C)(g: B => D): K[A, B, T] => K[C, D, T] = { 
        case K(t) => K(t)
      }
    }

    object ParFunc extends Functor2[Par] {
      def fmap2[A, C, B, D](f: A => C)(g: B => D): Par[A, B] => Par[C, D] = { 
        case Par(a) => Par(f(a))
      }
    }

    object RecFunc extends Functor2[Rec] {
      def fmap2[A, C, B, D](f: A => C)(g: B => D): Rec[A, B] => Rec[C, D] = { 
        case Rec(b) => Rec(g(b))
      }
    }
  }

}
