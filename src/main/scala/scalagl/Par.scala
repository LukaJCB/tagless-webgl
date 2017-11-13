package scalagl

import cats._
import cats.arrow.FunctionK
import cats.effect._
import cats.implicits._
import fs2._

import scala.concurrent.ExecutionContext



object ParIO {

  def applicativeForParIO(implicit ev: ExecutionContext): Applicative[IO] = new Applicative[IO] {
    def ap[A, B](ff: IO[A => B])(fa: IO[A]) = for {
      tf <- async.start(ff)
      ta <- async.start(fa)
      f <- tf
      a <- ta
    } yield f(a)

    def pure[A](a: A): IO[A] = IO.pure(a)
  }

  implicit def parallelForIO(implicit ev: ExecutionContext): Parallel[IO, IO] = new Parallel[IO, IO] {
    def monad: Monad[IO] = IO.ioEffect

    def applicative: Applicative[IO] = applicativeForParIO

    def sequential = FunctionK.id
    def parallel = FunctionK.id
  }
}


case class Par[F[_], A](value: F[A])

object Par {

  def applicativeForPar[F[_]: Effect](implicit ev: ExecutionContext): Applicative[Par[F, ?]] = new Applicative[Par[F, ?]] {
    def ap[A, B](ff: Par[F, A => B])(fa: Par[F, A]) = Par(for {
      tf <- async.start(ff.value)
      ta <- async.start(fa.value)
      f <- tf
      a <- ta
    } yield f(a))

    def pure[A](a: A): Par[F, A] = Par(Applicative[F].pure(a))
  }

  implicit def parallelForPar[F[_]: Effect](implicit ev: ExecutionContext): Parallel[F, Par[F, ?]] = new Parallel[F, Par[F, ?]] {
    def monad: Monad[F] = Monad[F]

    def applicative: Applicative[Par[F, ?]] = applicativeForPar[F]

    override def sequential = Lambda[Par[F, ?] ~> F](_.value)
    def parallel = Lambda[F ~> Par[F, ?]](g => Par(g))
  }
}
