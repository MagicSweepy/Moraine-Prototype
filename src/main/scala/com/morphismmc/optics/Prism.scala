package com.morphismmc.optics

import com.morphismmc.kind.Applicative

final class Prism[S, T, A, B](val matching: S => Either[T, A], val build: B => T) extends Optic[S, T, A, B] {
  
  def modifyF[F[_]](f: A => F[B], s: S)(using app: Applicative[F]): F[T] = matching(s) match {
    case Left(t)  => app.pure(t)
    case Right(a) => app.map(f(a))(build)
  }
}

object Prism {
  def apply[S, T, A, B](matching: S => Either[T, A], build: B => T): Prism[S, T, A, B] = new Prism(matching, build)
  
  def left[F, G, F2]: Prism[Either[F, G], Either[F2, G], F, F2] 
    = Prism[Either[F, G], Either[F2, G], F, F2](matching = _.fold(f => Right(f), g => Left(Right(g))), build = f => Left(f))
  
  def right[F, G, G2]: Prism[Either[F, G], Either[F, G2], G, G2] 
    = Prism[Either[F, G], Either[F, G2], G, G2](matching = _.fold(f => Left(Left(f)), g => Right(g)), build = g => Right(g))
}
