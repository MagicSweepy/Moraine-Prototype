package com.morphismmc.optics

import com.morphismmc.kind.Applicative

trait Optic[S, T, A, B] {

  def modifyF[F[_]](f: A => F[B], s: S)(using Applicative[F]): F[T]
  
  def andThen[C, D](other: Optic[A, B, C, D]): Optic[S, T, C, D] = Optic.Composed(this, other)
}

object Optic {
  private final class Composed[S, T, A, B, C, D](outer: Optic[S, T, A, B], 
                                                 inner: Optic[A, B, C, D]) extends Optic[S, T, C, D] {
    def modifyF[F[_]](f: C => F[D], s: S)(using Applicative[F]): F[T] =
      outer.modifyF(a => inner.modifyF(f, a), s)
  }
}

