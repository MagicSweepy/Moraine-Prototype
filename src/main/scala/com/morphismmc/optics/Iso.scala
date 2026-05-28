package com.morphismmc.optics

import com.morphismmc.kind.Applicative

final class Iso[S, T, A, B](val forward: S => A, val backward: B => T) extends Optic[S, T, A, B] {
  
  def modifyF[F[_]](f: A => F[B], s: S)(using ap: Applicative[F]): F[T] = ap.map(f(forward(s)))(backward)
  
  def reverse: Iso[B, A, T, S] = Iso(backward, forward)
}

object Iso {
  def apply[S, T, A, B](forward: S => A, backward: B => T): Iso[S, T, A, B] = new Iso(forward, backward)
  
  def id[A]: Iso[A, A, A, A] = Iso(identity, identity)
}
