package com.morphismmc.optics

import com.morphismmc.kind.Applicative

final class Lens[S, T, A, B](val view: S => A, val update: (B, S) => T) extends Optic[S, T, A, B] {
  
  def modifyF[F[_]](f: A => F[B], s: S)(using app: Applicative[F]): F[T]
    = app.map(f(view(s)))(b => update(b, s))
}

object Lens {
  def apply[S, T, A, B](view: S => A, update: (B, S) => T): Lens[S, T, A, B] = new Lens(view, update)
  
  def _1[F, G, F2]: Lens[(F, G), (F2, G), F, F2] 
    = Lens[(F, G), (F2, G), F, F2](view = _._1, update = (b, s) => (b, s._2))
  
  def _2[F, G, G2]: Lens[(F, G), (F, G2), G, G2]
    = Lens[(F, G), (F, G2), G, G2](view = _._2, update = (b, s) => (s._1, b))
}
