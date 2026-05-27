package com.morphismmc.kind

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  
  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]
  
  @inline
  final def app[A, B](ff: F[A => B])(fa: F[A]): F[B] = ap(ff)(fa)
  
  def ap2[A, B, C](ff: F[(A, B) => C])(fa: F[A], fb: F[B]): F[C] // TODO: higher situation
    = map(product(fa, product(fb, ff))) { case (a, (b, f)) => f(a, b) }
  
  private def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ap(map(fa)(a => (b : B) => (a, b)))(fb)
}