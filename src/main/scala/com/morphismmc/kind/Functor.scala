package com.morphismmc.kind

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  
  final def fmap[A, B](fa: F[A])(f: A => B): F[B] = map(fa)(f)
}