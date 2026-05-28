package com.morphismmc.kind

type Id[A] = A

given idApplicative: Applicative[Id] with {
  def pure[A](a: A): Id[A] = a

  def ap[A, B](ff: Id[A => B])(fa: Id[A]): Id[B] = ff(fa)

  def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)
}
