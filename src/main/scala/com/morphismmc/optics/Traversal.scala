package com.morphismmc.optics

import com.morphismmc.kind.Applicative

trait Traversal[S, T, A, B] extends Optic[S, T, A, B]

object Traversal {
  
  def list[A, B]: Traversal[List[A], List[B], A, B] = ListTraversal[A, B]()

  private final class ListTraversal[A, B] extends Traversal[List[A], List[B], A, B] {
    def modifyF[F[_]](f: A => F[B], s: List[A])(using ap: Applicative[F]): F[List[B]]
      = s.foldRight(ap.pure(List.empty[B])) { (a, fbs) => ap.ap2(ap.pure((b: B, bs: List[B]) => b :: bs))(f(a), fbs) }
  }
}
