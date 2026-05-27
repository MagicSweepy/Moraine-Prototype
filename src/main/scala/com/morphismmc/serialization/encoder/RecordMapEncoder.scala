package com.morphismmc.serialization.encoder

import com.morphismmc.serialization.{DynamicOps, RecordBuilder}

class RecordMapEncoder[A, B](fa: MapEncoder[A], ff: MapEncoder[A => B], a: A) extends MapEncoderImpl[B] {
  def keys[T](ops: DynamicOps[T]): Iterable[T] = fa.keys(ops) ++ ff.keys(ops)

  def encode[T](input: B, ops: DynamicOps[T], prefix: RecordBuilder[T]): RecordBuilder[T] 
    = ff.encode(_ => input, ops, fa.encode(a, ops, prefix))
}

class RecordMapEncoder2[A, B, C](ff: MapEncoder[(A, B) => C], fa: MapEncoder[A], fb: MapEncoder[B],
                                 a: A, b: B) extends MapEncoderImpl[C] {
  def keys[T](ops: DynamicOps[T]): Iterable[T] = ff.keys(ops) ++ fa.keys(ops) ++ fb.keys(ops)

  def encode[T](input: C, ops: DynamicOps[T], prefix: RecordBuilder[T]): RecordBuilder[T]
    = ff.encode((_, _) => input, ops, fa.encode(a, ops, fb.encode(b, ops, prefix)))
}