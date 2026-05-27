package com.morphismmc.serialization.decoder

import com.morphismmc.serialization.{DataResult, DynamicOps, MapLike}

class RecordMapDecoder[A, B](fa: MapDecoder[A], ff: MapDecoder[A => B]) extends MapDecoderImpl[B] {
  def keys[T](ops: DynamicOps[T]): Iterable[T] = fa.keys(ops) ++ ff.keys(ops)

  def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[B] 
    = fa.decode(ops, input).flatMap(a => ff.decode(ops, input).map(f => f(a)))
}

class RecordMapDecoder2[A, B, C](ff: MapDecoder[(A, B) => C], fa: MapDecoder[A], fb: MapDecoder[B]) extends MapDecoderImpl[C] {
  def keys[T](ops: DynamicOps[T]): Iterable[T] = ff.keys(ops) ++ fa.keys(ops) ++ fb.keys(ops)

  def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[C]
    = fa.decode(ops, input).flatMap(a => fb.decode(ops, input).flatMap(b => ff.decode(ops, input).map(f => f(a, b))))
}
