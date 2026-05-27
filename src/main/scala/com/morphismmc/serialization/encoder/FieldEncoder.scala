package com.morphismmc.serialization.encoder

import com.morphismmc.serialization.{DynamicOps, RecordBuilder}

final class FieldEncoder[A](name: String, elementCodec: Encoder[A]) extends MapEncoderImpl[A] {
  
  def encode[T](input: A, ops: DynamicOps[T], prefix: RecordBuilder[T]): RecordBuilder[T] = {
    prefix.add(name, elementCodec.encodeStart(ops, input))
    prefix
  }

  def keys[T](ops: DynamicOps[T]): Iterable[T] = Iterable(ops.createString(name))

  override def toString: String = s"$name: $elementCodec"
}