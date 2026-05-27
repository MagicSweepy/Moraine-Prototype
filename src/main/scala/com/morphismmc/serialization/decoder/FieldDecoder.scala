package com.morphismmc.serialization.decoder

import com.morphismmc.serialization.{DataResult, DynamicOps, MapLike}

final class FieldDecoder[A](name: String, elementCodec: Decoder[A]) extends MapDecoderImpl[A] {
  
  def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[A] = input.get(name) match
    case Some(v) => elementCodec.parse(ops, v)
    case None    => DataResult.error(() => s"key missing: $name in $input")

  def keys[T](ops: DynamicOps[T]): Iterable[T] = Iterable(ops.createString(name))

  override def toString: String = s"$name: $elementCodec"
}
