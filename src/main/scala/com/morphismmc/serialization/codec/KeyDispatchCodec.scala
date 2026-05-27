package com.morphismmc.serialization.codec

import com.morphismmc.serialization.{DataResult, DynamicOps, MapLike, RecordBuilder}
import com.morphismmc.serialization.encoder.MapEncoder
import com.morphismmc.serialization.decoder.MapDecoder

final class KeyDispatchCodec[V, K](keyCodec: MapCodec[K], 
                                   typeFn: V => DataResult[? <: K], 
                                   codecFn: K => DataResult[? <: MapCodec[? <: V]]) extends MapCodec[V] {
  
  def keys[T](ops: DynamicOps[T]): Iterable[T] = keyCodec.keys(ops) ++ Iterable(ops.createString("value"))

  def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[V] = {
    keyCodec.decode(ops, input).flatMap { key =>
      codecFn(key).flatMap { elementCodec =>
        if (ops.compressMaps) {
          input.get("value") match
            case Some(v) => elementCodec.asInstanceOf[MapDecoder[V]].decoder.parse(ops, v)
            case None    => DataResult.error(() => s"Input does not have a \"value\" entry: $input")
        } else {
          elementCodec.asInstanceOf[MapDecoder[V]].decode(ops, input)
        }
      }
    }
  }

  def encode[T](input: V, ops: DynamicOps[T], prefix: RecordBuilder[T]): RecordBuilder[T] = {
    val typeResult = typeFn(input)
    val encoderResult = typeResult.flatMap(key => codecFn(key).map(c => c.asInstanceOf[MapEncoder[V]]))
    val builder = prefix.withErrorsFrom(encoderResult).withErrorsFrom(typeResult)
    if (encoderResult.isError || typeResult.isError) {
      builder
    } else {
      val elementEncoder = encoderResult.getOrThrow
      val keyValue = typeResult.getOrThrow
      if (ops.compressMaps) {
        keyCodec.encode(keyValue, ops, builder).add("value", elementEncoder.encoder.encodeStart(ops, input))
      } else {
        val encodedContents = elementEncoder.encode(input, ops, builder)
        keyCodec.encode(keyValue, ops, encodedContents)
      }
    }
  }

  override def toString: String = s"KeyDispatchCodec[$keyCodec $typeFn $codecFn]"
}