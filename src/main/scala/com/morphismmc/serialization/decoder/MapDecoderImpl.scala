package com.morphismmc.serialization.decoder

import com.morphismmc.serialization.{DynamicOps, KeyCompressor}

abstract class MapDecoderImpl[A] extends MapDecoder[A] {
  
  def compressor[T](ops: DynamicOps[T]): KeyCompressor[T] = KeyCompressor[T](ops, keys(ops))
}
