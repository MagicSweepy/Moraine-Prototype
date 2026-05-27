package com.morphismmc.serialization.encoder

import com.morphismmc.serialization.{DynamicOps, KeyCompressor}

abstract class MapEncoderImpl[A] extends MapEncoder[A] {
  
  def compressor[T](ops: DynamicOps[T]): KeyCompressor[T] = KeyCompressor[T](ops, keys(ops))
}
