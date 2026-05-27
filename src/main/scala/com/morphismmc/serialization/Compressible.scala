package com.morphismmc.serialization

trait Compressible extends Keyable {
  
  def compressor[T](ops: DynamicOps[T]): KeyCompressor[T]
}
