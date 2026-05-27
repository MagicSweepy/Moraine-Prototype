package com.morphismmc.serialization

import scala.collection.mutable

abstract class CompressorHolder extends Compressible {
  
  private val compressors = mutable.HashMap[Any, KeyCompressor[?]]()

  def compressor[T](ops: DynamicOps[T]): KeyCompressor[T]
    = compressors.getOrElseUpdate(ops, new KeyCompressor[T](ops, keys(ops))).asInstanceOf[KeyCompressor[T]]
}