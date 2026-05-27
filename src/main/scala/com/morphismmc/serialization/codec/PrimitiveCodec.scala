package com.morphismmc.serialization.codec

import com.morphismmc.serialization.{DataResult, DynamicOps}

trait PrimitiveCodec[A] extends Codec[A] {
  def read[T](ops: DynamicOps[T], input: T): DataResult[A]

  def write[T](ops: DynamicOps[T], value: A): T

  def decode[T](ops: DynamicOps[T], input: T): DataResult[(A, T)] = read(ops, input).map(r => (r, ops.empty))

  def encode[T](input: A, ops: DynamicOps[T], prefix: T): DataResult[T] = ops.mergeToPrimitive(prefix, write(ops, input))
}
