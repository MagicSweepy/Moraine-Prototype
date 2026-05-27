package com.morphismmc.serialization.codec

import com.morphismmc.serialization.{DataResult, DynamicOps}

final class RecursiveCodec[A](name: String, wrapped: Codec[A] => Codec[A]) extends Codec[A] {

  private lazy val delegate: Codec[A] = wrapped(this)

  def decode[T](ops: DynamicOps[T], input: T): DataResult[(A, T)] = delegate.decode(ops, input)

  def encode[T](input: A, ops: DynamicOps[T], prefix: T): DataResult[T] = delegate.encode(input, ops, prefix)

  override def toString: String = s"RecursiveCodec[$name]"
}
