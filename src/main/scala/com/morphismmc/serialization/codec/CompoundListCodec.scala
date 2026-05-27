package com.morphismmc.serialization.codec

import com.morphismmc.serialization.{DataResult, DynamicOps}

final class CompoundListCodec[K, V](keyCodec: Codec[K], elementCodec: Codec[V]) extends Codec[List[(K, V)]] {
  def decode[T](ops: DynamicOps[T], input: T): DataResult[(List[(K, V)], T)]
    = ops.getStream(input).flatMap { stream =>
      val results = stream.map { elem =>
        keyCodec.decode(ops, elem).flatMap { (k, rest) =>
          elementCodec.decode(ops, rest).map { (v, _) => (k, v) }
        }
      }.toList
      sequence(results).map(rs => (rs, ops.empty))
    }

  def encode[T](input: List[(K, V)], ops: DynamicOps[T], prefix: T): DataResult[T] = {
    val builder = ops.listBuilder
    input.foreach { (k, v) =>
      builder.add(keyCodec.encodeStart(ops, k))
      builder.add(elementCodec.encodeStart(ops, v))
    }
    builder.build(prefix)
  }

  private def sequence[R](list: List[DataResult[R]]): DataResult[List[R]] 
    = list.foldRight(DataResult.success(List.empty[R]))((fa, acc) => fa.flatMap(a => acc.map(as => a :: as)))

  override def toString: String = s"CompoundListCodec[$keyCodec -> $elementCodec]"
}