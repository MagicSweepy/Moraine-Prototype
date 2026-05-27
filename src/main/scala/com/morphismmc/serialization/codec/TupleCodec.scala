package com.morphismmc.serialization.codec

import com.morphismmc.serialization.{DataResult, DynamicOps, MapLike, RecordBuilder}

final class PairCodec[T1, T2](t1: Codec[T1], t2: Codec[T2]) extends Codec[(T1, T2)] {
  def decode[T](ops: DynamicOps[T], input: T): DataResult[((T1, T2), T)] 
    = t1.decode(ops, input).flatMap { (v1, rest) =>t2.decode(ops, rest).map { (v2, rest2) =>((v1, v2), rest2) } }

  def encode[T](input: (T1, T2), ops: DynamicOps[T], prefix: T): DataResult[T] 
    = t1.encode(input._1, ops, prefix).flatMap(p => t2.encode(input._2, ops, p))

  override def toString: String = s"PairCodec[$t1, $t2]"
}

final class PairMapCodec[T1, T2](t1: MapCodec[T1], t2: MapCodec[T2]) extends MapCodec[(T1, T2)] {
  def keys[T](ops: DynamicOps[T]): Iterable[T] = t1.keys(ops) ++ t2.keys(ops)

  def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[(T1, T2)] 
    = t1.decode(ops, input).flatMap(v1 => t2.decode(ops, input).map(v2 => (v1, v2)))

  def encode[T](input: (T1, T2), ops: DynamicOps[T], prefix: RecordBuilder[T]): RecordBuilder[T] 
    = t1.encode(input._1, ops, t2.encode(input._2, ops, prefix))

  override def toString: String = s"PairMapCodec[$t1, $t2]"
}

final class TripleCodec[T1, T2, T3](t1: Codec[T1], t2: Codec[T2], t3: Codec[T3]) extends Codec[(T1, T2, T3)] {
  def decode[T](ops: DynamicOps[T], input: T): DataResult[((T1, T2, T3), T)] 
    = t1.decode(ops, input).flatMap { (v1, rest1) =>t2.decode(ops, rest1)
      .flatMap { (v2, rest2) =>t3.decode(ops, rest2).map { (v3, rest3) =>((v1, v2, v3), rest3) } } }

  def encode[T](input: (T1, T2, T3), ops: DynamicOps[T], prefix: T): DataResult[T] 
    = t1.encode(input._1, ops, prefix).flatMap(p1 => t2.encode(input._2, ops, p1)
      .flatMap(p2 => t3.encode(input._3, ops, p2)))

  override def toString: String = s"TripleCodec[$t1, $t2, $t3]"
}

final class TripleMapCodec[T1, T2, T3](t1: MapCodec[T1], t2: MapCodec[T2], t3: MapCodec[T3]) extends MapCodec[(T1, T2, T3)] {
  def keys[T](ops: DynamicOps[T]): Iterable[T] = t1.keys(ops) ++ t2.keys(ops) ++ t3.keys(ops)

  def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[(T1, T2, T3)] 
    = t1.decode(ops, input).flatMap(v1 => t2.decode(ops, input).flatMap(v2 => t3.decode(ops, input).map(v3 => (v1, v2, v3))))

  def encode[T](input: (T1, T2, T3), ops: DynamicOps[T], prefix: RecordBuilder[T]): RecordBuilder[T]
    = t1.encode(input._1, ops, t2.encode(input._2, ops, t3.encode(input._3, ops, prefix)))

  override def toString: String = s"TripleMapCodec[$t1, $t2, $t3]"
}