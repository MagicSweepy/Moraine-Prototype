package com.morphismmc.serialization.codec

import com.morphismmc.serialization.{DataResult, DynamicOps, Keyable, MapLike, RecordBuilder}
import scala.collection.mutable

final class SimpleMapCodec[K, V](keyCodec: Codec[K], elementCodec: Codec[V], keys: Keyable) extends MapCodec[Map[K, V]] {

  def keys[T](ops: DynamicOps[T]): Iterable[T] = keys.keys(ops)

  def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[Map[K, V]] 
    = keys.keys(ops).foldLeft(DataResult.success(Map.empty[K, V])) { (acc, key) =>
      acc.flatMap { map =>
        input.get(key) match
          case Some(v) =>
            val keyResult = keyCodec.parse(ops, key)
            keyResult.flatMap(k => elementCodec.parse(ops, v).map(e => map + (k -> e)))
          case None    => DataResult.error(() => s"Missing key: $key in $input")
      }
    }

  def encode[T](input: Map[K, V], ops: DynamicOps[T], prefix: RecordBuilder[T]): RecordBuilder[T] = {
    input.foreach { (k, v) =>
      prefix.add(keyCodec.encodeStart(ops, k), elementCodec.encodeStart(ops, v))
    }
    prefix
  }

  override def toString: String = s"SimpleMapCodec[$keyCodec -> $elementCodec]"
}

final class UnboundedMapCodec[K, V](keyCodec: Codec[K], elementCodec: Codec[V]) extends Codec[Map[K, V]] {

  def decode[T](ops: DynamicOps[T], input: T): DataResult[(Map[K, V], T)] 
    = ops.getMapValues(input).flatMap { entries =>
      val results = entries.map { (k, v) =>
        keyCodec.parse(ops, k).flatMap(kp => elementCodec.parse(ops, v).map(vp => (kp, vp)))
      }.toList
      sequence(results).map(rs => (rs.toMap, ops.empty))
    }

  def encode[T](input: Map[K, V], ops: DynamicOps[T], prefix: T): DataResult[T] = {
    val encodedEntries = input.toList.map { (k, v) =>
      keyCodec.encodeStart(ops, k).flatMap(ek =>
        elementCodec.encodeStart(ops, v).map(ev => (ek, ev)))
    }
    sequence(encodedEntries).flatMap { es =>
      ops.mergeToMap(prefix, MapLike.forMap(es.toMap, ops.createString))
    }
  }

  private def sequence[R](list: List[DataResult[R]]): DataResult[List[R]] 
    = list.foldRight(DataResult.success(List.empty[R]))((fa, acc) => fa.flatMap(a => acc.map(as => a :: as)))

  override def toString: String = s"UnboundedMapCodec[$keyCodec -> $elementCodec]"
}

final class StrictUnboundedMapCodec[K, V](keyCodec: Codec[K], elementCodec: Codec[V]) extends Codec[Map[K, V]] {

  def decode[T](ops: DynamicOps[T], input: T): DataResult[(Map[K, V], T)] =
    ops.getMapValues(input).flatMap { entries =>
      val results = entries.map { (k, v) =>
        keyCodec.parse(ops, k).flatMap(kp => elementCodec.parse(ops, v).map(vp => (kp, vp)))
      }.toList
      sequenceStrict(results).map(rs => (rs.toMap, ops.empty))
    }

  def encode[T](input: Map[K, V], ops: DynamicOps[T], prefix: T): DataResult[T] =
    val encodedEntries = input.toList.map { (k, v) =>
      keyCodec.encodeStart(ops, k).flatMap(ek => elementCodec.encodeStart(ops, v).map(ev => (ek, ev)))
    }
    sequenceStrict(encodedEntries).flatMap { es =>
      ops.mergeToMap(prefix, MapLike.forMap(es.toMap, ops.createString))
    }

  private def sequenceStrict[R](list: List[DataResult[R]]): DataResult[List[R]] =
    val successes = mutable.ListBuffer[R]()
    val errs = mutable.ListBuffer[String]()
    list.foreach {
      case DataResult.Success(v, _)     => successes += v
      case DataResult.Error(msg, pv, _) =>
        errs += msg()
        pv.foreach(successes += _)
    }
    if (errs.nonEmpty) {
      DataResult.error(() => errs.mkString("; "), Some(successes.toList))
    } else {
      DataResult.success(successes.toList)
    }

  override def toString: String = s"StrictUnboundedMapCodec[$keyCodec -> $elementCodec]"
}
