package com.morphismmc.serialization

import com.morphismmc.serialization.decoder.Decoder
import java.nio.ByteBuffer

abstract class DynamicLike[T](val ops: DynamicOps[T]) {

  def asNumber: DataResult[Number]

  def asString: DataResult[String]

  def asStreamOpt: DataResult[Iterable[Dynamic[T]]]

  def asMapOpt: DataResult[Iterable[(Dynamic[T], Dynamic[T])]]

  def asByteBufferOpt: DataResult[ByteBuffer]

  def asIntStreamOpt: DataResult[Iterable[Int]]

  def asLongStreamOpt: DataResult[Iterable[Long]]

  def get(key: String): OptionDynamic[T]

  def getGeneric(key: T): DataResult[T]

  def getElement(key: String): DataResult[T]

  def getElementGeneric(key: T): DataResult[T]

  def decode[A](decoder: Decoder[? <: A]): DataResult[(A, T)]

  def asNumber(default: Number): Number = asNumber.result.getOrElse(default)

  def asInt(default: Int): Int = asNumber(default).intValue

  def asLong(default: Long): Long = asNumber(default).longValue

  def asFloat(default: Float): Float = asNumber(default).floatValue

  def asDouble(default: Double): Double = asNumber(default).doubleValue

  def asByte(default: Byte): Byte = asNumber(default).byteValue

  def asShort(default: Short): Short = asNumber(default).shortValue

  def asBoolean(default: Boolean): Boolean = asNumber(if default then 1 else 0).intValue != 0

  def asString(default: String): String = asString.result.getOrElse(default)

  def asStream: Iterable[Dynamic[T]] = asStreamOpt.result.getOrElse(Iterable.empty)

  def asByteBuffer: ByteBuffer = asByteBufferOpt.result.getOrElse(ByteBuffer.wrap(new Array[Byte](0)))

  def asIntStream: Iterable[Int] = asIntStreamOpt.result.getOrElse(Iterable.empty)

  def asLongStream: Iterable[Long] = asLongStreamOpt.result.getOrElse(Iterable.empty)

  def getElement(key: String, default: T): T = getElement(key).result.getOrElse(default)

  def getElementGeneric(key: T, default: T): T = getElementGeneric(key).result.getOrElse(default)

  def read[A](decoder: Decoder[? <: A]): DataResult[A] = decode(decoder).map(_._1)

  def asListOpt[U](deserializer: Dynamic[T] => U): DataResult[List[U]] = asStreamOpt.map(_.map(deserializer).toList)

  def asMapOpt[K, V](keyDeserializer: Dynamic[T] => K, valueDeserializer: Dynamic[T] => V): DataResult[Map[K, V]]
    = asMapOpt.map(_.map((k, v) => (keyDeserializer(k), valueDeserializer(v))).toMap)

  def readList[E](decoder: Decoder[E]): DataResult[List[E]]
    = asStreamOpt.flatMap { stream =>
      sequence(stream.map(_.read(decoder)).toList)
    }

  def readList[E](decoder: Dynamic[?] => DataResult[? <: E]): DataResult[List[E]]
    = asStreamOpt.flatMap { stream =>
      // noinspection ScalaRedundantCast
      sequence(stream.map(d => decoder(d).map(_.asInstanceOf[E])).toList)
    }

  def readMap[K, V](keyDecoder: Decoder[K], valueDecoder: Decoder[V]): DataResult[List[(K, V)]]
    = asMapOpt.flatMap { stream =>
      sequence(stream.map { (k, v) =>
        k.read(keyDecoder).flatMap(kf => v.read(valueDecoder).map(vs => (kf, vs)))
      }.toList)
    }

  def readMap[K, V](keyDecoder: Decoder[K], valueDecoder: K => Decoder[V]): DataResult[List[(K, V)]]
    = asMapOpt.flatMap { stream =>
      sequence(stream.map { (k, v) =>
        k.read(keyDecoder).flatMap(kf => v.read(valueDecoder(kf)).map(vs => (kf, vs)))
      }.toList)
    }

  def readMap[R](empty: DataResult[R], combiner: (R, Dynamic[T], Dynamic[T]) => DataResult[R]): DataResult[R]
    = asMapOpt.flatMap { stream =>
      stream.foldLeft(empty)((acc, entry) => acc.flatMap(r => combiner(r, entry._1, entry._2)))
    }

  def emptyList: Dynamic[T] = Dynamic[T](ops, ops.emptyList)

  def emptyMap: Dynamic[T] = Dynamic[T](ops, ops.emptyMap)

  def createNumeric(i: Number): Dynamic[T] = Dynamic[T](ops, ops.createNumeric(i))

  def createByte(value: Byte): Dynamic[T] = Dynamic[T](ops, ops.createByte(value))

  def createShort(value: Short): Dynamic[T] = Dynamic[T](ops, ops.createShort(value))

  def createInt(value: Int): Dynamic[T] = Dynamic[T](ops, ops.createInt(value))

  def createLong(value: Long): Dynamic[T] = Dynamic[T](ops, ops.createLong(value))

  def createFloat(value: Float): Dynamic[T] = Dynamic[T](ops, ops.createFloat(value))

  def createDouble(value: Double): Dynamic[T] = Dynamic[T](ops, ops.createDouble(value))

  def createBoolean(value: Boolean): Dynamic[T] = Dynamic[T](ops, ops.createBoolean(value))

  def createString(value: String): Dynamic[T] = Dynamic[T](ops, ops.createString(value))

  def createList(input: Iterable[? <: Dynamic[?]]): Dynamic[T]
    = Dynamic[T](ops, ops.createList(input.map(_.cast(ops))))

  def createMap(map: Map[? <: Dynamic[?], ? <: Dynamic[?]]): Dynamic[T] = {
    val converted = map.map[T, T] { case (k, v) => (k.cast(ops), v.cast(ops)) }
    Dynamic[T](ops, ops.createMap(converted))
  }

  def createByteList(input: ByteBuffer): Dynamic[?] = Dynamic(ops, ops.createByteList(input))

  def createIntList(input: Iterable[Int]): Dynamic[?] = Dynamic(ops, ops.createIntList(input))

  def createLongList(input: Iterable[Long]): Dynamic[?] = Dynamic(ops, ops.createLongList(input))

  private def sequence[A](list: List[DataResult[A]]): DataResult[List[A]]
    = list.foldRight(DataResult.success(List.empty[A]))((fa, acc) => fa.flatMap(a => acc.map(as => a :: as)))

  def asList[U](deserializer: Dynamic[T] => U): List[U]
    = asListOpt(deserializer).result.getOrElse(Nil)

  def asMap[K, V](keyDeserializer: Dynamic[T] => K, valueDeserializer: Dynamic[T] => V): Map[K, V]
    = asMapOpt(keyDeserializer, valueDeserializer).result.getOrElse(Map.empty)
}