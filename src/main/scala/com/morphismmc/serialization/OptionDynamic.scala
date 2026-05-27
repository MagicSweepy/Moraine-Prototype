package com.morphismmc.serialization

import com.morphismmc.serialization.decoder.Decoder
import java.nio.ByteBuffer

final class OptionDynamic[T](ops: DynamicOps[T], private val delegate: DataResult[Dynamic[T]]) extends DynamicLike[T](ops) {

  def get: DataResult[Dynamic[T]] = delegate

  def result: Option[Dynamic[T]] = delegate.result

  def map[U](f: Dynamic[T] => U): DataResult[U] = delegate.map(f)

  def flatMap[U](f: Dynamic[T] => DataResult[U]): DataResult[U] = delegate.flatMap(f)

  def orElseEmptyMap: Dynamic[T] = result.getOrElse(emptyMap)

  def orElseEmptyList: Dynamic[T] = result.getOrElse(emptyList)

  def into[V](action: Dynamic[T] => V): DataResult[V] = delegate.map(action)

  def asNumber: DataResult[Number] = flatMap(_.asNumber)

  def asString: DataResult[String] = flatMap(_.asString)

  def asStreamOpt: DataResult[Iterable[Dynamic[T]]] = flatMap(_.asStreamOpt)

  def asMapOpt: DataResult[Iterable[(Dynamic[T], Dynamic[T])]] = flatMap(_.asMapOpt)

  def asByteBufferOpt: DataResult[ByteBuffer] = flatMap(_.asByteBufferOpt)

  def asIntStreamOpt: DataResult[Iterable[Int]] = flatMap(_.asIntStreamOpt)

  def asLongStreamOpt: DataResult[Iterable[Long]] = flatMap(_.asLongStreamOpt)

  def get(key: String): OptionDynamic[T] =
    new OptionDynamic[T](ops, delegate.flatMap(k => k.get(key).delegate))

  def getGeneric(key: T): DataResult[T] = flatMap(_.getGeneric(key))

  def getElement(key: String): DataResult[T] = flatMap(_.getElement(key))

  def getElementGeneric(key: T): DataResult[T] = flatMap(_.getElementGeneric(key))

  def decode[A](decoder: Decoder[? <: A]): DataResult[(A, T)] = delegate.flatMap(t => t.decode(decoder))
}