package com.morphismmc.serialization

import com.morphismmc.serialization.decoder.Decoder
import java.nio.ByteBuffer

class Dynamic[T](ops: DynamicOps[T], val value: T) extends DynamicLike[T](ops):

  def this(ops: DynamicOps[T]) = this(ops, ops.empty)

  def map(f: T => T): Dynamic[T] = Dynamic[T](ops, f(value))

  def castTyped[U](targetOps: DynamicOps[U]): Dynamic[U] = {
    if (ops != targetOps) {
      throw IllegalStateException("Dynamic type doesn't match")
    }
    this.asInstanceOf[Dynamic[U]]
  }

  def cast[U](targetOps: DynamicOps[U]): U = castTyped(targetOps).value

  def merge(other: Dynamic[?]): OptionDynamic[T] = {
    val merged = ops.mergeToList(value, other.cast(ops))
    OptionDynamic[T](ops, merged.map(m => Dynamic[T](ops, m)))
  }

  def merge(key: Dynamic[?], other: Dynamic[?]): OptionDynamic[T] = {
    val merged = ops.mergeToMap(value, key.cast(ops), other.cast(ops))
    OptionDynamic[T](ops, merged.map(m => Dynamic[T](ops, m)))
  }

  def getMapValues: DataResult[Map[Dynamic[T], Dynamic[T]]]
    = ops.getMapValues(value).map { entries =>
      entries.map((k, v) => (Dynamic[T](ops, k), Dynamic[T](ops, v))).toMap
    }

  def updateMapValues(updater: ((Dynamic[?], Dynamic[?])) => (Dynamic[?], Dynamic[?])): Dynamic[T]
    = getMapValues.map { map =>
        val updated = map.map { (k, v) =>
          val (nk, nv) = updater((k, v))
          (nk.castTyped(ops), nv.castTyped(ops))
        }
        createMap(updated)
      }.result.getOrElse(this)

  def remove(key: String): Dynamic[T] = map(v => ops.remove(v, key))

  def set(key: String, value: Dynamic[?]): Dynamic[T] = map(v => ops.set(v, key, value.cast(ops)))

  def update(key: String, f: Dynamic[?] => Dynamic[?]): Dynamic[T]
    = map(v => ops.update(v, key, oldVal => {
      val in: Dynamic[?] = Dynamic(ops, oldVal)
      f(in).cast(ops)
    }))

  def updateGeneric(key: T, f: T => T): Dynamic[T] = map(v => ops.updateGeneric(v, key, f))

  def convert[U](outOps: DynamicOps[U]): Dynamic[U] = Dynamic[U](outOps, Dynamic.convert(ops, outOps, value))

  def into[V](action: Dynamic[T] => V): V = action(this)

  // region Impl

  def asNumber: DataResult[Number] = ops.getNumberValue(value)
  
  def asString: DataResult[String] = ops.getStringValue(value)
  
  def asStreamOpt: DataResult[Iterable[Dynamic[T]]] = ops.getStream(value).map(_.map(e => Dynamic[T](ops, e)))
  
  def asMapOpt: DataResult[Iterable[(Dynamic[T], Dynamic[T])]] 
    = ops.getMapValues(value).map(_.map((k, v) => (Dynamic[T](ops, k), Dynamic[T](ops, v))))
  
  def asByteBufferOpt: DataResult[ByteBuffer] = ops.getByteBuffer(value)
  
  def asIntStreamOpt: DataResult[Iterable[Int]] = ops.getIntStream(value)
  
  def asLongStreamOpt: DataResult[Iterable[Long]] = ops.getLongStream(value)

  def get(key: String): OptionDynamic[T]
    = OptionDynamic[T](ops,
      ops.getMap(value).flatMap { map =>
        map.get(key) match
          case Some(v) => DataResult.success(Dynamic[T](ops, v))
          case None    => DataResult.error(() => s"key missing: $key in $value")
      })

  def getGeneric(key: T): DataResult[T] = ops.getGeneric(value, key)
  
  def getElement(key: String): DataResult[T] = getElementGeneric(ops.createString(key))
  
  def getElementGeneric(key: T): DataResult[T] = ops.getGeneric(value, key)

  def decode[A](decoder: Decoder[? <: A]): DataResult[(A, T)] = decoder.decode(ops, value)

  // endregion

  override def equals(obj: Any): Boolean = obj match
    case that: Dynamic[?]  => ops == that.ops && value == that.value
    case _                 => false

  override def hashCode: Int = 31 * value.hashCode + ops.hashCode

  override def toString: String = s"$ops[$value]"

object Dynamic {
  
  def convert[S, T](inOps: DynamicOps[S], outOps: DynamicOps[T], input: S): T = {
    if (inOps == outOps) {
      input.asInstanceOf[T]
    } else {
      inOps.convertTo(outOps, input)
    }
  }
}