package com.morphismmc.serialization

import java.nio.ByteBuffer

trait DynamicOps[T] {
  
  def empty: T

  def emptyMap: T = createMap(Map.empty)

  def emptyList: T = createList(Iterable.empty)

  // region Convert Ops

  def convertTo[U](outOps: DynamicOps[U], input: T): U

  def convertList[U](outOps: DynamicOps[U], input: T): U = {
    val stream = getStream(input).result.getOrElse(Iterable.empty)
    outOps.createList(stream.map(e => convertTo(outOps, e)))
  }
  
  def convertMap[U](outOps: DynamicOps[U], input: T): U = {
    val entries = getMapValues(input).result.getOrElse(Iterable.empty)
    outOps.createMap(entries.map((k, v) => (convertTo(outOps, k), convertTo(outOps, v))))
  }
  
  // endregion

  // region Primitive Type Ops

  def getNumberValue(input: T): DataResult[Number]

  def getNumberValue(input: T, default: Number): Number = getNumberValue(input).result.getOrElse(default)

  def createNumeric(value: Number): T

  def createByte(value: Byte): T = createNumeric(value)

  def createShort(value: Short): T = createNumeric(value)

  def createInt(value: Int): T = createNumeric(value)

  def createLong(value: Long): T = createNumeric(value)

  def createFloat(value: Float): T = createNumeric(value)

  def createDouble(value: Double): T = createNumeric(value)

  def getBooleanValue(input: T): DataResult[Boolean] = getNumberValue(input).map(n => n.byteValue != 0)

  def createBoolean(value: Boolean): T = createByte(if value then 1.toByte else 0.toByte)

  def getStringValue(input: T): DataResult[String]

  def createString(value: String): T

  // endregion

  // region List/Map/Primitive Type Merger

  def mergeToList(list: T, value: T): DataResult[T]

  def mergeToList(list: T, values: Iterable[T]): DataResult[T]
    = values.foldLeft(DataResult.success(list))((acc, v) => acc.flatMap(l => mergeToList(l, v)))

  def mergeToMap(map: T, key: T, value: T): DataResult[T]

  def mergeToMap(map: T, values: Map[T, T]): DataResult[T] 
    = mergeToMap(map, MapLike.forMap(values, createString))

  def mergeToMap(map: T, values: MapLike[T]): DataResult[T] 
    = values.entries.foldLeft(DataResult.success[T](map))((acc, entry) => acc.flatMap(m => mergeToMap(m, entry._1, entry._2)))

  def mergeToPrimitive(prefix: T, value: T): DataResult[T] = {
    if (prefix != empty) {
      DataResult.error(() => s"Do not know how to append a primitive value $value to $prefix")
    } else {
      DataResult.success(value)
    }
  }

  // endregion

  // region Map Ops

  def getMapValues(input: T): DataResult[Iterable[(T, T)]]

  def createMap(entries: Iterable[(T, T)]): T

  def createMap(map: Map[T, T]): T = createMap(map.to(Iterable))

  def getMap(input: T): DataResult[MapLike[T]]
    = getMapValues(input).flatMap { entries =>
      try {
        val map = entries.toMap
        DataResult.success(MapLike.forMap(map, createString))
      }
      catch 
        case e: IllegalStateException => DataResult.error(() => s"Error while building map: ${e.getMessage}")
    }

  // endregion

  // region List Ops

  def getStream(input: T): DataResult[Iterable[T]]

  def getList(input: T): DataResult[(T => Unit) => Unit] = getStream(input).map(s => f => s.foreach(f))

  def createList(input: Iterable[T]): T

  def getByteBuffer(input: T): DataResult[ByteBuffer] 
    = getStream(input).flatMap { stream =>
      val list = stream.toList
      if (list.forall(e => getNumberValue(e).result.isDefined)) {
        val buffer = ByteBuffer.wrap(new Array[Byte](list.size))
        for (i <- list.indices) {
          buffer.put(i, getNumberValue(list(i)).result.get.byteValue)
        }
        DataResult.success(buffer)
      } else {
        DataResult.error(() => s"Some elements are not bytes: $input")
      }
    }

  def createByteList(input: ByteBuffer): T = createList((0 until input.capacity).map(i => createByte(input.get(i))))

  def getIntStream(input: T): DataResult[Iterable[Int]]
    = getStream(input).flatMap { stream =>
      val list = stream.toList
      if (list.forall(e => getNumberValue(e).result.isDefined)) {
        DataResult.success(list.map(e => getNumberValue(e).result.get.intValue))
      } else {
        DataResult.error(() => s"Some elements are not ints: $input")
      }
    }

  def createIntList(input: Iterable[Int]): T = createList(input.map(i => createInt(i)))

  def getLongStream(input: T): DataResult[Iterable[Long]] 
    = getStream(input).flatMap { stream =>
      val list = stream.toList
      if (list.forall(e => getNumberValue(e).result.isDefined)) {
        DataResult.success(list.map(e => getNumberValue(e).result.get.longValue))
      } else {
        DataResult.error(() => s"Some elements are not longs: $input")
      }
    }

  def createLongList(input: Iterable[Long]): T = createList(input.map(i => createLong(i)))

  // endregion

  // region Generic Ops

  def remove(input: T, key: String): T

  def compressMaps: Boolean = false

  def get(input: T, key: String): DataResult[T] = getGeneric(input, createString(key))

  def getGeneric(input: T, key: T): DataResult[T] 
    = getMap(input).flatMap { map =>
      map.get(key) match
        case Some(v) => DataResult.success(v)
        case None => DataResult.error(() => s"No element $key in the map $input")
    }

  def set(input: T, key: String, value: T): T = mergeToMap(input, createString(key), value).result.getOrElse(input)

  def update(input: T, key: String, function: T => T): T 
    = get(input, key).map(v => set(input, key, function(v))).result.getOrElse(input)

  def updateGeneric(input: T, key: T, function: T => T): T
    = getGeneric(input, key).flatMap(v => mergeToMap(input, key, function(v))).result.getOrElse(input)

  // endregion

  // region Codec Ops

  def listBuilder: ListBuilder[T]

  def mapBuilder: RecordBuilder[T]

  // endregion
}
