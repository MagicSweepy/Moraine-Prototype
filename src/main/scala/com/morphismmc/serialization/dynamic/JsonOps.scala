package com.morphismmc.serialization.dynamic

import com.morphismmc.serialization.*
import ujson.*

import scala.collection.mutable

final class JsonOps(private val compressed: Boolean) extends DynamicOps[Value] {

  def empty: Value = Null

  override def emptyMap: Value = Obj()

  override def emptyList: Value = Arr()

  // region Convert Ops

  def convertTo[U](outOps: DynamicOps[U], input: Value): U = input match
    case _: Obj => convertMap(outOps, input)
    case _: Arr => convertList(outOps, input)
    case Null   => outOps.empty
    case Str(s) => outOps.createString(s)
    case True   => outOps.createBoolean(true)
    case False  => outOps.createBoolean(false)
    case Num(d) =>
      val l = d.toLong
      if (d == l.toDouble) {
        if      l.toByte.toDouble == d  then outOps.createByte(l.toByte)
        else if l.toShort.toDouble == d then outOps.createShort(l.toShort)
        else if l.toInt.toDouble == d   then outOps.createInt(l.toInt)
        else                                 outOps.createLong(l)
      } else {
        val f = d.toFloat
        if (f.toDouble == d) {
          outOps.createFloat(f)
        }
        else outOps.createDouble(d)
      }

  // endregion

  // region Primitive Type Ops

  def getNumberValue(input: Value): DataResult[Number] = input match
    case Num(d)               => DataResult.success(d)
    case True                 => DataResult.success(1)
    case False                => DataResult.success(0)
    case Str(s) if compressed =>
      try DataResult.success(s.toInt)
      catch case _: NumberFormatException => DataResult.error(() => s"Not a number: $input")
    case _                    => DataResult.error(() => s"Not a number: $input")

  def createNumeric(value: Number): Value = Num(value.doubleValue())

  override def getBooleanValue(input: Value): DataResult[Boolean] = input match
    case True   => DataResult.success(true)
    case False  => DataResult.success(false)
    case Num(d) => DataResult.success(d.toByte != 0)
    case _      => DataResult.error(() => s"Not a boolean: $input")

  override def createBoolean(value: Boolean): Value = if value then True else False

  def getStringValue(input: Value): DataResult[String] = input match
    case Str(s)               => DataResult.success(s)
    case Num(d) if compressed => DataResult.success(d.toString)
    case _                    => DataResult.error(() => s"Not a string: $input")

  def createString(value: String): Value = Str(value)

  // endregion

  // region List/Map Merger

  def mergeToList(list: Value, value: Value): DataResult[Value] = {
    if (!list.isInstanceOf[Arr] && list != empty) {
      DataResult.error(() => s"mergeToList called with not a list: $list", Some(list))
    } else {
      val result = list match
        case Arr(arr) => arr.to(mutable.ArrayBuffer)
        case _        => mutable.ArrayBuffer[Value]()
      result += value
      DataResult.success(Arr.from(result))
    }
  }

  override def mergeToList(list: Value, values: Iterable[Value]): DataResult[Value] = {
    if (!list.isInstanceOf[Arr] && list != empty) {
      DataResult.error(() => s"mergeToList called with not a list: $list", Some(list))
    } else {
      val result = list match
        case Arr(arr) => arr.to(mutable.ArrayBuffer)
        case _        => mutable.ArrayBuffer[Value]()
      result ++= values
      DataResult.success(Arr.from(result))
    }
  }

  def mergeToMap(map: Value, key: Value, value: Value): DataResult[Value] = {
    if (!map.isInstanceOf[Obj] && map != empty) {
      DataResult.error(() => s"mergeToMap called with not a map: $map", Some(map))
    } else if (!key.isInstanceOf[Str] && !compressed) {
      DataResult.error(() => s"key is not a string: $key", Some(map))
    } else {
      val result = map match
        case Obj(obj) => obj.to(mutable.LinkedHashMap)
        case _        => mutable.LinkedHashMap[String, Value]()
      result(key match { case Str(s) => s; case _ => key.toString }) = value
      DataResult.success(Obj.from(result))
    }
  }

  // endregion

  // region Map Ops

  def getMapValues(input: Value): DataResult[Iterable[(Value, Value)]] =
    input match
      case Obj(obj) => DataResult.success(obj.map((k, v) => (Str(k), v)))
      case _        => DataResult.error(() => s"Not a JSON object: $input")

  def createMap(entries: Iterable[(Value, Value)]): Value = {
    val obj = mutable.LinkedHashMap[String, Value]()
    entries.foreach { (k, v) =>
      val key = k match {
        case Str(s) => s;
        case _ => k.toString
      }
      obj(key) = v
    }
    Obj.from(obj)
  }

  // endregion

  // region List Ops

  def getStream(input: Value): DataResult[Iterable[Value]] =
    input match
      case Arr(arr) => DataResult.success(arr)
      case _        => DataResult.error(() => s"Not a json array: $input")

  def createList(input: Iterable[Value]): Value = Arr.from(input)

  // endregion

  // region Generic Ops

  def remove(input: Value, key: String): Value = input match
    case Obj(obj) => Obj.from(obj.filter((k, _) => k != key))
    case _        => input

  override def compressMaps: Boolean = compressed

  // endregion

  override def listBuilder: ListBuilder[Value] = new JsonArrayBuilder

  override def mapBuilder: RecordBuilder[Value] = new JsonRecordBuilder(this)

  override def toString: String = "JSON"

  private class JsonArrayBuilder extends ListBuilder[Value] {
    def ops: DynamicOps[Value] = JsonOps.this

    private var builder: DataResult[mutable.ArrayBuffer[Value]] =
      DataResult.success(mutable.ArrayBuffer(), LifeCycle.Stable)

    def add(value: Value): ListBuilder[Value] = {
      builder = builder.map(b => {
        b += value
        b
      })
      this
    }

    def add(value: DataResult[Value]): ListBuilder[Value] = {
      builder = builder.apply2stable((b, v: Value) => {
        b += v
        b
      }, value)
      this
    }

    def withErrorsFrom(result: DataResult[?]): ListBuilder[Value] = {
      builder = builder.flatMap(r => result.map(_ => r))
      this
    }

    def mapError(onError: String => String): ListBuilder[Value] = {
      builder = builder.mapError(onError)
      this
    }

    def build(prefix: Value): DataResult[Value] = {
      val result = builder.flatMap { b =>
        prefix match
          case Arr(arr) => DataResult.success(Arr.from(arr ++ b))
          case Null     => DataResult.success(Arr.from(b))
          case _        => DataResult.error(() => s"Cannot append a list to not a list: $prefix", Some(prefix))
      }
      builder = DataResult.success(mutable.ArrayBuffer(), LifeCycle.Stable)
      result
    }
  }

  private class JsonRecordBuilder(override val ops: DynamicOps[Value])
      extends RecordBuilder.AbstractStringBuilder[Value, mutable.LinkedHashMap[String, Value]](ops) {

    protected def initBuilder: mutable.LinkedHashMap[String, Value] = mutable.LinkedHashMap()

    protected def append(key: String, value: Value,
                         builder: mutable.LinkedHashMap[String, Value]): mutable.LinkedHashMap[String, Value] = {
      builder(key) = value
      builder
    }

    protected def build(builder: mutable.LinkedHashMap[String, Value], prefix: Value): DataResult[Value] = prefix match
      case Null     => DataResult.success(Obj.from(builder))
      case Obj(obj) =>
        val merged = obj.to(mutable.LinkedHashMap) ++ builder
        DataResult.success(Obj.from(merged))
      case _        => DataResult.error(() => s"mergeToMap called with not a map: $prefix", Some(prefix))
  }
}

object JsonOps {
  val INSTANCE: JsonOps = new JsonOps(false)
  val COMPRESSED: JsonOps = new JsonOps(true)
}