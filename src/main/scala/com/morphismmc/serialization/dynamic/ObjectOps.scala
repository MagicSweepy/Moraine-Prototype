package com.morphismmc.serialization.dynamic

import com.morphismmc.serialization.*
import scala.collection.mutable

final class ObjectOps(private val compressed: Boolean) extends DynamicOps[Any] {

  def empty: Any = null

  override def emptyMap: Any = Map.empty[Any, Any]

  override def emptyList: Any = List.empty[Any]

  // region Convert Ops

  def convertTo[U](outOps: DynamicOps[U], input: Any): U = input match
    case _: Map[?, ?] => convertMap(outOps, input)
    case _: List[?]   => convertList(outOps, input)
    case null         => outOps.empty
    case s: String    => outOps.createString(s)
    case b: Boolean   => outOps.createBoolean(b)
    case n: Number    =>
      val d = n.doubleValue()
      val l = d.toLong
      if (d == l.toDouble) {
        if      l.toByte.toDouble == d  then outOps.createByte(l.toByte)
        else if l.toShort.toDouble == d then outOps.createShort(l.toShort)
        else if l.toInt.toDouble == d   then outOps.createInt(l.toInt)
        else                                 outOps.createLong(l)
      } else {
        outOps.createDouble(d)
      }

  // endregion

  // region Primitive Type Ops

  def getNumberValue(input: Any): DataResult[Number] = input match
    case n: Number               => DataResult.success(n)
    case b: Boolean              => DataResult.success(if b then 1 else 0)
    case s: String if compressed =>
      try DataResult.success(s.toDouble)
      catch case _: NumberFormatException => DataResult.error(() => s"Not a number: $s")
    case _                       => DataResult.error(() => s"Not a number: $input")

  def createNumeric(value: Number): Any = value

  override def getBooleanValue(input: Any): DataResult[Boolean] = input match
    case b: Boolean => DataResult.success(b)
    case n: Number  => DataResult.success(n.byteValue() != 0)
    case _          => DataResult.error(() => s"Not a boolean: $input")

  override def createBoolean(value: Boolean): Any = value

  def getStringValue(input: Any): DataResult[String] = input match
    case s: String                => DataResult.success(s)
    case n: Number if compressed  => DataResult.success(n.toString)
    case _                        => DataResult.error(() => s"Not a string: $input")

  def createString(value: String): Any = value

  // endregion

  // region List/Map Merger

  def mergeToList(list: Any, value: Any): DataResult[Any] = {
    if (list != null && !list.isInstanceOf[List[?]]) {
      DataResult.error(() => s"mergeToList called with not a list: $list", Some(list))
    } else {
      val buf = (list match {
        case l: List[?] => l;
        case _          => Nil
      }).to(mutable.ListBuffer)
      buf += value
      DataResult.success(buf.toList)
    }
  }

  override def mergeToList(list: Any, values: Iterable[Any]): DataResult[Any] = {
    if (list != null && !list.isInstanceOf[List[?]]) {
      DataResult.error(() => s"mergeToList called with not a list: $list", Some(list))
    } else {
      val buf = (list match {
        case l: List[?] => l;
        case _          => Nil
      }).to(mutable.ListBuffer)
      buf ++= values
      DataResult.success(buf.toList)
    }
  }

  def mergeToMap(map: Any, key: Any, value: Any): DataResult[Any] = {
    if (map != null && !map.isInstanceOf[Map[?, ?]]) {
      DataResult.error(() => s"mergeToMap called with not a map: $map", Some(map))
    } else if (!key.isInstanceOf[String] && !compressed) {
      DataResult.error(() => s"key is not a string: $key", Some(map))
    } else {
      val m = (map match {
        case m: Map[?, ?] => m.asInstanceOf[Map[Any, Any]];
        case _            => Map.empty[Any, Any]
      }).to(mutable.HashMap)
      m(key) = value
      DataResult.success(m.toMap)
    }
  }

  // endregion

  // region Map Ops

  def getMapValues(input: Any): DataResult[Iterable[(Any, Any)]] = input match
    case m: Map[?, ?] => DataResult.success(m.asInstanceOf[Map[Any, Any]])
    case _            => DataResult.error(() => s"Not a map: $input")

  def createMap(entries: Iterable[(Any, Any)]): Any = entries.toMap

  // endregion

  // region List Ops

  def getStream(input: Any): DataResult[Iterable[Any]] = input match
    case l: List[?] => DataResult.success(l)
    case _          => DataResult.error(() => s"Not a list: $input")

  def createList(input: Iterable[Any]): Any = input.toList

  // endregion

  // region Generic Ops

  def remove(input: Any, key: String): Any = input match
    case m: Map[?, ?] => m.asInstanceOf[Map[Any, Any]] - key
    case _            => input

  override def compressMaps: Boolean = compressed

  // endregion

  override def listBuilder: ListBuilder[Any] = new ObjectListBuilder

  override def mapBuilder: RecordBuilder[Any] = new ObjectRecordBuilder(this)

  override def toString: String = "Object"

  private class ObjectListBuilder extends ListBuilder[Any] {
    def ops: DynamicOps[Any] = ObjectOps.this

    private var builder: DataResult[mutable.ListBuffer[Any]] = DataResult.success(mutable.ListBuffer(), LifeCycle.Stable)

    def add(value: Any): ListBuilder[Any] = {
      builder = builder.map(b => {
        b += value
        b
      })
      this
    }

    def add(value: DataResult[Any]): ListBuilder[Any] = {
      builder = builder.apply2stable((b, v) => {
        b += v
        b
      }, value)
      this
    }

    def withErrorsFrom(result: DataResult[?]): ListBuilder[Any] = {
      builder = builder.flatMap(r => result.map(_ => r))
      this
    }

    def mapError(onError: String => String): ListBuilder[Any] = {
      builder = builder.mapError(onError)
      this
    }

    def build(prefix: Any): DataResult[Any] = {
      val result = builder.flatMap { b =>
        prefix match
          case l: List[?] => DataResult.success(l ++ b)
          case null       => DataResult.success(b.toList)
          case _          => DataResult.error(() => s"Cannot append a list to not a list: $prefix", Some(prefix))
      }
      builder = DataResult.success(mutable.ListBuffer(), LifeCycle.Stable)
      result
    }
  }

  private class ObjectRecordBuilder(override val ops: DynamicOps[Any])
    extends RecordBuilder.AbstractStringBuilder[Any, mutable.HashMap[Any, Any]](ops) {
    
    protected def initBuilder: mutable.HashMap[Any, Any] = mutable.HashMap()

    protected def append(key: String, value: Any, builder: mutable.HashMap[Any, Any]): mutable.HashMap[Any, Any] = {
      builder(key) = value
      builder
    }

    protected def build(builder: mutable.HashMap[Any, Any], prefix: Any): DataResult[Any] = prefix match
        case null         => DataResult.success(builder.toMap)
        case m: Map[?, ?] => DataResult.success(m.asInstanceOf[Map[Any, Any]] ++ builder)
        case _            => DataResult.error(() => s"Cannot merge to a non-map: $prefix", Some(prefix))
  }
}

object ObjectOps {
  val INSTANCE: ObjectOps   = ObjectOps(false)
  val COMPRESSED: ObjectOps = ObjectOps(true)
}
