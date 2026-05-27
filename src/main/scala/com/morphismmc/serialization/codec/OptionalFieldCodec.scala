package com.morphismmc.serialization.codec

import com.morphismmc.serialization.{DataResult, DynamicOps, MapLike, RecordBuilder}

final class OptionalFieldCodec[A](name: String, element: Codec[A], lenient: Boolean) extends MapCodec[Option[A]] {

  def keys[T](ops: DynamicOps[T]): Iterable[T] = Iterable(ops.createString(name))

  def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[Option[A]] = input.get(name) match
    case None    => DataResult.success(None)
    case Some(v) =>
      val parsed = element.parse(ops, v)
      if (parsed.isError && lenient) {
        DataResult.success(None)
      } else {
        parsed.map(Some(_)).setPartial(parsed.resultOrPartial)
      }

  def encode[T](input: Option[A], ops: DynamicOps[T], prefix: RecordBuilder[T]): RecordBuilder[T] = input match
    case Some(a) => prefix.add(name, element.encodeStart(ops, a))
    case None    => prefix

  override def toString: String = s"OptionalFieldCodec[$name: $element]"
}
