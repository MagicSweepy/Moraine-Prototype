package com.morphismmc.serialization.codec

import com.morphismmc.serialization.encoder.{Encoder, MapEncoder}
import com.morphismmc.serialization.decoder.{Decoder, MapDecoder}
import com.morphismmc.serialization.{DataResult, Dynamic, DynamicOps, Keyable, LifeCycle, MapLike, RecordBuilder, ResultCofunction}
import java.nio.ByteBuffer

trait Codec[A] extends Encoder[A] with Decoder[A] {

  // region Life Cycle

  override def withLifeCycle(lifeCycle: LifeCycle): Codec[A] = new Codec[A] {
    def encode[T](input: A, ops: DynamicOps[T], prefix: T): DataResult[T] 
      = Codec.this.encode(input, ops, prefix).addLifeCycle(lifeCycle)

    def decode[T](ops: DynamicOps[T], input: T): DataResult[(A, T)]
      = Codec.this.decode(ops, input).addLifeCycle(lifeCycle)

    override def toString: String = Codec.this.toString
  }

  def stable: Codec[A] = withLifeCycle(LifeCycle.Stable)

  def deprecated(since: Int): Codec[A] = withLifeCycle(LifeCycle.Deprecated(since))

  // endregion

  // region Field Codec

  override def fieldOf(name: String): MapCodec[A]
    = MapCodec.of(super[Encoder].fieldOf(name), super[Decoder].fieldOf(name), () => s"Field[$name: $this]")

  def optionalFieldOf(name: String): MapCodec[Option[A]] = OptionalFieldCodec[A](name, this, false)

  def optionalFieldOf(name: String, default: A): MapCodec[A] = optionalFieldOf(name, default, false)

  def optionalFieldOf(name: String, default: A, lenient: Boolean): MapCodec[A]
    = OptionalFieldCodec[A](name, this, lenient).xmap(_.getOrElse(default), a => if a == default then None else Some(a))

  def lenientOptionalFieldOf(name: String): MapCodec[Option[A]] = OptionalFieldCodec[A](name, this, true)

  def lenientOptionalFieldOf(name: String, default: A): MapCodec[A] = optionalFieldOf(name, default, true)

  // endregion

  // region Mapper

  def xmap[B](to: A => B, from: B => A): Codec[B] = Codec.of(comap(from), map(to), s"$this[xmapped]")

  def comapFlatMap[B](to: A => DataResult[? <: B], from: B => A): Codec[B]
    = Codec.of(comap(from), flatMap(to), s"$this[comapFlatMapped]")

  def flatComapMap[B](to: A => B, from: B => DataResult[? <: A]): Codec[B]
    = Codec.of(flatComap(from), map(to), s"$this[flatComapMapped]")

  def flatXmap[B](to: A => DataResult[? <: B], from: B => DataResult[? <: A]): Codec[B]
    = Codec.of(flatComap(from), flatMap(to), s"$this[flatXmapped]")

  def mapResult(f: ResultCofunction[A]): Codec[A] = new Codec[A] {
    def encode[T](input: A, ops: DynamicOps[T], prefix: T): DataResult[T]
      = f.coApply(ops, input, Codec.this.encode(input, ops, prefix))

    def decode[T](ops: DynamicOps[T], input: T): DataResult[(A, T)] = f.apply(ops, input, Codec.this.decode(ops, input))

    override def toString: String = s"${Codec.this}[mapResult $f]"
  }
  
  def validate(checker: A => DataResult[A]): Codec[A] = flatXmap(checker, checker)

  // endregion

  // region Accessor

  def orElse(onError: String => String, value: A): Codec[A] = mapResult(new ResultCofunction[A] {
    def apply[T](ops: DynamicOps[T], input: T, a: DataResult[(A, T)]): DataResult[(A, T)]
      = DataResult.success(a.mapError(onError).result.getOrElse((value, input)))

    def coApply[T](ops: DynamicOps[T], input: A, t: DataResult[T]): DataResult[T] = t.mapError(onError)

    override def toString: String = s"OrElse[$onError $value]"
  })

  def orElseGet(onError: String => String, value: () => A): Codec[A] = mapResult(new ResultCofunction[A] {
    def apply[T](ops: DynamicOps[T], input: T, a: DataResult[(A, T)]): DataResult[(A, T)]
      = DataResult.success(a.mapError(onError).result.getOrElse((value(), input)))

    def coApply[T](ops: DynamicOps[T], input: A, t: DataResult[T]): DataResult[T] = t.mapError(onError)

    override def toString: String = s"OrElseGet[$onError ${value()}]"
  })

  def orElse(value: A): Codec[A] = mapResult(new ResultCofunction[A] {
    def apply[T](ops: DynamicOps[T], input: T, a: DataResult[(A, T)]): DataResult[(A, T)]
      = DataResult.success(a.result.getOrElse((value, input)))

    def coApply[T](ops: DynamicOps[T], input: A, t: DataResult[T]): DataResult[T] = t

    override def toString: String = s"OrElse[$value]"
  })

  def orElseGet(value: () => A): Codec[A] = mapResult(new ResultCofunction[A] {
    def apply[T](ops: DynamicOps[T], input: T, a: DataResult[(A, T)]): DataResult[(A, T)]
      = DataResult.success(a.result.getOrElse((value(), input)))

    def coApply[T](ops: DynamicOps[T], input: A, t: DataResult[T]): DataResult[T] = t

    override def toString: String = s"OrElseGet[${value()}]"
  })

  override def promotePartial(onError: String => Unit): Codec[A]
    = Codec.of(this, super[Decoder].promotePartial(onError))

  // endregion

  // region Dispatcher

  def dispatch[E](typeFn: E => A, codec: A => MapCodec[? <: E]): Codec[E]
    = dispatch("type", typeFn, codec)

  def dispatch[E](typeKey: String, typeFn: E => A, codec: A => MapCodec[? <: E]): Codec[E]
    = fieldOf(typeKey).dispatch(typeFn, codec)

  def dispatchStable[E](typeFn: E => A, codec: A => MapCodec[? <: E]): Codec[E]
    = fieldOf("type").dispatchStable(typeFn, codec)

  def partialDispatch[E](typeKey: String,
                         typeFn: E => DataResult[? <: A],
                         codec: A => DataResult[? <: MapCodec[? <: E]]): Codec[E]
    = fieldOf(typeKey).partialDispatch(typeFn, codec)

  def dispatchMap[E](typeFn: E => A, codec: A => MapCodec[? <: E]): MapCodec[E]
    = dispatchMap("type", typeFn, codec)

  def dispatchMap[E](typeKey: String, typeFn: E => A, codec: A => MapCodec[? <: E]): MapCodec[E]
    = fieldOf(typeKey).dispatchMap(typeFn, codec)

  // endregion
}

object Codec {

  def of[A](encoder: Encoder[A], decoder: Decoder[A]): Codec[A] = of(encoder, decoder, s"Codec[$encoder $decoder]")

  def of[A](encoder: Encoder[A], decoder: Decoder[A], name: String): Codec[A] = new Codec[A] {
    def decode[T](ops: DynamicOps[T], input: T): DataResult[(A, T)] = decoder.decode(ops, input)

    def encode[T](input: A, ops: DynamicOps[T], prefix: T): DataResult[T] = encoder.encode(input, ops, prefix)

    override def toString: String = name
  }

  def of[A](enc: MapEncoder[A], dec: MapDecoder[A]): MapCodec[A] = of(enc, dec, () => s"MapCodec[$enc $dec]")

  def of[A](enc: MapEncoder[A], dec: MapDecoder[A], name: () => String): MapCodec[A] = new MapCodec[A] {
    def keys[T](ops: DynamicOps[T]): Iterable[T] = enc.keys(ops) ++ dec.keys(ops)

    def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[A] = dec.decode(ops, input)

    def encode[T](input: A, ops: DynamicOps[T], prefix: RecordBuilder[T]): RecordBuilder[T] = enc.encode(input, ops, prefix)

    override def toString: String = name()
  }

  // region Tuple Codec

  def pair[T1, T2](t1: Codec[T1], t2: Codec[T2]): Codec[(T1, T2)] = PairCodec[T1, T2](t1, t2)

  def triple[T1, T2, T3](t1: Codec[T1], t2: Codec[T2], t3: Codec[T3]): Codec[(T1, T2, T3)]
    = TripleCodec[T1, T2, T3](t1, t2, t3)

  def mapPair[T1, T2](t1: MapCodec[T1], t2: MapCodec[T2]): MapCodec[(T1, T2)] = PairMapCodec[T1, T2](t1, t2)

  def mapTriple[T1, T2, T3](t1: MapCodec[T1], t2: MapCodec[T2], t3: MapCodec[T3]): MapCodec[(T1, T2, T3)]
    = TripleMapCodec[T1, T2, T3](t1, t2, t3)

  // endregion

  // region Choice Codec

  def either[L, R](left: Codec[L], right: Codec[R]): Codec[Either[L, R]] = EitherCodec[L, R](left, right)

  def xor[F, S](first: Codec[F], second: Codec[S]): Codec[Either[F, S]] = XorCodec[F, S](first, second)

  def maybe[T](value: Codec[T]): Codec[Option[T]] = MaybeCodec[T](value)

  def mapEither[L, R](left: MapCodec[L], right: MapCodec[R]): MapCodec[Either[L, R]] = EitherMapCodec[L, R](left, right)

  def mapXor[L, R](left: MapCodec[L], right: MapCodec[R]): MapCodec[Either[L, R]] = XorMapCodec[L, R](left, right)

  def mapMaybe[T](value: MapCodec[T]): MapCodec[Option[T]] = MaybeMapCodec[T](value)

  // endregion

  // region List / Map Codec

  def list[E](elementCodec: Codec[E]): Codec[List[E]] = list(elementCodec, 0, Int.MaxValue)

  def list[E](elementCodec: Codec[E], maxSize: Int): Codec[List[E]] = list(elementCodec, 0, maxSize)

  def list[E](elementCodec: Codec[E], minSize: Int, maxSize: Int): Codec[List[E]] 
    = ListCodec[E](elementCodec, minSize, maxSize)

  def compoundList[K, V](keyCodec: Codec[K], elementCodec: Codec[V]): Codec[List[(K, V)]] 
    = CompoundListCodec[K, V](keyCodec, elementCodec)

  def simpleMap[K, V](keyCodec: Codec[K], elementCodec: Codec[V], keys: Keyable): SimpleMapCodec[K, V]
    = SimpleMapCodec[K, V](keyCodec, elementCodec, keys)

  def unboundedMap[K, V](keyCodec: Codec[K], elementCodec: Codec[V]): UnboundedMapCodec[K, V]
    = UnboundedMapCodec[K, V](keyCodec, elementCodec)

  def strictUnboundedMap[K, V](keyCodec: Codec[K], elementCodec: Codec[V]): StrictUnboundedMapCodec[K, V] 
    = StrictUnboundedMapCodec[K, V](keyCodec, elementCodec)

  // endregion

  // region Field Codec

  def optionalField[F](name: String, elementCodec: Codec[F], lenient: Boolean): MapCodec[Option[F]] 
    = OptionalFieldCodec[F](name, elementCodec, lenient)

  def recursive[A](name: String, wrapped: Codec[A] => Codec[A]): Codec[A] = RecursiveCodec[A](name, wrapped)

  def lazyRecursive[A](delegate: () => Codec[A]): Codec[A] = RecursiveCodec[A](delegate.toString, _ => delegate())

  // endregion

  // region Primitive Codec

  val BOOL: PrimitiveCodec[Boolean] = new PrimitiveCodec[Boolean] {
    def read[T](ops: DynamicOps[T], input: T): DataResult[Boolean] = ops.getBooleanValue(input)
    
    def write[T](ops: DynamicOps[T], value: Boolean): T = ops.createBoolean(value)

    override def toString: String = "Bool"
  }

  val BYTE: PrimitiveCodec[Byte] = new PrimitiveCodec[Byte] {
    def read[T](ops: DynamicOps[T], input: T): DataResult[Byte] = ops.getNumberValue(input).map(_.byteValue)

    def write[T](ops: DynamicOps[T], value: Byte): T = ops.createByte(value)

    override def toString: String = "Byte"
  }

  val SHORT: PrimitiveCodec[Short] = new PrimitiveCodec[Short] {
    def read[T](ops: DynamicOps[T], input: T): DataResult[Short] = ops.getNumberValue(input).map(_.shortValue)

    def write[T](ops: DynamicOps[T], value: Short): T = ops.createShort(value)

    override def toString: String = "Short"
  }

  val INT: PrimitiveCodec[Int] = new PrimitiveCodec[Int] {
    def read[T](ops: DynamicOps[T], input: T): DataResult[Int] = ops.getNumberValue(input).map(_.intValue)

    def write[T](ops: DynamicOps[T], value: Int): T = ops.createInt(value)

    override def toString: String = "Int"
  }

  val LONG: PrimitiveCodec[Long] = new PrimitiveCodec[Long] {
    def read[T](ops: DynamicOps[T], input: T): DataResult[Long] = ops.getNumberValue(input).map(_.longValue)

    def write[T](ops: DynamicOps[T], value: Long): T = ops.createLong(value)

    override def toString: String = "Long"
  }

  val FLOAT: PrimitiveCodec[Float] = new PrimitiveCodec[Float] {
    def read[T](ops: DynamicOps[T], input: T): DataResult[Float] = ops.getNumberValue(input).map(_.floatValue)

    def write[T](ops: DynamicOps[T], value: Float): T = ops.createFloat(value)

    override def toString: String = "Float"
  }

  val DOUBLE: PrimitiveCodec[Double] = new PrimitiveCodec[Double] {
    def read[T](ops: DynamicOps[T], input: T): DataResult[Double] = ops.getNumberValue(input).map(_.doubleValue)

    def write[T](ops: DynamicOps[T], value: Double): T = ops.createDouble(value)

    override def toString: String = "Double"
  }

  val STRING: PrimitiveCodec[String] = new PrimitiveCodec[String] {
    def read[T](ops: DynamicOps[T], input: T): DataResult[String] = ops.getStringValue(input)

    def write[T](ops: DynamicOps[T], value: String): T = ops.createString(value)

    override def toString: String = "String"
  }

  val BYTE_BUFFER: PrimitiveCodec[ByteBuffer] = new PrimitiveCodec[ByteBuffer] {
    def read[T](ops: DynamicOps[T], input: T): DataResult[ByteBuffer] = ops.getByteBuffer(input)

    def write[T](ops: DynamicOps[T], value: ByteBuffer): T = ops.createByteList(value)

    override def toString: String = "ByteBuffer"
  }

  val PASSTHROUGH: Codec[Dynamic[?]] = new Codec[Dynamic[?]] {
    def decode[T](ops: DynamicOps[T], input: T): DataResult[(Dynamic[?], T)] = {
      val dyn: Dynamic[?] = new Dynamic(ops.asInstanceOf[DynamicOps[Any]], input.asInstanceOf[Any])
      DataResult.success((dyn, ops.empty))
    }

    def encode[T](input: Dynamic[?], ops: DynamicOps[T], prefix: T): DataResult[T] = {
      if (input.value == input.ops.empty) {
        DataResult.success(prefix, LifeCycle.Experimental)
      } else {
        val casted = input.convert(ops).value
        if (prefix == ops.empty) {
          DataResult.success(casted, LifeCycle.Experimental)
        } else {
          ops.getMap(casted)
              .flatMap(map => ops.mergeToMap(prefix, map))
              .result.map(DataResult.success(_)).getOrElse {
                ops.getStream(casted).flatMap(stream => ops.mergeToList(prefix, stream.toSeq))
                    .result.map(DataResult.success(_))
                    .getOrElse(DataResult.error(() => s"Don't know how to merge $prefix and $casted",
                      Some(prefix), LifeCycle.Experimental))
              }
        }
      }
    }

    override def toString: String = "passthrough"
  }

  val EMPTY: MapCodec[Unit] = of(Encoder.empty[Unit], Decoder.unit[Unit](()))

  // endregion
}
