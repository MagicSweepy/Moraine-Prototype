package com.morphismmc.serialization.codec

import com.morphismmc.serialization.encoder.{Encoder, MapEncoder}
import com.morphismmc.serialization.decoder.{Decoder, MapDecoder}
import com.morphismmc.serialization.{CompressorHolder, DataResult, DynamicOps, LifeCycle, MapLike, RecordBuilder, ResultFunction}

abstract class MapCodec[A] extends CompressorHolder with MapEncoder[A] with MapDecoder[A] {

  def forGetter[O](getter: O => A): RecordCodecBuilder[O, A] = RecordCodecBuilder.of(getter, this)

  def codec: Codec[A] = new MapCodecCodec[A](this)

  override def withLifeCycle(lifeCycle: LifeCycle): MapCodec[A] = new MapCodec[A] {
    def keys[T](ops: DynamicOps[T]): Iterable[T] = MapCodec.this.keys(ops)

    def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[A]
      = MapCodec.this.decode(ops, input).addLifeCycle(lifeCycle)

    def encode[T](input: A, ops: DynamicOps[T], prefix: RecordBuilder[T]): RecordBuilder[T]
      = MapCodec.this.encode(input, ops, prefix).setLifecycle(lifeCycle)

    override def toString: String = MapCodec.this.toString
  }

  def stable: MapCodec[A] = withLifeCycle(LifeCycle.Stable)

  def deprecated(since: Int): MapCodec[A] = withLifeCycle(LifeCycle.Deprecated(since))

  def xmap[B](to: A => B, from: B => A): MapCodec[B] = MapCodec.of(comap(from), map(to), () => s"$this[xmapped]")

  def flatXmap[B](to: A => DataResult[? <: B], from: B => DataResult[? <: A]): MapCodec[B] 
    = Codec.of(flatComap(from), flatMap(to), () => s"$toString[flatXmapped]")

  def mapResult(f: ResultFunction[A]): MapCodec[A] = new MapCodec[A] {
    def keys[T](ops: DynamicOps[T]): Iterable[T] = MapCodec.this.keys(ops)

    def encode[T](input: A, ops: DynamicOps[T], prefix: RecordBuilder[T]): RecordBuilder[T] 
      = f.coApply(ops, input, MapCodec.this.encode(input, ops, prefix))

    def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[A]
      = f.apply(ops, input, MapCodec.this.decode(ops, input))

    override def toString: String = s"${MapCodec.this}[mapResult $f]"
  }

  def dispatch[E](typeFn: E => A, codec: A => MapCodec[? <: E]): Codec[E] 
    = partialDispatch(e => DataResult.success(typeFn(e)), a => DataResult.success(codec(a)))

  def dispatchStable[E](typeFn: E => A, codec: A => MapCodec[? <: E]): Codec[E] 
    = partialDispatch(e => DataResult.success(typeFn(e), LifeCycle.Stable), a => DataResult.success(codec(a), LifeCycle.Stable))

  def partialDispatch[E](typeFn: E => DataResult[? <: A], codec: A => DataResult[? <: MapCodec[? <: E]]): Codec[E] 
    = KeyDispatchCodec[E, A](this, typeFn, codec).codec

  def dispatchMap[E](typeFn: E => A, codec: A => MapCodec[? <: E]): MapCodec[E] 
    = KeyDispatchCodec[E, A](this, e => DataResult.success(typeFn(e)), a => DataResult.success(codec(a)))
}

object MapCodec {
  
  def of[A](enc: MapEncoder[A], dec: MapDecoder[A]): MapCodec[A] = of(enc, dec, () => s"MapCodec[$enc $dec]")

  def of[A](enc: MapEncoder[A], dec: MapDecoder[A], name: () => String): MapCodec[A] = new MapCodec[A] {
    def keys[T](ops: DynamicOps[T]): Iterable[T] = enc.keys(ops) ++ dec.keys(ops)

    def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[A] = dec.decode(ops, input)

    def encode[T](input: A, ops: DynamicOps[T], prefix: RecordBuilder[T]): RecordBuilder[T] = enc.encode(input, ops, prefix)

    override def toString: String = name()
  }

  def unit[A](default: => A): MapCodec[A] = of(Encoder.empty[A], Decoder.unit[A](default))
}

final class MapCodecCodec[A](mapCodec: MapCodec[A]) extends Codec[A] {
  def decode[T](ops: DynamicOps[T], input: T): DataResult[(A, T)] 
    = mapCodec.compressedDecode(ops, input).map(r => (r, input))

  def encode[T](input: A, ops: DynamicOps[T], prefix: T): DataResult[T] 
    = mapCodec.encode(input, ops, mapCodec.compressedBuilder(ops)).build(prefix)

  override def toString: String = mapCodec.toString
}