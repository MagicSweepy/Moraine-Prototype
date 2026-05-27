package com.morphismmc.serialization.codec

import com.morphismmc.serialization.decoder.*
import com.morphismmc.serialization.encoder.*
import com.morphismmc.serialization.*

final class RecordCodecBuilder[O, F](val getter: O => F, val encoder: O => MapEncoder[F], val decoder: MapDecoder[F]) {

  def dependent[E](getter: O => E, enc: MapEncoder[E], decoderGetter: F => MapDecoder[E]): RecordCodecBuilder[O, E] =
    val fDecoder = decoder
    new RecordCodecBuilder[O, E](getter, _ => enc, new MapDecoderImpl[E] {
      def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[E] 
        = fDecoder.decode(ops, input).map(decoderGetter).flatMap(dc => dc.decode(ops, input).map(identity))

      def keys[T](ops: DynamicOps[T]): Iterable[T] = enc.keys(ops)

      override def toString: String = s"Dependent[$enc]"
    })
}

object RecordCodecBuilder {
  
  def of[O, F](getter: O => F, name: String, fieldCodec: Codec[F]): RecordCodecBuilder[O, F] 
    = of(getter, fieldCodec.fieldOf(name))

  def of[O, F](getter: O => F, codec: MapCodec[F]): RecordCodecBuilder[O, F] 
    = RecordCodecBuilder[O, F](getter, _ => codec, codec)

  def point[O, F](instance: F): RecordCodecBuilder[O, F]
    = RecordCodecBuilder[O, F](_ => instance, _ => Encoder.empty[F], Decoder.unit[F](instance))

  def stable[O, F](instance: F): RecordCodecBuilder[O, F] = point(instance, LifeCycle.Stable)

  def deprecated[O, F](instance: F, since: Int): RecordCodecBuilder[O, F]
    = point(instance, LifeCycle.Deprecated(since))

  def point[O, F](instance: F, lifeCycle: LifeCycle): RecordCodecBuilder[O, F]
    = RecordCodecBuilder[O, F](_ => instance, _ => Encoder.empty[F].withLifeCycle(lifeCycle),
      Decoder.unit[F](instance).withLifeCycle(lifeCycle))

  def codec[O](builder: Instance[O] => RecordCodecBuilder[O, O]): Codec[O] = build(builder(instance[O])).codec

  def mapCodec[O](builder: Instance[O] => RecordCodecBuilder[O, O]): MapCodec[O] = build(builder(instance[O]))

  def build[O](builderKind: RecordCodecBuilder[O, O]): MapCodec[O] = {
    val builder = builderKind
    new MapCodec[O] {
      def keys[T](ops: DynamicOps[T]): Iterable[T] = builder.decoder.keys(ops)

      def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[O] = builder.decoder.decode(ops, input)

      def encode[T](input: O, ops: DynamicOps[T], prefix: RecordBuilder[T]): RecordBuilder[T] 
        = builder.encoder(input).encode(input, ops, prefix)

      override def toString: String = s"RecordCodec[${builder.decoder}]"
    }
  }

  def instance[O]: Instance[O] = new Instance[O]

  final class Instance[O] {

    def pure[A](a: A): RecordCodecBuilder[O, A] = point(a)

    def map[A, B](fa: RecordCodecBuilder[O, A], f: A => B): RecordCodecBuilder[O, B]
      = RecordCodecBuilder[O, B](o => f(fa.getter(o)), o => fa.encoder(o).comap[B](_ => fa.getter(o)),
        fa.decoder.map(f))

    def ap[A, B](ff: RecordCodecBuilder[O, A => B], fa: RecordCodecBuilder[O, A]): RecordCodecBuilder[O, B]
      = RecordCodecBuilder[O, B](o => ff.getter(o)(fa.getter(o)), o => RecordMapEncoder[A, B](fa.encoder(o),
        ff.encoder(o), fa.getter(o)), RecordMapDecoder[A, B](fa.decoder, ff.decoder))

    def ap2[A, B, C](ff: RecordCodecBuilder[O, (A, B) => C], fa: RecordCodecBuilder[O, A],
                     fb: RecordCodecBuilder[O, B]): RecordCodecBuilder[O, C]
      = RecordCodecBuilder[O, C](o => ff.getter(o)(fa.getter(o), fb.getter(o)), o => 
        RecordMapEncoder2[A, B, C](ff.encoder(o), fa.encoder(o), fb.encoder(o), fa.getter(o), fb.getter(o)),
        RecordMapDecoder2[A, B, C](ff.decoder, fa.decoder, fb.decoder))
  }
}