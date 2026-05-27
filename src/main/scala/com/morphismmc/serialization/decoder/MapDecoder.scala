package com.morphismmc.serialization.decoder

import com.morphismmc.serialization.{DataResult, DynamicOps, KeyCompressor, Keyable, LifeCycle, MapLike}

import scala.collection.mutable

trait MapDecoder[A] extends Keyable {
  
  def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[A]

  def compressedDecode[T](ops: DynamicOps[T], input: T): DataResult[A] = {
    if (ops.compressMaps) {
      ops.getList(input).result match
        case None           => DataResult.error(() => "Input is not a list")
        case Some(consumer) =>
          val buf = mutable.ArrayBuffer[T]()
          consumer(e => buf += e)
          val comp = compressor(ops)
          val map = new MapLike[T] {
            def get(key: T): Option[T] = buf.lift(comp.compress(key))

            def get(key: String): Option[T] = buf.lift(comp.compress(key))

            def entries: Iterable[(T, T)]
              = buf.zipWithIndex.collect { case (v, i) if v != null => (comp.decompress(i), v)}
          }
          decode(ops, map)
    } else {
      ops.getMap(input).addLifeCycle(LifeCycle.Stable).flatMap(map => decode(ops, map))
    }
  }

  def compressor[T](ops: DynamicOps[T]): KeyCompressor[T]

  private def self: MapDecoder[A] = this

  def decoder: Decoder[A] = new Decoder[A] {
    
    def decode[T](ops: DynamicOps[T], input: T): DataResult[(A, T)] 
      = self.compressedDecode(ops, input).map(r => (r, input))

    override def toString: String = self.toString
  }

  def map[B](f: A => B): MapDecoder[B] = {
    val s = self
    new MapDecoderImpl[B] {
      def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[B] = s.decode(ops, input).map(f)

      def keys[T](ops: DynamicOps[T]): Iterable[T] = s.keys(ops)

      override def toString: String = s"$s[mapped]"
    }
  }

  def flatMap[B](f: A => DataResult[? <: B]): MapDecoder[B] = {
    val s = self
    new MapDecoderImpl[B] {
      def keys[T](ops: DynamicOps[T]): Iterable[T] = s.keys(ops)

      def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[B] 
        = s.decode(ops, input).flatMap(b => f(b).map(identity))

      override def toString: String = s"$s[flatMapped]"
    }
  }

  def ap[E](d: MapDecoder[A => E]): MapDecoder[E] = {
    val s = self
    new MapDecoderImpl[E] {
      def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[E]
        = s.decode(ops, input).flatMap(f => d.decode(ops, input).map(e => e(f)))

      def keys[T](ops: DynamicOps[T]): Iterable[T] = s.keys(ops) ++ d.keys(ops)

      override def toString: String = s"$d * $s"
    }
  }

  def withLifeCycle(lifeCycle: LifeCycle): MapDecoder[A] = {
    val s = self
    new MapDecoderImpl[A]:
      def keys[T](ops: DynamicOps[T]): Iterable[T] = s.keys(ops)

      def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[A] = s.decode(ops, input).addLifeCycle(lifeCycle)

      override def toString: String = s.toString
  }
}