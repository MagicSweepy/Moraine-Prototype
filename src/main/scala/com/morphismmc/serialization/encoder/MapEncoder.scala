package com.morphismmc.serialization.encoder

import com.morphismmc.serialization.*

import scala.collection.mutable

trait MapEncoder[A] extends Keyable {
  
  def encode[T](input: A, ops: DynamicOps[T], prefix: RecordBuilder[T]): RecordBuilder[T]

  def compressedBuilder[T](ops: DynamicOps[T]): RecordBuilder[T] = {
    if (ops.compressMaps) {
      MapEncoder.makeCompressedBuilder(ops, compressor(ops))
    } else {
      ops.mapBuilder
    }
  }

  def compressor[T](ops: DynamicOps[T]): KeyCompressor[T]

  def comap[B](f: B => A): MapEncoder[B] = new MapEncoderImpl[B] {
    
    def encode[T](input: B, ops: DynamicOps[T], prefix: RecordBuilder[T]): RecordBuilder[T]
      = MapEncoder.this.encode(f(input), ops, prefix)

    def keys[T](ops: DynamicOps[T]): Iterable[T] = MapEncoder.this.keys(ops)

    override def toString: String = s"$MapEncoder.this[comapped]"
  }

  def flatComap[B](f: B => DataResult[? <: A]): MapEncoder[B] = new MapEncoderImpl[B] {
    
    def keys[T](ops: DynamicOps[T]): Iterable[T] = MapEncoder.this.keys(ops)

    def encode[T](input: B, ops: DynamicOps[T], prefix: RecordBuilder[T]): RecordBuilder[T] = {
      val aResult = f(input)
      val builder = prefix.withErrorsFrom(aResult)
      aResult.map(r => MapEncoder.this.encode(r, ops, builder)).result.getOrElse(builder)
    }

    override def toString: String = s"$MapEncoder.this[flatComapped]"
  }

  def encoder: Encoder[A] = new Encoder[A] {
    
    def encode[T](input: A, ops: DynamicOps[T], prefix: T): DataResult[T]
      = MapEncoder.this.encode(input, ops, compressedBuilder(ops)).build(prefix)

    override def toString: String = MapEncoder.this.toString
  }

  def withLifeCycle(lifeCycle: LifeCycle): MapEncoder[A] = new MapEncoderImpl[A] {
    
    def keys[T](ops: DynamicOps[T]): Iterable[T] = MapEncoder.this.keys(ops)

    def encode[T](input: A, ops: DynamicOps[T], prefix: RecordBuilder[T]): RecordBuilder[T]
      = MapEncoder.this.encode(input, ops, prefix).setLifecycle(lifeCycle)

    override def toString: String = MapEncoder.this.toString
  }
}

object MapEncoder {

  def makeCompressedBuilder[T](dynOps: DynamicOps[T], compressor: KeyCompressor[T]): RecordBuilder[T]
    = new RecordBuilder.AbstractUniversalBuilder[T, mutable.ArrayBuffer[T]](dynOps) {
      protected def initBuilder: mutable.ArrayBuffer[T] = {
        val buf = mutable.ArrayBuffer.fill[T](compressor.size)(null.asInstanceOf[T])
        buf
      }
  
      protected def append(key: T, value: T, builder: mutable.ArrayBuffer[T]): mutable.ArrayBuffer[T] = {
        builder(compressor.compress(key)) = value
        builder
      }
  
      protected def build(builder: mutable.ArrayBuffer[T], prefix: T): DataResult[T]
      = dynOps.mergeToList(prefix, builder.toSeq)
    }
}