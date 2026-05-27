package com.morphismmc.serialization.encoder

import com.morphismmc.serialization.{DataResult, DynamicOps, LifeCycle, RecordBuilder}

trait Encoder[A] {
  
  def encode[T](input: A, ops: DynamicOps[T], prefix: T): DataResult[T]

  def encodeStart[T](ops: DynamicOps[T], input: A): DataResult[T] = encode(input, ops, ops.empty)

  def fieldOf(name: String): MapEncoder[A] = new FieldEncoder[A](name, this)

  def comap[B](f: B => A): Encoder[B] = new Encoder[B] {
    def encode[T](input: B, ops: DynamicOps[T], prefix: T): DataResult[T] = {
      Encoder.this.encode(f(input), ops, prefix)
    }

    override def toString: String = s"$Encoder.this[comapped]"
  }

  def flatComap[B](f: B => DataResult[? <: A]): Encoder[B] = new Encoder[B] {
    def encode[T](input: B, ops: DynamicOps[T], prefix: T): DataResult[T] = {
      f(input).flatMap(a => Encoder.this.encode(a, ops, prefix))
    }

    override def toString: String = s"$Encoder.this[flatComapped]"
  }

  def withLifeCycle(lifeCycle: LifeCycle): Encoder[A] = new Encoder[A] {
    def encode[T](input: A, ops: DynamicOps[T], prefix: T): DataResult[T] = {
      Encoder.this.encode(input, ops, prefix).addLifeCycle(lifeCycle)
    }
  }
  
  override def toString: String = Encoder.this.toString
}

object Encoder {
  def empty[A]: MapEncoder[A] = new MapEncoderImpl[A] {
    
    def encode[T](input: A, ops: DynamicOps[T], prefix: RecordBuilder[T]): RecordBuilder[T] = prefix

    def keys[T](ops: DynamicOps[T]): Iterable[T] = Iterable.empty

    override def toString: String = "EmptyEncoder"
  }

  def error[A](error: String): Encoder[A] = new Encoder[A] {
    
    def encode[T](input: A, ops: DynamicOps[T], prefix: T): DataResult[T] = DataResult.error(() => s"$error $input")

    override def toString: String = s"ErrorEncoder[$error]"
  }
}