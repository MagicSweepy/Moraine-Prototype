package com.morphismmc.serialization.decoder

import com.morphismmc.serialization.{DataResult, Dynamic, DynamicOps, LifeCycle, MapLike}

trait Decoder[A] {
  
  def decode[T](ops: DynamicOps[T], input: T): DataResult[(A, T)]

  def decode[T](input: Dynamic[T]): DataResult[(A, T)] = decode(input.ops, input.value)

  def parse[T](ops: DynamicOps[T], input: T): DataResult[A] = decode(ops, input).map(_._1)

  def parse[T](input: Dynamic[T]): DataResult[A] = decode(input).map(_._1)

  def terminal: Decoder.Terminal[A] = new Decoder.Terminal[A] {
    def decode[T](ops: DynamicOps[T], input: T): DataResult[A] = parse(ops, input)
  }
  
  def boxed: Decoder.Boxed[A] = new Decoder.Boxed[A] {
    def decode[T](input: Dynamic[T]): DataResult[(A, T)] = Decoder.this.decode(input)
  }
  
  def simple: Decoder.Simple[A] = new Decoder.Simple[A] {
    def decode[T](input: Dynamic[T]): DataResult[A] = parse(input)
  }
  
  def fieldOf(name: String): MapDecoder[A] = FieldDecoder[A](name, this)

  def map[B](f: A => B): Decoder[B] = new Decoder[B] {
    def decode[T](ops: DynamicOps[T], input: T): DataResult[(B, T)] 
      = Decoder.this.decode(ops, input).map((a, t) => (f(a), t))

    override def toString: String = s"$Decoder.this[mapped]"
  }

  def flatMap[B](f: A => DataResult[? <: B]): Decoder[B] = new Decoder[B] {
    def decode[T](ops: DynamicOps[T], input: T): DataResult[(B, T)] 
      = Decoder.this.decode(ops, input).flatMap((a, t) => f(a).map(b => (b, t)))

    override def toString: String = s"$Decoder.this[flatMapped]"
  }

  def promotePartial(onError: String => Unit): Decoder[A] = new Decoder[A] {
    def decode[T](ops: DynamicOps[T], input: T): DataResult[(A, T)] 
      = Decoder.this.decode(ops, input).promotePartial(onError)

    override def toString: String = s"$Decoder.this[promotePartial]"
  }

  def withLifeCycle(lifeCycle: LifeCycle): Decoder[A] = new Decoder[A] {
    def decode[T](ops: DynamicOps[T], input: T): DataResult[(A, T)] 
      = Decoder.this.decode(ops, input).addLifeCycle(lifeCycle)

    override def toString: String = Decoder.this.toString
  }
}

object Decoder {
  trait Terminal[A] {
    def decode[T](ops: DynamicOps[T], input: T): DataResult[A]

    def decoder: Decoder[A] = new Decoder[A] {
      def decode[T](ops: DynamicOps[T], input: T): DataResult[(A, T)] 
        = Terminal.this.decode(ops, input).map(a => (a, ops.empty))

      override def toString: String = s"TerminalDecoder[${Terminal.this}]"
    }
  }

  trait Boxed[A] {
    def decode[T](input: Dynamic[T]): DataResult[(A, T)]

    def decoder: Decoder[A] = new Decoder[A] {
      def decode[T](ops: DynamicOps[T], input: T): DataResult[(A, T)]
        = Boxed.this.decode(new Dynamic[T](ops, input))

      override def toString: String = s"BoxedDecoder[${Boxed.this}]"
    }
  }
  
  trait Simple[A] {
    def decode[T](input: Dynamic[T]): DataResult[A]

    def decoder: Decoder[A] = new Decoder[A] {
      def decode[T](ops: DynamicOps[T], input: T): DataResult[(A, T)] 
        = Simple.this.decode(new Dynamic[T](ops, input)).map(a => (a, ops.empty))

      override def toString: String = s"SimpleDecoder[${Simple.this}]"
    }
  }

  def unit[A](instance: => A): MapDecoder[A] = new MapDecoderImpl[A] {
    def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[A] = DataResult.success(instance)

    def keys[T](ops: DynamicOps[T]): Iterable[T] = Iterable.empty

    override def toString: String = s"UnitDecoder[$instance]"
  }

  def error[A](error: String): Decoder[A] = new Decoder[A] {
    def decode[T](ops: DynamicOps[T], input: T): DataResult[(A, T)] = DataResult.error(() => error)

    override def toString: String = s"ErrorDecoder[$error]"
  }
}