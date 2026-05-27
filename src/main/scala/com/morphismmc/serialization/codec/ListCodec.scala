package com.morphismmc.serialization.codec

import com.morphismmc.serialization.{DataResult, DynamicOps, LifeCycle}
import scala.collection.mutable

final class ListCodec[A](element: Codec[A], minSize: Int, maxSize: Int) extends Codec[List[A]] {
  
  def encode[T](input: List[A], ops: DynamicOps[T], prefix: T): DataResult[T] = {
    if (input.size < minSize) {
      createTooShortError(input.size)
    } else if (input.size > maxSize) {
      createTooLongError(input.size)
    } else {
      val builder = ops.listBuilder
      input.foreach(a => builder.add(element.encodeStart(ops, a)))
      builder.build(prefix)
    }
  }

  def decode[T](ops: DynamicOps[T], input: T): DataResult[(List[A], T)] = {
    ops.getList(input).addLifeCycle(LifeCycle.Stable).flatMap { consumer =>
      val state = new DecoderState[T](ops)
      consumer(e => state.accept(e))
      state.build()
    }
  }

  private def createTooShortError[R](size: Int): DataResult[R] 
    = DataResult.error(() => s"List is too short: $size, expected range [$minSize-$maxSize]")

  private def createTooLongError[R](size: Int): DataResult[R] 
    = DataResult.error(() => s"List is too long: $size, expected range [$minSize-$maxSize]")

  override def toString: String = s"ListCodec[$element]"

  private class DecoderState[T](ops: DynamicOps[T]) {
    private val elements = mutable.ListBuffer[A]()
    private val failed = mutable.ListBuffer[T]()
    private var result: DataResult[Unit] = DataResult.success((), LifeCycle.Stable)
    private var totalCount = 0

    def accept(value: T): Unit = {
      totalCount += 1
      if (elements.size >= maxSize) {
        failed += value
      } else {
        val elemResult = element.decode(ops, value)
        elemResult.error.foreach(_ => failed += value)
        elemResult.resultOrPartial.foreach(p => elements += p._1)
        result = result.apply2stable((_, _) => (), elemResult)
      }
    }

    def build(): DataResult[(List[A], T)] = {
      if (elements.size < minSize) {
        createTooShortError(elements.size)
      } else {
        val errors = ops.createList(failed.toList)
        val pair = (elements.toList, errors)
        if totalCount > maxSize then result = createTooLongError(totalCount)
        result.map(_ => pair).setPartial(pair)
      }
    }
  }
}