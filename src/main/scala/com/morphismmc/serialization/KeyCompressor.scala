package com.morphismmc.serialization

import scala.collection.mutable

final class KeyCompressor[T](val ops: DynamicOps[T], keyStream: Iterable[T]) {

  private val intToKey: mutable.ArrayBuffer[T] = mutable.ArrayBuffer()
  private val keyToInt: mutable.HashMap[T, Int] = mutable.HashMap()
  private val stringToInt: mutable.HashMap[String, Int] = mutable.HashMap()

  keyStream.foreach { key =>
    if (!keyToInt.contains(key)) {
      val next = keyToInt.size
      keyToInt(key) = next
      ops.getStringValue(key).result.foreach(k => stringToInt(k) = next)
      intToKey += key
    }
  }

  val size: Int = keyToInt.size

  def decompress(key: Int): T = intToKey(key)

  def compress(key: String): Int
    = stringToInt.getOrElse(key, {
      val intKey = compress(ops.createString(key))
      stringToInt(key) = intKey
      intKey
    })

  def compress(key: T): Int = keyToInt(key)
}