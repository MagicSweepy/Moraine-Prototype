package com.morphismmc.serialization

trait ListBuilder[T] {
  
  def ops: DynamicOps[T]

  def add(value: T): ListBuilder[T]

  def add(value: DataResult[T]): ListBuilder[T]

  def withErrorsFrom(result: DataResult[?]): ListBuilder[T]

  def mapError(onError: String => String): ListBuilder[T]

  def build(prefix: T): DataResult[T]

  def build(prefix: DataResult[T]): DataResult[T] = prefix.flatMap(p => build(p))
}
