package com.morphismmc.serialization

trait RecordBuilder[T] {
  
  def ops: DynamicOps[T]

  def add(key: T, value: T): RecordBuilder[T]

  def add(key: T, value: DataResult[T]): RecordBuilder[T]

  def add(key: DataResult[T], value: DataResult[T]): RecordBuilder[T]

  def withErrorsFrom(result: DataResult[?]): RecordBuilder[T]

  def setLifecycle(lifecycle: LifeCycle): RecordBuilder[T]

  def mapError(onError: String => String): RecordBuilder[T]

  def build(prefix: T): DataResult[T]

  def build(prefix: DataResult[T]): DataResult[T] = prefix.flatMap(p => build(p))

  def add(key: String, value: T): RecordBuilder[T] =
    add(ops.createString(key), value)

  def add(key: String, value: DataResult[T]): RecordBuilder[T] =
    add(ops.createString(key), value)
}