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

object RecordBuilder {

  abstract class AbstractBuilder[T, R](val ops: DynamicOps[T]) extends RecordBuilder[T] {
    
    protected var builder: DataResult[R] = DataResult.success(initBuilder, LifeCycle.Stable)

    protected def initBuilder: R

    protected def build(builder: R, prefix: T): DataResult[T]

    def build(prefix: T): DataResult[T] = {
      val result = builder.flatMap(b => build(b, prefix))
      builder = DataResult.success(initBuilder, LifeCycle.Stable)
      result
    }

    def withErrorsFrom(result: DataResult[?]): RecordBuilder[T] = {
      builder = builder.flatMap(v => result.map(_ => v))
      this
    }

    def setLifecycle(lifecycle: LifeCycle): RecordBuilder[T] = {
      builder = builder.addLifeCycle(lifecycle)
      this
    }

    def mapError(onError: String => String): RecordBuilder[T] = {
      builder = builder.mapError(onError)
      this
    }
  }
    
  abstract class AbstractStringBuilder[T, R](ops: DynamicOps[T]) extends AbstractBuilder[T, R](ops) {
    
    protected def append(key: String, value: T, builder: R): R

    override def add(key: String, value: T): RecordBuilder[T] = {
      builder = builder.map(b => append(key, value, b))
      this
    }

    override def add(key: String, value: DataResult[T]): RecordBuilder[T] = {
      builder = builder.apply2stable((b, v) => append(key, v, b), value)
      this
    }

    def add(key: T, value: T): RecordBuilder[T] = {
      builder = ops.getStringValue(key).flatMap { k =>
        add(k, value)
        builder
      }
      this
    }

    def add(key: T, value: DataResult[T]): RecordBuilder[T] = {
      builder = ops.getStringValue(key).flatMap { k =>
        add(k, value)
        builder
      }
      this
    }

    def add(key: DataResult[T], value: DataResult[T]): RecordBuilder[T] = {
      builder = key.flatMap(k => ops.getStringValue(k)).flatMap { k =>
        add(k, value)
        builder
      }
      this
    }
  }

  abstract class AbstractUniversalBuilder[T, R](ops: DynamicOps[T]) extends AbstractBuilder[T, R](ops) {
    
    protected def append(key: T, value: T, builder: R): R

    def add(key: T, value: T): RecordBuilder[T] = {
      builder = builder.map(b => append(key, value, b))
      this
    }

    def add(key: T, value: DataResult[T]): RecordBuilder[T] = {
      builder = builder.apply2stable((b, v) => append(key, v, b), value)
      this
    }

    def add(key: DataResult[T], value: DataResult[T]): RecordBuilder[T] = {
      builder = builder.ap(key.apply2stable[T, R => R]((k: T, v: T) => (b: R) => append(k, v, b), value))
      this
    }
  }
}