package com.morphismmc.serialization

trait ResultFunction[A] {
  
  def apply[T](ops: DynamicOps[T], input: MapLike[T], a: DataResult[A]): DataResult[A]

  def coApply[T](ops: DynamicOps[T], input: A, t: RecordBuilder[T]): RecordBuilder[T]
}