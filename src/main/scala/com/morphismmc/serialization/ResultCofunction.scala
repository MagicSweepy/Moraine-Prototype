package com.morphismmc.serialization

trait ResultCofunction[A] {
 
  def apply[T](ops: DynamicOps[T], input: T, a: DataResult[(A, T)]): DataResult[(A, T)]

  def coApply[T](ops: DynamicOps[T], input: A, t: DataResult[T]): DataResult[T]
}