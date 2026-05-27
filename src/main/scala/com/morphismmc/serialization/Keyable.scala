package com.morphismmc.serialization

trait Keyable {
  
  def keys[T](ops: DynamicOps[T]): Iterable[T]
}
