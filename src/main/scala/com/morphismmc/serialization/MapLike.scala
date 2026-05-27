package com.morphismmc.serialization

trait MapLike[T] {
  
  def get(key: T): Option[T]

  def get(key: String): Option[T]

  def entries: Iterable[(T, T)]
}

object MapLike {
  
  def forMap[T](map: Map[T, T], createStringKey: String => T): MapLike[T] = new MapLike[T] {
    
    def get(key: T): Option[T] = map.get(key)

    def get(key: String): Option[T] = map.get(createStringKey(key))

    def entries: Iterable[(T, T)] = map
  }
}