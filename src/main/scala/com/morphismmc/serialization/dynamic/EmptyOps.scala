package com.morphismmc.serialization.dynamic

import com.morphismmc.serialization.*

object EmptyOps extends DynamicOps[Unit] {

  def empty: Unit = ()

  override def emptyMap: Unit = ()

  override def emptyList: Unit = ()

  def convertTo[U](outOps: DynamicOps[U], input: Unit): U = outOps.empty

  def getNumberValue(input: Unit): DataResult[Number] = DataResult.error(() => "Not a number: ()")

  def createNumeric(value: Number): Unit = ()

  def getStringValue(input: Unit): DataResult[String] = DataResult.error(() => "Not a string: ()")

  def createString(value: String): Unit = ()

  def mergeToList(list: Unit, value: Unit): DataResult[Unit] 
    = DataResult.error(() => "Cannot merge to list in EmptyOps")

  def mergeToMap(map: Unit, key: Unit, value: Unit): DataResult[Unit] 
    = DataResult.error(() => "Cannot merge to map in EmptyOps")

  def getMapValues(input: Unit): DataResult[Iterable[(Unit, Unit)]]
    = DataResult.error(() => "Not a map: ()")

  def createMap(entries: Iterable[(Unit, Unit)]): Unit = ()

  def getStream(input: Unit): DataResult[Iterable[Unit]] = DataResult.error(() => "Not a list: ()")

  def createList(input: Iterable[Unit]): Unit = ()

  def remove(input: Unit, key: String): Unit = ()

  override def listBuilder: ListBuilder[Unit] = EmptyListBuilder

  override def mapBuilder: RecordBuilder[Unit] = EmptyRecordBuilder

  override def compressMaps: Boolean = false

  override def toString: String = "Empty"

  private object EmptyListBuilder extends ListBuilder[Unit] {
    def ops: DynamicOps[Unit] = EmptyOps

    def add(value: Unit): ListBuilder[Unit] = this

    def add(value: DataResult[Unit]): ListBuilder[Unit] = this

    def withErrorsFrom(result: DataResult[?]): ListBuilder[Unit] = this

    def mapError(onError: String => String): ListBuilder[Unit] = this

    def build(prefix: Unit): DataResult[Unit] = DataResult.success(())
  }

  private object EmptyRecordBuilder extends RecordBuilder[Unit] {
    def ops: DynamicOps[Unit] = EmptyOps

    def add(key: Unit, value: Unit): RecordBuilder[Unit] = this

    def add(key: Unit, value: DataResult[Unit]): RecordBuilder[Unit] = this

    def add(key: DataResult[Unit], value: DataResult[Unit]): RecordBuilder[Unit] = this

    def withErrorsFrom(result: DataResult[?]): RecordBuilder[Unit] = this

    def setLifecycle(lifecycle: LifeCycle): RecordBuilder[Unit] = this

    def mapError(onError: String => String): RecordBuilder[Unit] = this

    def build(prefix: Unit): DataResult[Unit] = DataResult.success(())
  }
}
