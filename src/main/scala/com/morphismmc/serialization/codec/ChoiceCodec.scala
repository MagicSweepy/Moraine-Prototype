package com.morphismmc.serialization.codec

import com.morphismmc.serialization.{DataResult, DynamicOps, MapLike, RecordBuilder}

final class EitherCodec[L, R](left: Codec[L], right: Codec[R]) extends Codec[Either[L, R]] {
  def decode[T](ops: DynamicOps[T], input: T): DataResult[(Either[L, R], T)] = {
    val lRead = left.decode(ops, input).map((p: (L, T)) => (Left(p._1): Either[L, R], p._2))
    if (lRead.result.isDefined) {
      lRead
    } else {
      right.decode(ops, input).map((p: (R, T)) => (Right(p._1): Either[L, R], p._2))
    }
  }

  def encode[T](input: Either[L, R], ops: DynamicOps[T], prefix: T): DataResult[T]
    = input.fold(l => left.encode(l, ops, prefix), r => right.encode(r, ops, prefix))

  override def toString: String = s"EitherCodec[$left, $right]"
}

final class EitherMapCodec[L, R](left: MapCodec[L], right: MapCodec[R]) extends MapCodec[Either[L, R]] {
  def keys[T](ops: DynamicOps[T]): Iterable[T] = left.keys(ops) ++ right.keys(ops)

  def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[Either[L, R]] = {
    val lRead = left.decode(ops, input).map(Left(_): Either[L, R])
    if lRead.result.isDefined then lRead
    else right.decode(ops, input).map(Right(_): Either[L, R])
  }

  def encode[T](input: Either[L, R], ops: DynamicOps[T], prefix: RecordBuilder[T]): RecordBuilder[T]
    = input.fold(l => left.encode(l, ops, prefix), r => right.encode(r, ops, prefix))

  override def toString: String = s"EitherMapCodec[$left, $right]"
}

final class XorCodec[F, S](first: Codec[F], second: Codec[S]) extends Codec[Either[F, S]] {
  def decode[T](ops: DynamicOps[T], input: T): DataResult[(Either[F, S], T)] = {
    val fRead = first.decode(ops, input).map((p: (F, T)) => (Left(p._1): Either[F, S], p._2))
    if (fRead.isSuccess) {
      fRead
    } else {
      second.decode(ops, input).map((p: (S, T)) => (Right(p._1): Either[F, S], p._2)).promotePartial(_ => ())
    }
  }

  def encode[T](input: Either[F, S], ops: DynamicOps[T], prefix: T): DataResult[T] 
    = input.fold(f => first.encode(f, ops, prefix), s => second.encode(s, ops, prefix))

  override def toString: String = s"XorCodec[$first, $second]"
}

final class XorMapCodec[F, S](first: MapCodec[F], second: MapCodec[S]) extends MapCodec[Either[F, S]] {
  def keys[T](ops: DynamicOps[T]): Iterable[T] = first.keys(ops) ++ second.keys(ops)

  def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[Either[F, S]] = {
    val fRead = first.decode(ops, input).map(Left(_): Either[F, S])
    if (fRead.isSuccess) {
      fRead
    } else {
      second.decode(ops, input).map(Right(_): Either[F, S]).promotePartial(_ => ())
    }
  }

  def encode[T](input: Either[F, S], ops: DynamicOps[T], prefix: RecordBuilder[T]): RecordBuilder[T] 
    = input.fold(f => first.encode(f, ops, prefix), s => second.encode(s, ops, prefix))

  override def toString: String = s"XorMapCodec[$first, $second]"
}

final class MaybeCodec[A](codec: Codec[A]) extends Codec[Option[A]] {
  def decode[T](ops: DynamicOps[T], input: T): DataResult[(Option[A], T)] = {
    val result = codec.decode(ops, input)
    DataResult.success(result.resultOrPartial(_ => ()).map(p => (Some(p._1), p._2)).getOrElse((None, input)))
  }

  def encode[T](input: Option[A], ops: DynamicOps[T], prefix: T): DataResult[T] = input match
    case Some(a) => codec.encode(a, ops, prefix)
    case None    => DataResult.success(prefix)

  override def toString: String = s"MaybeCodec[$codec]"
}

final class MaybeMapCodec[A](codec: MapCodec[A]) extends MapCodec[Option[A]] {
  def keys[T](ops: DynamicOps[T]): Iterable[T] = codec.keys(ops)

  def decode[T](ops: DynamicOps[T], input: MapLike[T]): DataResult[Option[A]] = {
    val decoded = codec.decode(ops, input)
    DataResult.success(decoded.resultOrPartial(_ => ()).flatMap(Some(_)))
  }
  
  def encode[T](input: Option[A], ops: DynamicOps[T], prefix: RecordBuilder[T]): RecordBuilder[T] = input match
    case Some(a) => codec.encode(a, ops, prefix)
    case None    => prefix

  override def toString: String = s"MaybeMapCodec[$codec]"
}
