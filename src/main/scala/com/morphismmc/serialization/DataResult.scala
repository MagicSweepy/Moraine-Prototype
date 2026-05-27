package com.morphismmc.serialization

import com.morphismmc.kind.Monad

enum DataResult[+R] {
  case Success(value: R, lifeCycle: LifeCycle)
  case Error(message: () => String, partialValue: Option[R], lifeCycle: LifeCycle)

  // region Life Cycle
  
  def addLifeCycle(lc: LifeCycle): DataResult[R] = this match
    case Success(v, lc0)     => Success(v, lc0 + lc)
    case Error(msg, pv, lc0) => Error(msg, pv, lc0 + lc)

  def setLifeCycle(lc: LifeCycle): DataResult[R] = this match
    case s: Success[R] if s.lifeCycle == lc => s
    case Success(v, _)                      => Success(v, lc)
    case e: Error[R] if e.lifeCycle == lc   => e
    case Error(msg, pv, _)                  => Error(msg, pv, lc)
    
  // endregion
    
  // region Result Accessor

  def result: Option[R] = this match
    case Success(v, _) => Some(v)
    case _             => None

  def error: Option[Error[R]] = this match
    case e: Error[R] => Some(e)
    case _           => None

  def hasResultOrPartial: Boolean = this match
    case Success(_, _)   => true
    case Error(_, pv, _) => pv.isDefined

  def resultOrPartial: Option[R] = this match
    case Success(v, _)   => Some(v)
    case Error(_, pv, _) => pv

  def resultOrPartial(onError: String => Unit): Option[R] = this match
    case Success(v, _)     => Some(v)
    case Error(msg, pv, _) => onError(msg()); pv

  def getOrThrow[E <: Throwable](f: String => E): R = this match
    case Success(v, _)    => v
    case Error(msg, _, _) => throw f(msg())

  def getOrThrow: R = getOrThrow(msg => IllegalStateException(msg))

  def getPartialOrThrow[E <: Throwable](f: String => E): R = this match
    case Success(v, _)          => v
    case Error(_, Some(v), _)   => v
    case Error(msg, None, _)    => throw f(msg())

  def getPartialOrThrow: R = getPartialOrThrow(msg => IllegalStateException(msg))

  def setPartial[B >: R](partial: => B): DataResult[B] = this match
    case _: Success[R]     => this
    case Error(msg, _, lc) => Error(msg, Some(partial), lc)

  def promotePartial(onError: String => Unit): DataResult[R] = this match
    case _: Success[R]           => this
    case Error(msg, Some(v), lc) =>
      onError(msg())
      Success(v, lc)
    case e: Error[R]             => e
  
  // endregion

  // region Transformer
  
  def map[B](f: R => B): DataResult[B] = this match
    case Success(v, lc)     => Success(f(v), lc)
    case Error(msg, pv, lc) => Error(msg, pv.map(f), lc)

  def mapOrElse[B](onSuccess: R => B, onError: Error[R] => B): B = this match
    case Success(v, _) => onSuccess(v)
    case e: Error[R]   => onError(e)

  def mapError(f: String => String): DataResult[R] = this match
    case _: Success[R]      => this
    case Error(msg, pv, lc) => Error(() => f(msg()), pv, lc)

  def flatMap[B](f: R => DataResult[B]): DataResult[B] = this match
    case Success(v, lc)          => f(v).addLifeCycle(lc)
    case Error(msg, Some(v), lc) => f(v) match
      case Success(b, lc2)         => Error(msg, Some(b), lc + lc2)
      case Error(msg2, pb, lc2)    => Error(() => msg() + "; " + msg2(), pb, lc + lc2)
    case e: Error[R]             => e.asInstanceOf[DataResult[B]]

  def ap[B](ff: DataResult[R => B]): DataResult[B] = (this, ff) match
    case (Success(a, lc1), Success(f, lc2))           => Success(f(a), lc1 + lc2)
    case (Success(a, lc1), Error(msg, pf, lc2))       => Error(msg, pf.map(_(a)), lc1 + lc2)
    case (Error(msg, pa, lc1), Success(f, lc2))       => Error(msg, pa.map(f), lc1 + lc2)
    case (Error(msg1, pa, lc1), Error(msg2, pf, lc2)) => Error(() => msg1() + "; " + msg2(), pa.flatMap(a => pf.map(f => f(a))), lc1 + lc2)

  def apply2[T2, S](f: (R, T2) => S, fa: DataResult[T2]): DataResult[S] =
    (this, fa) match
      case (Success(a, lc1), Success(b, lc2)) => Success(f(a, b), lc1 + lc2)
      case _                                  => summon[Monad[DataResult]].ap2(summon[Monad[DataResult]].pure(f))(this, fa)

  def apply2stable[T2, S](f: (R, T2) => S, fa: DataResult[T2]): DataResult[S] =
    val ff = summon[Monad[DataResult]].pure(f).setLifeCycle(LifeCycle.Stable)
    summon[Monad[DataResult]].ap2(ff)(this, fa)
  
  // endregion

  // region Predicate

  def ifSuccess(f: R => Unit): DataResult[R] =
    this match
      case Success(v, _) => f(v)
      case _             => ()
    this

  def ifError(f: Error[R] => Unit): DataResult[R] =
    this match
      case e: Error[R] => f(e)
      case _           => ()
    this

  def isSuccess: Boolean = this match
    case Success(_, _) => true
    case _             => false

  inline def isError: Boolean = !isSuccess
  
  // endregion
}

object DataResult {

  def success[R](value: R, lifeCycle: LifeCycle = LifeCycle.Experimental): DataResult[R] = Success(value, lifeCycle)

  def error[R](message: () => String, partialValue: Option[R] = None, lifeCycle: LifeCycle = LifeCycle.Experimental): DataResult[R] 
    = Error(message, partialValue, lifeCycle)

  def partialGet[K, V](partialGetFn: K => V, errorPrefix: () => String): K => DataResult[V] 
    = name => Option(partialGetFn(name)).map(v => success(v)).getOrElse(error(() => errorPrefix() + name))

  def fixedSize[R](list: List[R], expectedSize: Int): DataResult[List[R]] = {
    if (list.size != expectedSize) {
      if (list.size >= expectedSize) {
        error(() => s"Input is not a list of $expectedSize elements", Some(list.take(expectedSize)))
      } else {
        error(() => s"Input is not a list of $expectedSize elements")
      }
    } else {
      success(list)
    }
  }

  def fixedSizeInts(ints: Array[Int], expectedSize: Int): DataResult[Array[Int]] = {
    if (ints.length != expectedSize) {
      val msg = () => s"Input is not a list of $expectedSize ints"
      if (ints.length >= expectedSize) {
        error(msg, Some(ints.take(expectedSize)))
      } else {
        error(msg)
      }
    }
    else {
      success(ints)
    }
  }

  def fixedSizeLongs(longs: Array[Long], expectedSize: Int): DataResult[Array[Long]] = {
    if (longs.length != expectedSize) {
      val msg = () => s"Input is not a list of $expectedSize longs"
      if (longs.length >= expectedSize) {
        error(msg, Some(longs.take(expectedSize)))
      } else {
        error(msg)
      }
    }
    else {
      success(longs)
    }
  }

  given Monad[DataResult] with
    def pure[A](a: A): DataResult[A] = success(a)

    def flatMap[A, B](fa: DataResult[A])(f: A => DataResult[B]): DataResult[B] = fa.flatMap(f)

    def ap[A, B](ff: DataResult[A => B])(fa: DataResult[A]): DataResult[B] = fa.ap(ff) 
}