package com.morphismmc.serialization

enum LifeCycle {
  case Stable, Experimental
  case Deprecated(since: Int)

  def +(other: LifeCycle): LifeCycle = (this, other) match
    case (Experimental, _) | (_, Experimental) => Experimental
    case (_: Deprecated, _: Deprecated)        =>
      val d1 = this.asInstanceOf[Deprecated]
      val d2 = other.asInstanceOf[Deprecated]
      if d2.since < d1.since then other else this
    case (_: Deprecated, _)                    => this
    case (_, _: Deprecated)                    => other
    case _                                     => Stable
}