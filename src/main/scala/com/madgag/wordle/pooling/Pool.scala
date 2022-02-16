package com.madgag.wordle.pooling

import java.util.concurrent.ConcurrentMap
import java.util.concurrent.atomic.AtomicInteger

trait Pool[P,T <: Pooled] {
  private val counter = new AtomicInteger()

  val itemsByParams: ConcurrentMap[P,T] = new java.util.concurrent.ConcurrentHashMap()

  def stored: Int = counter.intValue

  def intern(p: P): T =
    itemsByParams.computeIfAbsent(p, { _ => create(counter.getAndIncrement(), p) })

  protected def create(id: Int, p: P): T

}

trait Pooled(id: Int) {
  override def hashCode(): Int = id
}
