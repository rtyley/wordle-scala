package com.madgag.wordle.evidence

type FrequencyMap[T] = Map[T, Int]

extension [T](freqMap: FrequencyMap[T])
  def increment(t: T): FrequencyMap[T] = freqMap.updated(t, freqMap(t) + 1)
  def incrementIf(cond: Boolean)(t: T): FrequencyMap[T] = if (cond) increment(t) else freqMap

object FrequencyMap {
  def empty[T]: FrequencyMap[T] = Map.empty[T, Int].withDefaultValue(0)
}