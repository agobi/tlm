package io.github.agobi.tlm.model

import scala.reflect.ClassTag
import scala.util.Random

object RandomQueue {
  def empty[T: ClassTag]: RandomQueue[T]                 = new RandomQueue[T](Random, Array.ofDim(8), 0)
  def empty[T: ClassTag](random: Random): RandomQueue[T] = new RandomQueue[T](random, Array.ofDim(8), 0)
}

final class RandomQueue[T: ClassTag] private (random: Random, state: Array[T], size: Int) {

  def headOption: Option[T] = {
    if (size == 0) None
    else {
      Some(state(size - 1))
    }
  }

  def tail: RandomQueue[T] = {
    if (size == 0) throw new IllegalStateException("Cannot remove from empty RandomQueue")
    else new RandomQueue(random, state, size - 1)
  }

  def +(elem: T): RandomQueue[T] = {
    val newState = if (size == state.length) {
      val a = Array.ofDim[T](size * 3 / 2)
      Array.copy(state, 0, a, 0, size)
      a
    } else state

    val position = random.nextInt(size + 1)
    if (position == size) {
      newState(size) = elem
    } else {
      newState(size) = newState(position)
      newState(position) = elem
    }

    new RandomQueue[T](random, newState, size + 1)
  }

  def ++(elems: Iterable[T]): RandomQueue[T] = {
    elems.foldLeft(this)(_ + _)
  }
}
