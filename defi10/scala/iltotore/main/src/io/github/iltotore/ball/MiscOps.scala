package io.github.iltotore.ball

import doodle.core.Vec

trait MiscOps {

  extension [A](iterable: Iterable[A]) {

    def mapIf[B >: A](cond: A => Boolean)(mapper: A => B): Iterable[B] =
      iterable.map((a: A) => if(cond(a)) mapper(a) else a)
  }
}
