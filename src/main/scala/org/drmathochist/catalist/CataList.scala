package org.drmathochist.catalist

object CataList {
  def nil: CataList[Nothing] = new CataList[Nothing] {
    override def cata[B](ifEmpty: B, ifNot: (Nothing, B) => B): B = ifEmpty
  }

  def cons[A](head: A, tail: CataList[A]): CataList[A] = new CataList[A] {
    override def cata[B](ifEmpty: B, ifNot: (A, B) => B): B = ifNot(head, tail.cata(ifEmpty, ifNot))
  }
}

trait CataList[+A] {
  def cata[B](ifEmpty: B, ifNot: (A, B) => B): B

  def iterator: Iterator[A] = cata(Iterator.empty, (a, rest: Iterator[A]) => Iterator.single(a) ++ rest)

  override def toString: String = iterator.addString(new StringBuilder, "CataList(", ", ", ")").toString()

  def map[B](f: A => B): CataList[B] = new CataList[B] {
    override def cata[C](ifEmpty: C, ifNot: (B, C) => C): C = CataList.this.cata(ifEmpty, (a, c) => ifNot(f(a), c))
  }


}
