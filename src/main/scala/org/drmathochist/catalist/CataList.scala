package org.drmathochist.catalist

object CataList {
  def nil: CataList[Nothing] = new CataList[Nothing] {
    def cata[B](ifEmpty: => B, ifNot: (Nothing, B) => B): B = ifEmpty
  }

  def cons[A](h: A, t: CataList[A]): CataList[A] = new CataList[A] {
    def cata[B](ifEmpty: => B, ifNot: (A, B) => B): B = ifNot(h, t.cata(ifEmpty, ifNot))
  }

  def apply[A](as: A*): CataList[A] = as.foldRight[CataList[A]](nil)(cons)
}

trait CataList[+A] {
  def cata[B](ifEmpty: => B, ifNot: (A, B) => B): B

  def iterator: Iterator[A] = cata[Iterator[A]](Iterator.empty, (a, rest) => Iterator.single(a) ++ rest)

  override def toString: String = iterator.addString(new StringBuilder, "CataList(", ", ", ")").toString()

  def map[B](f: A => B): CataList[B] = new CataList[B] {
    def cata[C](ifEmpty: => C, ifNot: (B, C) => C): C =
      CataList.this.cata(ifEmpty, (a, c) => {
        ifNot(f(a), c)
      })
  }

  def flatMap[B](f: A => CataList[B]): CataList[B] = new CataList[B] {
    def cata[C](ifEmpty: => C, ifNot: (B, C) => C): C =
      CataList.this.cata[C](ifEmpty, (a, c) => {
        f(a).cata(c, ifNot)
      })
  }

  def filter(p: A => Boolean): CataList[A] = new CataList[A] {
    def cata[B](ifEmpty: => B, ifNot: (A, B) => B): B =
      CataList.this.cata[B](ifEmpty, (a, b) => {
        if (p(a)) ifNot(a, b) else b
      })
  }

  def foreach(f: A => Unit): Unit = cata[Unit]((), (a, u) => f(a))

  def isEmpty: Boolean = cata[Boolean](true, (a, b) => false)

  def headOption: Option[A] = cata[Option[A]](None, (a, b) => Some(a))

  def head: A = headOption.getOrElse(throw new NoSuchElementException("Empty CataList!"))

  def tail: CataList[A] = {
    cata[Option[(A, CataList[A])]](None, (a, o) => {
      Some(a, o.map(pair => {
        val (h, t) = pair; CataList.cons(h, t)
      }).getOrElse(CataList.nil))
    }).getOrElse(throw new UnsupportedOperationException("Empty CataList!"))._2
  }

  def foldRight[B](z: => B)(op: (A, B) => B): B = cata(z, op)

  def foldLeft[B](z: => B)(op: (B, A) => B): B = cata[B => B](identity, (a, f) => b => f(op(b, a)))(z)


}
