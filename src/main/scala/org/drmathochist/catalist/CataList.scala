package org.drmathochist.catalist

import scala.collection.GenIterable

object CataList {
  val nil: CataList[Nothing] = new CataList[Nothing] {
    def cata[B](ifEmpty: => B, ifNot: (Nothing, B) => B): B = ifEmpty
  }

  def cons[A](h: A, t: CataList[A]): CataList[A] = new CataList[A] {
    def cata[B](ifEmpty: => B, ifNot: (A, B) => B): B = ifNot(h, t.cata(ifEmpty, ifNot))
  }

  def apply[A](as: A*): CataList[A] = as.foldRight[CataList[A]](nil)(cons)

  def unapply[A](arg: CataList[A]): Option[(A, CataList[A])] = arg.htDecomp

  def unapplySeq[A](arg: CataList[A]): Option[Seq[A]] = Some(arg.iterator.toSeq)
}

trait CataList[+A] {
  def cata[B](ifEmpty: => B, ifNot: (A, B) => B): B

  def ::[B >: A](b: B): CataList[B] = CataList.cons(b, this)
  def +:[B >: A](b: B): CataList[B] = CataList.cons(b, this)
  def :+[B >: A](b: B): CataList[B] = cata[CataList[B]](CataList(b), CataList.cons)

  def concat[B >: A](that: CataList[B]): CataList[B] = cata[CataList[B]](that, CataList.cons)
  def ++[B >: A](that: CataList[B]): CataList[B] = concat(that)
  def :::[B >: A](bs: CataList[B]): CataList[B] = bs.concat(this)

  def htDecomp: Option[(A, CataList[A])] = cata[Option[(A, CataList[A])]](None, (a, decomp) => {
    Some(a, decomp.map({case (h, t) => h :: t}).getOrElse(CataList.nil))
  })

  def headOption: Option[A] = this match {
    case CataList(h, _) => Some(h)
    case _ => None
  }

  def head: A = this match {
    case CataList(h, _) => h
    case _ => throw new NoSuchElementException("Empty CataList!")
  }

  def tail: CataList[A] = this match {
    case CataList(_, t) => t
    case _ => throw new UnsupportedOperationException("Empty CataList!")
  }

  def isEmpty: Boolean = cata[Boolean](true, (a, b) => false)

  def map[B](f: A => B): CataList[B] = cata[CataList[B]](CataList.nil, (a, bs) => f(a) :: bs)

  def flatMap[B](f: A => CataList[B]): CataList[B] = cata[CataList[B]](CataList.nil, (a, bs) => f(a) ::: bs)
  def flatten[B](implicit ev: A <:< CataList[B]): CataList[B] = flatMap[B](ev)

  def filter(p: A => Boolean): CataList[A] = cata[CataList[A]](CataList.nil, (a, as) => if (p(a)) a :: as else as)
  def filterNot(p: A => Boolean): CataList[A] = filter(a => !p(a))

  def foldRight[B](z: => B)(op: (A, B) => B): B = cata(z, op)
  def :\[B](z: => B)(op: (A, B) => B): B = foldRight(z)(op)
  def reduceRight[B >: A](op: (A, B) => B): B = this match {
    case CataList(h, t) => t.foldRight[B](h)(op)
    case _ => throw new UnsupportedOperationException("Empty CataList!")
  }

  def foldLeft[B](z: => B)(op: (B, A) => B): B = cata[B => B](identity, (a, f) => b => f(op(b, a)))(z)
  def /:[B](z: => B)(op: (B, A) => B): B = foldLeft(z)(op)
  def reduceLeft[B >: A](op: (B, A) => B): B = this match {
    case CataList(h, t) => t.foldLeft[B](h)(op)
    case _ => throw new UnsupportedOperationException("Empty CataList!")
  }

  def foreach(f: A => Unit): Unit = foldLeft[Unit](())((u, a) => f(a))

  def iterator: Iterator[A] = cata[Iterator[A]](Iterator.empty, (a, rest) => Iterator.single(a) ++ rest)

  def mkString: String = mkString("", "", "")
  def mkString(sep: String): String = mkString("", sep, "")
  def mkString(start: String, sep: String, end: String): String = addString(new StringBuilder, start, sep, end).toString()
  def addString(sb: StringBuilder): StringBuilder = addString(sb, "", "", "")
  def addString(sb: StringBuilder, sep: String): StringBuilder = addString(sb, "", sep, "")
  def addString(sb: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
    sb.append(start)
    this match {
      case CataList(h, t) =>
        sb.append(h)
        t.foreach(a => sb.append(s"$sep$a"))
      case _ =>
    }
    sb.append(end)
  }

  override def toString: String = addString(new StringBuilder, "CataList(", ", ", ")").toString()

  def take(n: Int): CataList[A] = splitAt(n)._1
  def drop(n: Int): CataList[A] = splitAt(n)._2
  def splitAt(n: Int): (CataList[A], CataList[A]) =
    this match {
      case CataList(h, t) if n > 0 =>
        val (tstart, tend) = t.splitAt(n-1)
        (h :: tstart, tend)
      case CataList(h, t) => (CataList.nil, this)
      case _ => (CataList.nil, CataList.nil)
    }

  def apply(n: Int): A = {
    if (n < 0) {
      throw new IndexOutOfBoundsException(s"$n < 0")
    }
    drop(n).headOption.getOrElse(throw new IndexOutOfBoundsException(s"$n > length"))
  }

  def count(p: A => Boolean): Int = cata[Int](0, (a, ct) => if (p(a)) ct + 1 else ct)

  def takeWhile(p: A => Boolean): CataList[A] = span(p)._1
  def dropWhile(p: A => Boolean): CataList[A] = span(p)._2
  def span(p: A => Boolean): (CataList[A], CataList[A]) =
    this match {
      case CataList(h, t) if p(h) =>
        val (tstart, tend) = t span p
        (h :: tstart, tend)
      case CataList(h, t) => (CataList.nil, this)
      case _ => (CataList.nil, CataList.nil)
    }

  def contains[B >: A](b: B): Boolean = exists(_==b)
  def exists(p: A => Boolean): Boolean = cata[Boolean](false, (a, exists) => exists || p(a))
  def forall(p: A => Boolean): Boolean = cata[Boolean](true, (a, forall) => forall && p(a))

  def indexOf[B >: A](b: B): Int = indexOf(b, 0)
  def indexOf[B >: A](b: B, from: Int): Int = indexWhere(_==b, from)
  def indexWhere(p: A => Boolean): Int = indexWhere(p, 0)
  def indexWhere(p: A => Boolean, from: Int): Int = zipWithIndex.find({case (a, idx) => p(a)}).map(_._2).getOrElse(-1)
  def find(p: A => Boolean): Option[A] = find(p, 0)
  def find(p: A => Boolean, from: Int): Option[A] = this match {
    case CataList(_, t) if from > 0 => t.find(p, from - 1)
    case CataList(h, t)             => if (p(h)) Some(h) else t.find(p, from)
    case _                          => None
  }

  def unzip[B, C](implicit ev: A <:< (B, C)): (CataList[B], CataList[C]) =
    cata[(CataList[B], CataList[C])]((CataList.nil, CataList.nil), (a, lists) => {
      val (b, c) = ev(a)
      val (bs, cs) = lists
      (b::bs, c::cs)
    })

  def zip[B](that: CataList[B]): CataList[(A, B)] = zipWith(that) { case (a, b) => (a, b) }
  def zipWith[B, C](that: CataList[B])(f: (A, B) => C): CataList[C] = (this, that) match {
    case (CataList.nil, _)                  => CataList.nil
    case (_, CataList.nil)                  => CataList.nil
    case (CataList(a, as), CataList(b, bs)) => f(a, b) :: (as zipWith bs)(f)
  }
  def zip[B](that: GenIterable[B]): CataList[(A, B)] = zipWith(that) { case (a, b) => (a, b) }
  def zipWith[B, C](that: GenIterable[B])(f: (A, B) => C): CataList[C] = this match {
    case CataList.nil      => CataList.nil
    case _ if that.isEmpty => CataList.nil
    case CataList(a, as)   =>
      val (b, bs) = that.splitAt(1)
      f(a, b.head) :: (as zipWith bs)(f)
  }
  def zipWithIndex: CataList[(A, Int)] = zip(Stream from 0)

  def max[B >: A](implicit cmp: Ordering[B], ev: A <:< B): A = maxBy(ev)
  def maxBy[B >: A](f: A => B)(implicit cmp: Ordering[B]): A =
    map(a => (a, f(a))).reduceLeft((aWVal, maxWVal) => {
      val ((a, aVal), (max, maxVal)) = (aWVal, maxWVal)
      if (cmp.gteq(aVal, maxVal)) (a, aVal) else (max, maxVal)
    })._1

  def min[B >: A](implicit cmp: Ordering[B], ev: A <:< B): A = minBy(ev)
  def minBy[B >: A](f: A => B)(implicit cmp: Ordering[B]): A =
    map(a => (a, f(a))).reduceLeft((aWVal, maxWVal) => {
      val ((a, aVal), (max, maxVal)) = (aWVal, maxWVal)
      if (cmp.lteq(aVal, maxVal)) (a, aVal) else (max, maxVal)
    })._1

  def sum[B >: A](implicit num: Numeric[B], ev: A <:< B): B =
    foldLeft[B](num.zero)((acc, a) => num.plus(ev(a), acc))

  def product[B >: A](implicit num: Numeric[B], ev: A <:< B): B =
    foldLeft[B](num.one)((acc, a) => num.times(ev(a), acc))
}
