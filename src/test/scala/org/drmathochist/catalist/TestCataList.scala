package org.drmathochist.catalist

import org.scalacheck.{Arbitrary, Gen, Properties}
import org.scalatest.FreeSpec

object CataListSpecification extends Properties("CataList") {
  import org.scalacheck.Prop.{forAll, BooleanOperators}

  implicit def arbCataList[A](implicit arbA: Arbitrary[A]): Arbitrary[CataList[A]] =
    Arbitrary( for { ints <- Gen.listOf(arbA.arbitrary) } yield CataList(ints:_*) )

  // Constructors give empty and non-empty lists as expected
  property("empty constructor") = CataList.nil.isEmpty

  property("non-empty constructor") = forAll { (head: Int, tail: CataList[Int]) =>
    !CataList.cons(head, tail).isEmpty
  }

  // The fundamental properties of lists, relating head, tail, and cons
  property("assembly") = forAll { cl: CataList[Int] =>
    (!cl.isEmpty) ==> (CataList.cons(cl.head, cl.tail) == cl)
  }

  property("disassembly") = forAll { (head: Int, tail: CataList[Int]) =>
    val cl = CataList.cons(head, tail)
    cl.head == head && cl.tail == tail
  }

  // The pattern-matching extractor works as expected
  property("pattern matching") = forAll { (head: Int, tail: CataList[Int]) =>
    CataList.cons(head, tail) match {
      case CataList(h, t) => h == head && t == tail
    }
  }

  // The maps in each direction between List[A] and CataList[A] are inverses
  property("isomorphism List -> CataList") = forAll { l: List[Int] =>
    CataList(l:_*).toList == l
  }

  property("isomorphism CataList -> List") = forAll { cl: CataList[Int] =>
    CataList(cl.toList:_*) == cl
  }

  // Most of these properties use the isomorphism property from above.
  // If List and CataList are isomorphic, then the operations on CataList
  // should "do the same thing" as on List.
  property("concatenation") = forAll { (cl1: CataList[Int], cl2: CataList[Int]) =>
    (cl1 ::: cl2).toList == cl1.toList ::: cl2.toList
  }

  property("length") = forAll { cl: CataList[Int] =>
    cl.length == cl.toList.length
  }

  property("startsWith") = forAll { (cl1: CataList[Int], cl2: CataList[Int]) =>
    (cl1 startsWith cl2) == (cl1.toList startsWith cl2.toList)
  }


  property("map") = forAll { cl: CataList[Int] =>
    cl.map(_.toString).toList == cl.toList.map(_.toString)
  }

  property("flatten") = forAll { cll: CataList[CataList[Int]] =>
    cll.flatten.toList == cll.toList.flatMap(_.toList)
  }

  // Does flatMap really behave like map-then-flatten?
  property("flatmap") = forAll { cl: CataList[Int] =>
    val triple = (n: Int) => CataList(n, n, n)
    cl.flatMap(triple) == cl.map(triple).flatten
  }

  property("foldLeft") = forAll { cl: CataList[String] =>
    // starting with a fixed 'X' allows us to distinguish left and right folds
    cl.foldLeft("X")(_+_) == cl.toList.foldLeft("X")(_+_)
  }

  property("foldRight") = forAll { cl: CataList[String] =>
    cl.foldRight("X")(_+_) == cl.toList.foldRight("X")(_+_)
  }

  property("mkString") = forAll { cl: CataList[String] =>
    cl.mkString == cl.toList.mkString
  }

  // The relationship between splitAt and take/drop
  property("splitAt") = forAll { (cl: CataList[Int], n: Int) =>
    cl.splitAt(n) == (cl.take(n), cl.drop(n))
  }

  // The relationship between span and takeWhile/dropWhile
  property("span") = forAll { cl: CataList[Int] =>
    val p = (n: Int) => n < 0
    cl.span(p) == (cl.takeWhile(p), cl.dropWhile(p))
  }

  property("contains") = forAll { (cl: CataList[Int], n: Int) =>
    cl.contains(n) == cl.toList.contains(n)
  }

  // zipping undoes unzipping
  property("unzip-zip") = forAll { cl: CataList[(Int, Int)] =>
    val (as, bs) = cl.unzip
    (as zip bs) == cl
  }

  // unzipping ALMOST undoes zipping, but might lose information from the longer list
  property("zip-unzip") = forAll { (cl1: CataList[Int], cl2: CataList[Int]) =>
    val (as, bs) = (cl1 zip cl2).unzip
    as.length == bs.length &&
    as.length == math.min(cl1.length, cl2.length) &&
    (cl1 startsWith as) &&
    (cl2 startsWith bs)
  }
}

class TestCataList extends FreeSpec {
  CataListSpecification.check()
}
