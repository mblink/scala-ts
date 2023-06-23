package scalats
package tests

import org.scalacheck.{Arbitrary, Gen}
import scala.compiletime.erasedValue
import scala.deriving.Mirror

object arbitrary {
  inline given derivedArbitrary[A](using A: Mirror.Of[A]): Arbitrary[A] = {
    val insts = summonInsts[A.MirroredElemTypes]

    inline A match {
      case m: Mirror.ProductOf[A] => Arbitrary(insts.foldRight(Gen.const[Tuple](EmptyTuple))((arb, acc) =>
        arb.arbitrary.flatMap(h => acc.map(h *: _)),
      ).map(t => m.fromTuple(t.asInstanceOf[m.MirroredElemTypes])))

    case _: Mirror.SumOf[A] =>
      Arbitrary((insts match {
        case a :: b :: rest => Gen.oneOf(a.arbitrary, b.arbitrary, rest.map(_.arbitrary): _*)
        case a :: Nil => a.arbitrary
        case Nil => Gen.const(null)
      }).map(_.asInstanceOf[A]))
    }
  }

  private inline def summonInst[A]: Arbitrary[A] =
    compiletime.summonFrom { case a: Arbitrary[A] => a }

  private inline def summonInsts[T <: Tuple]: List[Arbitrary[Any]] =
    inline erasedValue[T] match {
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInst[t].asInstanceOf[Arbitrary[Any]] :: summonInsts[ts]
    }
}
