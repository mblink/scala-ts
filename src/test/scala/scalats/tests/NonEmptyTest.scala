package scalats
package tests

import cats.data.{NonEmptyChain, NonEmptyList, NonEmptyVector}
import io.circe.derivation.{ConfiguredDecoder, ConfiguredEncoder}
import org.scalacheck.{Arbitrary, Gen}
import scalats.tests.arbitrary.*

object NonEmptyTest {
  private def genNonEmpty[F[_]](f: (Int, List[Int]) => F[Int]): Arbitrary[F[Int]] =
    Arbitrary(Gen.zip(Arbitrary.arbitrary[Int], Gen.listOf(Arbitrary.arbitrary[Int])).map(f.tupled))

  given arbNonEmptyChain: Arbitrary[NonEmptyChain[Int]] = genNonEmpty((h, t) => NonEmptyChain.fromNonEmptyList(NonEmptyList(h, t)))
  given arbNonEmptyList: Arbitrary[NonEmptyList[Int]] = genNonEmpty(NonEmptyList(_, _))
  given arbNonEmptyVector: Arbitrary[NonEmptyVector[Int]] = genNonEmpty((h, t) => NonEmptyVector(h, t.toVector))

  case class Foo(
    chain: NonEmptyChain[Int],
    list: NonEmptyList[Int],
    vector: NonEmptyVector[Int],
  ) derives Arbitrary, ConfiguredDecoder, ConfiguredEncoder

  case object Bar {
    val chain = NonEmptyChain(1, 2)
    val list = NonEmptyList.of(1, 2)
    val vector = NonEmptyVector.of(1, 2)
  }

  val expectedFooCode = """
import { ReadonlyNonEmptyArrayC, readonlyNonEmptyArray } from "io-ts-types/lib/readonlyNonEmptyArray";
import * as t from "io-ts";
import * as RNEA from "fp-ts/lib/ReadonlyNonEmptyArray";

export type FooC = t.TypeC<{
  chain: ReadonlyNonEmptyArrayC<t.NumberC>,
  list: ReadonlyNonEmptyArrayC<t.NumberC>,
  vector: ReadonlyNonEmptyArrayC<t.NumberC>
}>;
export type Foo = {
  chain: RNEA.ReadonlyNonEmptyArray<number>,
  list: RNEA.ReadonlyNonEmptyArray<number>,
  vector: RNEA.ReadonlyNonEmptyArray<number>
};
export const fooC: FooC = t.type({
  chain: readonlyNonEmptyArray(t.number),
  list: readonlyNonEmptyArray(t.number),
  vector: readonlyNonEmptyArray(t.number)
}) satisfies t.Type<Foo, unknown>;
""".trim

  val expectedBarCode = """
import * as t from "io-ts";

export const bar = {
  _tag: `Bar`,
  chain: [1, 2],
  list: [1, 2],
  vector: [1, 2]
} as const;

export type BarC = t.TypeC<{
  _tag: t.LiteralC<`Bar`>,
  chain: t.LiteralC<[1, 2]>,
  list: t.LiteralC<[1, 2]>,
  vector: t.LiteralC<[1, 2]>
}>;
export type Bar = {
  _tag: `Bar`,
  chain: [1, 2],
  list: [1, 2],
  vector: [1, 2]
};
export const barC: BarC = t.type({
  _tag: t.literal(`Bar`),
  chain: t.literal([1, 2]),
  list: t.literal([1, 2]),
  vector: t.literal([1, 2])
}) satisfies t.Type<Bar, unknown>;
""".trim

  val fooFile = "foo.ts"
  val barFile = "bar.ts"

  val types = Map(
    fooFile -> (List(parse[Foo]), expectedFooCode),
    barFile -> (List(parse[Bar.type]), expectedBarCode),
  )
}

class NonEmptyTest extends CodecTest[NonEmptyTest.Foo](
  outputDir / "nonEmpty",
  NonEmptyTest.types,
  "fooC",
  "fooC",
  NonEmptyTest.fooFile,
)
