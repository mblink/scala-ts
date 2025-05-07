package scalats
package tests

import io.circe.{Decoder, Encoder}
import org.scalacheck.Arbitrary
import scalats.tests.arbitrary.*

object UnionWithTypeParamTest {
  sealed trait Foo[A] derives Arbitrary {
    val int: Int
    val data: A
  }
  object Foo {
    given decoder[A: Decoder]: Decoder[Foo[A]] = Decoder.derivedConfigured
    given encoder[A: Encoder]: Encoder[Foo[A]] = Encoder.AsObject.derivedConfigured
  }
  case object Bar extends Foo[String] {
    val int = 1
    val data = "bar"
  }
  case class Baz[A](int: Int, data: A) extends Foo[A]

  val expectedFooCode = """
import { pipe } from "fp-ts/lib/function";
import * as E from "fp-ts/lib/Either";
import * as t from "io-ts";

export const bar = {
  _tag: `Bar`,
  data: `bar`,
  int: 1
} as const;

export type BarTaggedC = t.TypeC<{
  _tag: t.LiteralC<`Bar`>
}>;
export const barTaggedC: BarTaggedC = t.type({
  _tag: t.literal(`Bar`)
});
export type BarTagged = t.TypeOf<BarTaggedC>;
export type Bar = BarTagged & typeof bar;
export type BarC = t.Type<Bar, BarTagged>;
export const barC: BarC = pipe(barTaggedC, c => new t.Type<Bar, BarTagged>(
  `Bar`,
  (u: unknown): u is Bar => E.isRight(c.decode(u)),
  (u: unknown): E.Either<t.Errors, Bar> => pipe(c.decode(u), E.map(x => ({ ...x, ...bar }))),
  (x: Bar): BarTagged => ({ ...x, _tag: `Bar`}),
)) satisfies t.Type<Bar, unknown>;


export type BazC<A1 extends t.Mixed> = t.TypeC<{
  _tag: t.LiteralC<`Baz`>,
  int: t.NumberC,
  data: A1
}>;
export type Baz<A1> = {
  _tag: `Baz`,
  int: number,
  data: A1
};
export const bazC = <A1 extends t.Mixed>(A1: A1): BazC<A1> => t.type({
  _tag: t.literal(`Baz`),
  int: t.number,
  data: A1
}) satisfies t.Type<Baz<t.TypeOf<A1>>, unknown>;


export const allFooC = <A1 extends t.Mixed>(A1: A1) => [barC, bazC(A1)] as const;
export const allFooNames = [`Bar`, `Baz`] as const;
export type FooName = (typeof allFooNames)[number];

export type FooCU<A1 extends t.Mixed> = t.UnionC<[BarC, BazC<A1>]>;
export type FooU<A1> = Bar | Baz<A1>;
export const FooCU = <A1 extends t.Mixed>(A1: A1): FooCU<A1> => t.union([barC, bazC(A1)]) satisfies t.Type<FooU<t.TypeOf<A1>>, unknown>;
""".trim

  val fooFile = "foo.ts"

  val types = Map(
    fooFile -> (List(parse[Foo[?]]), expectedFooCode),
  )
}

class UnionWithTypeParamTest extends CodecTest[UnionWithTypeParamTest.Foo[Int]](
  outputDir / "union-type-param",
  UnionWithTypeParamTest.types,
  "FooCU",
  "FooCU(t.number)",
  UnionWithTypeParamTest.fooFile,
)
