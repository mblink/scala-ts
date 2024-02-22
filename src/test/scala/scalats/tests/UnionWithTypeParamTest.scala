package scalats
package tests

import io.circe.{Decoder, Encoder}
import scalats.tests.arbitrary.given

object UnionWithTypeParamTest {
  sealed trait Foo[A] {
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

export const barTaggedC = t.type({
  _tag: t.literal(`Bar`)
});
export type BarTaggedC = typeof barTaggedC;
export type BarTagged = t.TypeOf<BarTaggedC>;
export type Bar = BarTagged & typeof bar;
export const barC = pipe(barTaggedC, c => new t.Type<Bar, BarTagged>(
  `Bar`,
  (u: unknown): u is Bar => E.isRight(c.decode(u)),
  (u: unknown): E.Either<t.Errors, Bar> => pipe(c.decode(u), E.map(x => ({ ...x, ...bar }))),
  (x: Bar): BarTagged => ({ ...x, _tag: `Bar`}),
));
export type BarC = typeof barC;


export class bazCC<A1 extends t.Mixed>{ codec = (A1: A1) => t.type({
  _tag: t.literal(`Baz`),
  int: t.number,
  data: A1
})}
export const bazC = <A1 extends t.Mixed>(A1: A1) => new bazCC<A1>().codec(A1);
export type BazC<A1 extends t.Mixed> = ReturnType<bazCC<A1>["codec"]>;
export type Baz<A1> = t.TypeOf<BazC<t.Type<A1>>>;

export const allFooC = <A1 extends t.Mixed>(A1: A1) => [barC, bazC(A1)] as const;
export const allFooNames = [`Bar`, `Baz`] as const;
export type FooName = (typeof allFooNames)[number];

export class FooCUC<A1 extends t.Mixed>{ codec = (A1: A1) => t.union([barC, bazC(A1)])}
export const FooCU = <A1 extends t.Mixed>(A1: A1) => new FooCUC<A1>().codec(A1);
export type FooCU<A1 extends t.Mixed> = ReturnType<FooCUC<A1>["codec"]>;
export type FooU<A1> = t.TypeOf<FooCU<t.Type<A1>>>;
""".trim

  val fooFile = "foo.ts"

  val types = Map(
    fooFile -> (List(parse[Foo[_]]), expectedFooCode),
  )
}

class UnionWithTypeParamTest extends CodecTest[UnionWithTypeParamTest.Foo[Int]](
  outputDir / "union-type-param",
  UnionWithTypeParamTest.types,
  "FooCU",
  "FooCU(t.number)",
  UnionWithTypeParamTest.fooFile,
)
