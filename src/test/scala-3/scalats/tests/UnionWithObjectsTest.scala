package scalats
package tests

import io.circe.{Decoder, Encoder}
import scalats.tests.arbitrary.given

object UnionWithObjectsTest {
  sealed trait Foo {
    val int: Int
    val str: String
  }
  object Foo {
    given decoder: Decoder[Foo] = Decoder.derivedConfigured
    given encoder: Encoder[Foo] = Encoder.AsObject.derivedConfigured
  }
  case object Bar extends Foo {
    val int = 1
    val str = "bar"
  }
  case object Baz extends Foo {
    val int = 2
    val str = "baz"
  }

  val expectedFooCode = """
import * as t from "io-ts";
import { Ord as stringOrd } from "fp-ts/lib/string";
import * as E from "fp-ts/lib/Either";
import { pipe } from "fp-ts/lib/function";
import * as Ord from "fp-ts/lib/Ord";

export const bar = {
  _tag: `Bar`,
  int: 1,
  str: `bar`
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


export const baz = {
  _tag: `Baz`,
  int: 2,
  str: `baz`
} as const;

export const bazTaggedC = t.type({
  _tag: t.literal(`Baz`)
});
export type BazTaggedC = typeof bazTaggedC;
export type BazTagged = t.TypeOf<BazTaggedC>;
export type Baz = BazTagged & typeof baz;
export const bazC = pipe(bazTaggedC, c => new t.Type<Baz, BazTagged>(
  `Baz`,
  (u: unknown): u is Baz => E.isRight(c.decode(u)),
  (u: unknown): E.Either<t.Errors, Baz> => pipe(c.decode(u), E.map(x => ({ ...x, ...baz }))),
  (x: Baz): BazTagged => ({ ...x, _tag: `Baz`}),
));
export type BazC = typeof bazC;


export const allFooC = [barC, bazC] as const;
export const allFooNames = [`Bar`, `Baz`] as const;
export type FooName = (typeof allFooNames)[number];

export const FooCU = t.union([barC, bazC]);
export type FooCU = typeof FooCU;
export type FooU = t.TypeOf<FooCU>;

export const fooOrd: Ord.Ord<FooU> = pipe(stringOrd, Ord.contramap(x => x._tag));
export const allFoo = [bar, baz] as const;
export type FooMap<A> = { [K in FooName]: A };
""".trim

  val fooFile = "foo.ts"

  val types = Map(
    fooFile -> (List(parse[Foo]), expectedFooCode),
  )
}

class UnionWithObjectsTest extends CodecTest[UnionWithObjectsTest.Foo](
  outputDir / "unionWithObjects",
  UnionWithObjectsTest.types,
  "FooCU",
  "FooCU",
  UnionWithObjectsTest.fooFile,
)
