package scalats
package tests

import io.circe.{Decoder, Encoder}
import scalats.tests.arbitrary.given

object UnionTest {
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
  case class Baz(int: Int, str: String) extends Foo

  val expectedFooCode = """
import * as t from "io-ts";
import { Ord as stringOrd } from "fp-ts/lib/string";
import * as E from "fp-ts/lib/Either";
import { pipe } from "fp-ts/lib/function";
import * as Ord from "fp-ts/lib/Ord";

export const bar = {
  int: 1,
  str: `bar`,
  _tag: `Bar`
} as const;

export const barTaggedC = t.type({ _tag: t.literal(`Bar`) });
export type BarTaggedC = typeof barTaggedC;
export type BarTagged = t.TypeOf<BarTaggedC>;
export type Bar = BarTagged & typeof bar;
export const barC = pipe(barTaggedC, c => new t.Type<Bar, BarTagged>(
  `Bar`,
  (u: unknown): u is Bar => E.isRight(c.decode(u)),
  (u: unknown): E.Either<t.Errors, Bar> =>pipe(c.decode(u), E.map(x => ({ ...x, ...bar }))),
  (x: Bar): BarTagged => ({ ...x, _tag: `Bar`}),
));

export const bazC = t.type({
  _tag: t.literal(`Baz`),
  int: t.number,
  str: t.string
});
export type BazC = typeof bazC;
export type Baz = t.TypeOf<BazC>;


export const allFooC = [barC, bazC] as const;
export const allFooNames = [`Bar`, `Baz`] as const;
export type FooName = (typeof allFooNames)[number];

export const FooCU = t.union([barC, bazC]);
export type FooCU = typeof FooCU;
export type FooU = t.TypeOf<FooCU>;

export const fooOrd: Ord.Ord<FooU> = pipe(stringOrd, Ord.contramap(x => x._tag));
export type FooMap<A> = { [K in FooName]: A };
""".trim

  val fooFile = "foo.ts"

  val types = Map(
    fooFile -> (List(parse[Foo]), expectedFooCode),
  )
}

class UnionTest extends CodecTest[UnionTest.Foo](
  outputDir / "union",
  UnionTest.types,
  "FooCU",
  "FooCU",
  UnionTest.fooFile,
)
