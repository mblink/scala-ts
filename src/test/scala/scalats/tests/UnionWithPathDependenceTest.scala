package scalats
package tests

import io.circe.{Decoder, Encoder}
import org.scalacheck.{Arbitrary, Gen}

object UnionWithPathDependenceTest {
  sealed trait DefinesFoo {
    sealed trait Foo
  }
  object ExtendsFoo1 extends DefinesFoo {
    case object Bar extends Foo
  }
  object ExtendsFoo2 extends DefinesFoo {
    case object Baz extends Foo
  }

  given foo1Decoder: Decoder[ExtendsFoo1.Foo] = Decoder.derivedConfigured
  given foo1Encoder: Encoder[ExtendsFoo1.Foo] = Encoder.AsObject.derivedConfigured
  given foo1Arbitrary: Arbitrary[ExtendsFoo1.Foo] = Arbitrary(Gen.const(ExtendsFoo1.Bar))

  val extendsFoo1Code = """
import * as t from "io-ts";
import { Ord as stringOrd } from "fp-ts/lib/string";
import * as E from "fp-ts/lib/Either";
import { pipe } from "fp-ts/lib/function";
import * as Ord from "fp-ts/lib/Ord";

export const bar = {
  _tag: `Bar`
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


export const allFooC = [barC] as const;
export const allFooNames = [`Bar`] as const;
export type FooName = (typeof allFooNames)[number];

export type FooCU = BarC;
export type FooU = Bar;
export const FooCU: FooCU = barC satisfies t.Type<FooU, unknown>;

export const fooOrd: Ord.Ord<FooU> = pipe(stringOrd, Ord.contramap(x => x._tag));
export const allFoo = [bar] as const;
export type FooMap<A> = { [K in FooName]: A };
""".trim

  val extendsFoo1File = "extendsFoo1.ts"
  val extendsFoo1Types = Map(extendsFoo1File -> (List(parse[ExtendsFoo1.Foo]), extendsFoo1Code))
}

class UnionWithPathDependenceTest extends CodecTest[UnionWithPathDependenceTest.ExtendsFoo1.Foo](
  outputDir / "union-path-dependence",
  UnionWithPathDependenceTest.extendsFoo1Types,
  "FooCU",
  "FooCU",
  UnionWithPathDependenceTest.extendsFoo1File,
)
