package scalats
package tests

import io.circe.{Decoder, Encoder}
import org.scalacheck.Arbitrary
import scalats.tests.arbitrary.*

object TypeAliasTest {
  case class FooF[A](a: A) derives Arbitrary
  object FooF {
    given decoder[A: Decoder]: Decoder[FooF[A]] = Decoder.derivedConfigured
    given encoder[A: Encoder]: Encoder[FooF[A]] = Encoder.AsObject.derivedConfigured
  }
  type Foo = FooF[Int]

  val expectedFooCode = """
import * as t from "io-ts";

export type FooFC<A1 extends t.Mixed> = t.TypeC<{
  a: A1
}>;
export type FooF<A1> = {
  a: A1
};
export const fooFC = <A1 extends t.Mixed>(A1: A1): FooFC<A1> => t.type({
  a: A1
}) satisfies t.Type<FooF<t.TypeOf<A1>>, unknown>;


export type FooC = FooFC<t.NumberC>;
export type Foo = FooF<number>;
export const fooC: FooC = fooFC(t.number) satisfies t.Type<Foo, unknown>;
""".trim

  val fooFile = "foo.ts"

  val types = Map(fooFile -> (List(parse[FooF[?]], parse[Foo]), expectedFooCode))
}

class TypeAliasTest extends CodecTest[TypeAliasTest.Foo](
  outputDir / "typeAlias",
  TypeAliasTest.types,
  "fooC",
  "fooC",
  TypeAliasTest.fooFile,
)
