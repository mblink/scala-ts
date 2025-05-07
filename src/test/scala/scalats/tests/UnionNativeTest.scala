package scalats
package tests

import cats.syntax.functor.*
import io.circe.{Decoder, Encoder}
import io.circe.derivation.{ConfiguredDecoder, ConfiguredEncoder}
import io.circe.syntax.*
import org.scalacheck.{Arbitrary, Gen}
import scalats.tests.arbitrary.*

object UnionNativeTest {
  case class Foo(run: Int | String) derives Arbitrary, ConfiguredDecoder, ConfiguredEncoder
  object Foo {
    private given intOrStringDecoder: Decoder[Int | String] =
      Decoder[Int].widen[Int | String].or(Decoder[String].widen[Int | String])

    private given intOrStringEncoder: Encoder[Int | String] =
      Encoder.instance {
        case i: Int => i.asJson
        case s: String => s.asJson
      }

    private given intOrStringArb: Arbitrary[Int | String] =
      Arbitrary(Gen.oneOf(
        Arbitrary.arbitrary[Int].map(identity[Int | String]),
        Arbitrary.arbitrary[String].map(identity[Int | String]),
      ))
  }

  val expectedFooCode = """
import * as t from "io-ts";

export type FooC = t.TypeC<{
  run: t.UnionC<[t.NumberC, t.StringC]>
}>;
export type Foo = {
  run: number | string
};
export const fooC: FooC = t.type({
  run: t.union([t.number, t.string])
}) satisfies t.Type<Foo, unknown>;
""".trim

  val fooFile = "foo.ts"

  val types = Map(fooFile -> (List(parse[Foo]), expectedFooCode))
}

class UnionNativeTest extends CodecTest[UnionNativeTest.Foo](
  outputDir / "unionNative",
  UnionNativeTest.types,
  "fooC",
  "fooC",
  UnionNativeTest.fooFile,
)
