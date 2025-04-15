package scalats
package tests

import io.circe.{Decoder, Encoder}
import org.scalacheck.Arbitrary

object OpaqueTypeTest {
  opaque type Foo = String
  object Foo {
    given arb: Arbitrary[Foo] = Arbitrary.arbString
    given decoder: Decoder[Foo] = Decoder.decodeString
    given encoder: Encoder[Foo] = Encoder.encodeString
  }

  val expectedFooCode = """
import * as t from "io-ts";

export type FooC = t.StringC;
export type Foo = string;
export const fooC: FooC = t.string satisfies t.Type<Foo, unknown>;
""".trim

  val fooFile = "foo.ts"

  val types = Map(fooFile -> (List(parse[Foo]), expectedFooCode))
}

class OpaqueTypeTest extends CodecTest[OpaqueTypeTest.Foo](
  outputDir / "opaque-type",
  OpaqueTypeTest.types,
  "fooC",
  "fooC",
  OpaqueTypeTest.fooFile,
)
