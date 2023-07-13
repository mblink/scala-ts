package scalats
package tests

import io.circe.{Decoder, Encoder, Json, JsonObject}
import scalats.tests.arbitrary.given

object ObjectTest {
  case object Foo {
    val name = "foo"
    given decoder: Decoder[Foo.type] =
      Decoder.instance(_.get["foo"]("name").map(_ => Foo))

    given encoder: Encoder[Foo.type] =
      Encoder.AsObject.instance(foo => JsonObject.singleton("name", Json.fromString(foo.name)))
  }

  val expectedFooCode = """
import * as t from "io-ts";

export const foo = {
  name: `foo`
} as const;

export const fooC = t.type({
  name: t.literal(`foo`)
});
export type FooC = typeof fooC;
export type Foo = t.TypeOf<FooC>;
""".trim

  val fooFile = "foo.ts"

  val types = Map(
    fooFile -> (List(parse[Foo.type]), expectedFooCode),
  )
}

class ObjectTest extends CodecTest[ObjectTest.Foo.type](
  outputDir / "object",
  ObjectTest.types,
  "fooC",
  "fooC",
  ObjectTest.fooFile,
) {
  // `Foo.type` is a singleton so we can set min successful tests to 1 as nothing will change from test to test
  override def scalaCheckTestParameters = super.scalaCheckTestParameters.withMinSuccessfulTests(1)
}
