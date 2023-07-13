package scalats
package tests

import io.circe.{Decoder, Encoder, JsonObject}
import io.circe.syntax.*
import scalats.tests.arbitrary.given

object ObjectTest {
  case object Foo {
    val int = 1
    val str = "test"

    given decoder: Decoder[Foo.type] =
      Decoder.instance(c => for {
        _ <- c.get["Foo"]("_tag")
        _ <- c.get[1]("int")
        _ <- c.get["test"]("str")
      } yield Foo)

    given encoder: Encoder[Foo.type] =
      Encoder.AsObject.instance(foo => JsonObject("_tag" := "Foo", "int" := foo.int, "str" := foo.str))
  }

  val expectedFooCode = """
import * as t from "io-ts";

export const foo = {
  _tag: `Foo`,
  int: 1,
  str: `test`
} as const;

export const fooC = t.type({
  _tag: t.literal(`Foo`),
  int: t.literal(1),
  str: t.literal(`test`)
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
