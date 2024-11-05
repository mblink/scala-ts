package scalats
package tests

import io.circe.{Decoder, Encoder}
import scalats.tests.arbitrary.given

object InterfaceWithTypeParamTest {
  case class Foo[A](int: Int, data: A)
  object Foo {
    given decoder[A: Decoder]: Decoder[Foo[A]] = Decoder.derivedConfigured
    given encoder[A: Encoder]: Encoder[Foo[A]] = Encoder.AsObject.derivedConfigured
  }

  val expectedFooCode = """
import * as t from "io-ts";

export type FooC<A1 extends t.Mixed> = t.TypeC<{
  int: t.NumberC,
  data: A1
}>;
export type Foo<A1> = {
  int: number,
  data: A1
};
export const fooC = <A1 extends t.Mixed>(A1: A1): FooC<A1> => t.type({
  int: t.number,
  data: A1
}) satisfies t.Type<Foo<t.TypeOf<A1>>, unknown>;
""".trim

  val fooFile = "foo.ts"

  val types = Map(
    fooFile -> (List(parse[Foo[?]]), expectedFooCode),
  )
}

class InterfaceWithTypeParamTest extends CodecTest[InterfaceWithTypeParamTest.Foo[Int]](
  outputDir / "interface-type-param",
  InterfaceWithTypeParamTest.types,
  "fooC",
  "fooC(t.number)",
  InterfaceWithTypeParamTest.fooFile,
)
