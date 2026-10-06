package scalats
package tests

import cats.data.NonEmptyList
import io.circe.{Decoder, Encoder, JsonObject}
import io.circe.syntax.*
import org.scalacheck.{Arbitrary, Gen}

object ObjectConstantsTest {
  case object Consts {
    val list = List(1, 2)
    val empty = List.empty[Int]
    val nel = NonEmptyList.of(1, 2)
    val some = Option(1)
    val none = Option.empty[Int]
    val map = Map("a" -> 1)
    val tuple = (1, "a")
    val int = 1

    given decoder: Decoder[Consts.type] =
      Decoder.instance(c => for {
        _ <- c.get["Consts"]("_tag")
        _ <- c.get[List[Int]]("list")
        _ <- c.get[List[Int]]("empty")
        _ <- c.get[NonEmptyList[Int]]("nel")
        _ <- c.get[Option[Int]]("some")
        _ <- c.get[Option[Int]]("none")
        _ <- c.get[Map[String, Int]]("map")
        _ <- c.get[(Int, String)]("tuple")
        _ <- c.get[1]("int")
      } yield Consts)

    given encoder: Encoder[Consts.type] =
      Encoder.AsObject.instance(consts => JsonObject(
        "_tag" := "Consts",
        "list" := consts.list,
        "empty" := consts.empty,
        "nel" := consts.nel,
        "some" := consts.some,
        "none" := consts.none,
        "map" := consts.map,
        "tuple" := consts.tuple,
        "int" := consts.int,
      ))

    given arb: Arbitrary[Consts.type] = Arbitrary(Gen.const(Consts))
  }

  val expectedConstsCode = """
import * as t from "io-ts";
import * as O from "fp-ts/lib/Option";
import { OptionFromNullableC, optionFromNullable } from "io-ts-types/lib/optionFromNullable";
import { ReadonlyNonEmptyArrayC, readonlyNonEmptyArray } from "io-ts-types/lib/readonlyNonEmptyArray";
import * as RNEA from "fp-ts/lib/ReadonlyNonEmptyArray";

export const consts = {
  _tag: `Consts`,
  empty: [],
  int: 1,
  list: [1, 2],
  map: {[`a`]: 1},
  nel: [1, 2],
  none: O.none,
  some: O.some(1),
  tuple: [1, `a`]
} as const;

export type ConstsC = t.TypeC<{
  _tag: t.LiteralC<`Consts`>,
  empty: t.ReadonlyArrayC<t.NumberC>,
  int: t.LiteralC<1>,
  list: t.ReadonlyArrayC<t.NumberC>,
  map: t.RecordC<t.StringC, t.NumberC>,
  nel: ReadonlyNonEmptyArrayC<t.NumberC>,
  none: OptionFromNullableC<t.NumberC>,
  some: OptionFromNullableC<t.NumberC>,
  tuple: t.TupleC<[t.NumberC, t.StringC]>
}>;
export type Consts = {
  _tag: `Consts`,
  empty: ReadonlyArray<number>,
  int: 1,
  list: ReadonlyArray<number>,
  map: Record<string, number>,
  nel: RNEA.ReadonlyNonEmptyArray<number>,
  none: O.Option<number>,
  some: O.Option<number>,
  tuple: [number, string]
};
export const constsC: ConstsC = t.type({
  _tag: t.literal(`Consts`),
  empty: t.readonlyArray(t.number),
  int: t.literal(1),
  list: t.readonlyArray(t.number),
  map: t.record(t.string, t.number),
  nel: readonlyNonEmptyArray(t.number),
  none: optionFromNullable(t.number),
  some: optionFromNullable(t.number),
  tuple: t.tuple([t.number, t.string])
}) satisfies t.Type<Consts, unknown>;
""".trim

  val constsFile = "consts.ts"

  val types = Map(
    constsFile -> (List(parse[Consts.type]), expectedConstsCode),
  )
}

class ObjectConstantsTest extends CodecTest[ObjectConstantsTest.Consts.type](
  outputDir / "objectConstants",
  ObjectConstantsTest.types,
  "constsC",
  "constsC",
  ObjectConstantsTest.constsFile,
)
