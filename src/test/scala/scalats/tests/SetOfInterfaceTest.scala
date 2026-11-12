package scalats
package tests

import io.circe.derivation.{ConfiguredDecoder, ConfiguredEncoder}
import org.scalacheck.Arbitrary
import scalats.tests.arbitrary.*

object SetOfInterfaceTest {
  sealed trait Change derives Arbitrary, ConfiguredDecoder, ConfiguredEncoder
  case object Added extends Change
  case object Removed extends Change

  case class Pos(line: Int) derives Arbitrary, ConfiguredDecoder, ConfiguredEncoder
  case class Diff(change: Change, target: String, at: Option[Pos]) derives Arbitrary, ConfiguredDecoder, ConfiguredEncoder
  case class Diffs(changes: Set[Diff]) derives Arbitrary, ConfiguredDecoder, ConfiguredEncoder

  val expectedChangeCode = """
import * as t from "io-ts";
import { Ord as stringOrd } from "fp-ts/lib/string";
import * as E from "fp-ts/lib/Either";
import { pipe } from "fp-ts/lib/function";
import * as Ord from "fp-ts/lib/Ord";

export const added = {
  _tag: `Added`
} as const;

export type AddedTaggedC = t.TypeC<{
  _tag: t.LiteralC<`Added`>
}>;
export const addedTaggedC: AddedTaggedC = t.type({
  _tag: t.literal(`Added`)
});
export type AddedTagged = t.TypeOf<AddedTaggedC>;
export type Added = AddedTagged & typeof added;
export type AddedC = t.Type<Added, AddedTagged>;
export const addedC: AddedC = pipe(addedTaggedC, c => new t.Type<Added, AddedTagged>(
  `Added`,
  (u: unknown): u is Added => E.isRight(c.decode(u)),
  (u: unknown): E.Either<t.Errors, Added> => pipe(c.decode(u), E.map(x => ({ ...x, ...added }))),
  (x: Added): AddedTagged => ({ ...x, _tag: `Added`}),
)) satisfies t.Type<Added, unknown>;


export const removed = {
  _tag: `Removed`
} as const;

export type RemovedTaggedC = t.TypeC<{
  _tag: t.LiteralC<`Removed`>
}>;
export const removedTaggedC: RemovedTaggedC = t.type({
  _tag: t.literal(`Removed`)
});
export type RemovedTagged = t.TypeOf<RemovedTaggedC>;
export type Removed = RemovedTagged & typeof removed;
export type RemovedC = t.Type<Removed, RemovedTagged>;
export const removedC: RemovedC = pipe(removedTaggedC, c => new t.Type<Removed, RemovedTagged>(
  `Removed`,
  (u: unknown): u is Removed => E.isRight(c.decode(u)),
  (u: unknown): E.Either<t.Errors, Removed> => pipe(c.decode(u), E.map(x => ({ ...x, ...removed }))),
  (x: Removed): RemovedTagged => ({ ...x, _tag: `Removed`}),
)) satisfies t.Type<Removed, unknown>;


export const allChangeC = [addedC, removedC] as const;
export const allChangeNames = [`Added`, `Removed`] as const;
export type ChangeName = (typeof allChangeNames)[number];

export type ChangeCU = t.UnionC<[AddedC, RemovedC]>;
export type ChangeU = Added | Removed;
export const ChangeCU: ChangeCU = t.union([addedC, removedC]) satisfies t.Type<ChangeU, unknown>;

export const changeOrd: Ord.Ord<ChangeU> = pipe(stringOrd, Ord.contramap(x => x._tag));
export const allChange = [added, removed] as const;
export type ChangeMap<A> = { [K in ChangeName]: A };
""".trim

  val expectedDiffCode = """
import * as O from "fp-ts/lib/Option";
import * as t from "io-ts";
import { ChangeCU as imported1_ChangeCU, ChangeU as imported0_ChangeU, ChangeCU as imported0_ChangeCU } from "./change";
import { OptionFromNullableC, optionFromNullable } from "io-ts-types/lib/optionFromNullable";

export type PosC = t.TypeC<{
  line: t.NumberC
}>;
export type Pos = {
  line: number
};
export const posC: PosC = t.type({
  line: t.number
}) satisfies t.Type<Pos, unknown>;


export type DiffC = t.TypeC<{
  change: imported1_ChangeCU,
  target: t.StringC,
  at: OptionFromNullableC<PosC>
}>;
export type Diff = {
  change: imported0_ChangeU,
  target: string,
  at: O.Option<Pos>
};
export const diffC: DiffC = t.type({
  change: imported0_ChangeCU,
  target: t.string,
  at: optionFromNullable(posC)
}) satisfies t.Type<Diff, unknown>;
""".trim

  val expectedDiffsCode = """
import * as t from "io-ts";
import { Ord as stringOrd } from "fp-ts/lib/string";
import * as O from "fp-ts/lib/Option";
import { diffC as imported0_diffC, Pos as imported1_Pos, DiffC as imported0_DiffC, Diff as imported1_Diff, Diff as imported0_Diff } from "./diff";
import { pipe } from "fp-ts/lib/function";
import * as Ord from "fp-ts/lib/Ord";
import { ReadonlySetFromArrayC, readonlySetFromArray } from "io-ts-types/lib/readonlySetFromArray";
import { changeOrd as imported0_changeOrd } from "./change";
import { Ord as numberOrd } from "fp-ts/lib/number";

export type DiffsC = t.TypeC<{
  changes: ReadonlySetFromArrayC<imported0_DiffC>
}>;
export type Diffs = {
  changes: ReadonlySet<imported1_Diff>
};
export const diffsC: DiffsC = t.type({
  changes: readonlySetFromArray(imported0_diffC, pipe(Ord.tuple(imported0_changeOrd, stringOrd, O.getOrd(pipe(Ord.tuple(numberOrd), Ord.contramap((x: imported1_Pos) => [x.line] as const)))), Ord.contramap((x: imported0_Diff) => [x.change, x.target, x.at] as const)))
}) satisfies t.Type<Diffs, unknown>;
""".trim

  val changeFile = "change.ts"
  val diffFile = "diff.ts"
  val diffsFile = "diffs.ts"

  val types = Map(
    changeFile -> (List(parse[Change]), expectedChangeCode),
    diffFile -> (List(parse[Pos], parse[Diff]), expectedDiffCode),
    diffsFile -> (List(parse[Diffs]), expectedDiffsCode),
  )
}

class SetOfInterfaceTest extends CodecTest[SetOfInterfaceTest.Diffs](
  outputDir / "setOfInterface",
  SetOfInterfaceTest.types,
  "diffsC",
  "diffsC",
  SetOfInterfaceTest.diffsFile,
)

object SetOfInterfaceMissingOrdTest {
  case class WithUuid(id: java.util.UUID)
  case class Node(label: String, next: Option[Node])
}

class SetOfInterfaceMissingOrdTest extends munit.FunSuite {
  import SetOfInterfaceMissingOrdTest.*

  private def ordError(model: TsModel): String =
    intercept[RuntimeException](referenceCode(model)).getMessage

  test("a case class field without an Ord fails generation") {
    assertEquals(ordError(parseReference[Set[WithUuid]]), "`Ord` instance requested for java.util.UUID but not found")
  }

  test("a recursive case class fails generation") {
    assertEquals(
      ordError(parseReference[Set[Node]]),
      "`Ord` instance requested for scalats.tests.SetOfInterfaceMissingOrdTest.Node but not found",
    )
  }
}
