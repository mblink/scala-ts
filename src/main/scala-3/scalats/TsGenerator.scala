package scalats

import cats.{Foldable, Monoid}
import cats.syntax.foldable.*
import scala.language.implicitConversions
import scala.util.chaining.*

extension [F[_], A](fa: F[A])(using F: Foldable[F]) {
  private final def intercalateMap[B](glue: B)(f: A => B)(implicit B: Monoid[B]): B =
    B.intercalate(glue).combineAllOption(F.toIterable(fa).map(f)).getOrElse(B.empty)
}

/**
 * Generates [[scalats.Generated]] from instances of [[scalats.TsModel]]
 *
 * [[scalats.TsGenerator.generateTopLevel]] produces TypeScript code with an `io-ts` codec defined as a const and
 * a type definition to match.
 *
 * [[scalats.TsGenerator.generate]] just produces the code of the `io-ts` codec and  determines what to do with it
 * based on the given [[scalats.TsGenerator.State]]. When the state's `top` value is `true`, it calls `state.wrapCodec`
 * with the codec's code. When `top` is `false`, is just returns the codec code.
 *
 * For example:
 *
 * ### Using generateTopLevel
 *
 * ```scala
 * generateTopLevel(TsModel.Number(TypeName("Test")))
 * ```
 *
 * Generates
 *
 * ```ts
 * export const testC = t.number;
 * export type TestC = typeof testC;
 * export type Test = t.TypeOf<TestC>;
 * ```
 *
 * ### Using generate with top = true
 *
 * ```scala
 * generate(
 *   State(true, codec => "export const codec = " |+| codec |+| ";")),
 *   TsModel.Number(TypeName("Test"))
 * )
 * ```
 *
 * Produces
 *
 * ```ts
 * export const codec = t.number;
 * ```
 *
 * ### Using generate with top = false
 *
 * ```scala
 * generate(State(false, identity))
 * ```
 *
 * Produces
 *
 * ```ts
 * t.number
 * ```
 */
final class TsGenerator(customType: TsCustomType, imports: TsImports.Available) {
  private def cap(s: String): String = s.take(1).toUpperCase + s.drop(1)
  private def decap(s: String): String = s.take(1).toLowerCase + s.drop(1)
  private val allCapsRx = """^[A-Z0-9_]+$""".r
  private def maybeDecap(s: String): String =
    s match {
      case allCapsRx() => s
      case _ => decap(s)
    }

  case class WrapCodec(custom: Generated => (Generated => Generated => Generated => Generated) => Generated) {
    final def apply(codec: Generated): Generated =
      custom(codec)(codec => codecType => valueType => codec |+| codecType |+| valueType)
  }

  object WrapCodec {
    val id: WrapCodec = WrapCodec(codec => _ => codec)
  }

  case class State(top: Boolean, wrapCodec: WrapCodec)

  private implicit def liftString(s: String): Generated = imports.lift(s)

  /**
   * Method to generate the name of the generated codec `const`.
   *
   * `case class`es (`interface`s in TS) follow the pattern:
   *   `${typeName.base.take(1).toLowerCase}${typeName.base.drop(1)}C`
   *
   * `enum`s (`union`s in TS) follow the pattern:
   *   `${typeName.base}CU`
   *
   * For example `case class Test()` becomes `testC`
   * and `sealed trait Test` becomes `TestCU`
   */
  private def mkCodecName(typeName: TypeName, isUnion: Boolean): String =
    if (isUnion) typeName.base ++ "CU"
    else maybeDecap(typeName.base) ++ "C"

  /** Wraps the given `codec` with `state.wrapCodec` if `state.top` is true */
  private def maybeWrapCodec(state: State, typeName: TypeName, codec: Generated): List[(Option[TypeName], Generated)] =
    List(if (state.top) (Some(typeName), state.wrapCodec(codec)) else (None, codec))

  /**
   * Produces TypeScript type parameters that extend `iots.Mixed`
   * e.g. `<A1 extends t.Mixed, A2 extends t.Mixed, ...>`
   */
  private def typeArgsMixed(state: State, typeArgs: List[TsModel]): Generated =
    if (typeArgs.isEmpty) Generated.empty
    else "<" |+| typeArgs.intercalateMap(imports.lift(", "))(
      generate(state, _).foldMap(_._2) |+| " extends " |+| imports.iotsMixed
    ) |+| ">"

  /**
   * Produces TypeScript function parameters, typically used in conjunction with `typeArgsMixed`
   * e.g. `(A1: A1, A2: A2, ...) => `
   */
  private def typeArgsFnParams(state: State, typeArgs: List[TsModel]): Generated =
    if (typeArgs.isEmpty) Generated.empty
    else "(" |+| typeArgs.intercalateMap(imports.lift(", "))(
      generate(state, _).foldMap(_._2).pipe(x => x |+| ": " |+| x)
    ) |+| ") => "

  /** The `io-ts` type of a TypeScript record, handles `number`s as `string`s */
  private def recordKeyType(state: State, tpe: TsModel): Generated =
    tpe match {
      case TsModel.Number(_) => imports.iotsNumberFromString
      case _ => generate(state, tpe).foldMap(_._2)
    }

  /** Converts a scala `value` to a TypeScript value. Used when generating objects with known values */
  private def tsValue(tpe: TsModel, value: Any): Generated =
    tpe match {
      case TsModel.TypeParam(_) => sys.error(s"Encountered unexpected type parameter in `tsValue`, value: $value")
      case TsModel.Literal(tpe, value) => tsValue(tpe, value)
      case TsModel.Json(_) => value.asInstanceOf[io.circe.Json].noSpaces
      case TsModel.Number(_) | TsModel.BigNumber(_) | TsModel.Boolean(_) => value.toString
      case TsModel.String(_) | TsModel.LocalDate(_) | TsModel.DateTime(_) => s"`$value`"
      case TsModel.Eval(_, tpe) => tsValue(tpe, value.asInstanceOf[cats.Eval[?]].value)
      case TsModel.Array(_, tpe, toList) =>
        "[" |+| toList(value).intercalateMap(imports.lift(", "))(tsValue(tpe, _)) |+| "]"
      case TsModel.Set(_, tpe) =>
        imports.fptsReadonlySet("fromReadonlyArray(" |+| ordInstance(tpe) |+| ")([" |+|
          value.asInstanceOf[Set[_]].toList.intercalateMap(imports.lift(", "))(tsValue(tpe, _)) |+|
        "])")
      case TsModel.NonEmptyArray(typeName, tpe, toNel) =>
        tsValue(TsModel.Array(typeName, tpe, toNel.andThen(_.toList)), value)
      case TsModel.Option(_, tpe) =>
        imports.fptsOption(value.asInstanceOf[Option[?]].fold[Generated]("none")(a => "some(" |+| tsValue(tpe, a) |+| ")"))
      case TsModel.Either(_, left, right, toEither) =>
        imports.fptsEither(toEither(value).fold(
          l => "left(" |+| tsValue(left, l) |+| ")",
          r => "right(" |+| tsValue(right, r) |+| ")",
        ))
      case TsModel.Ior(_, left, right, toIor) =>
        imports.fptsThese(toIor(value).fold(
          l => "left(" |+| tsValue(left, l) |+| ")",
          r => "right(" |+| tsValue(right, r) |+| ")",
          (l, r) => "both(" |+| tsValue(left, l) |+| ", " |+| tsValue(right, r) |+| ")",
        ))
      case TsModel.Map(_, key, value) =>
        val m = value.asInstanceOf[Map[?, ?]]
        if (m.isEmpty) imports.lift("{}")
        else "{" |+| m.toList.intercalateMap(imports.lift(", ")) { case (k, v) =>
          tsValue(key, k) |+| ": " |+| tsValue(value, v)
        } |+| "}"
      case TsModel.Tuple(_, tpes) =>
        "[" |+| tpes.zip(value.asInstanceOf[Product].productIterator).intercalateMap(imports.lift(", "))(tsValue.tupled) |+| "]"
      case (
        _: TsModel.Interface
        | _: TsModel.InterfaceRef
        | _: TsModel.Object
        | _: TsModel.ObjectRef
        | _: TsModel.Union
        | _: TsModel.UnionRef
        | _: TsModel.Unknown
      ) => imports.custom(tpe.typeName, maybeDecap(value.getClass.getSimpleName.stripSuffix("$")))
    }

  private def unionOrdName(s: String): String = decap(s) ++ "Ord"

  /** Produces code that refers to an `fp-ts` `Ord` instance for a given type */
  private def ordInstance(tpe: TsModel): Generated = {
    lazy val err = sys.error(s"`Ord` instance requested for ${tpe.typeName.full} but not found")

    tpe match {
      case TsModel.Literal(tpe, _) => ordInstance(tpe)
      case TsModel.String(_) => imports.fptsString("Ord", Some("stringOrd"))
      case TsModel.Boolean(_) => imports.fptsBoolean("Ord", Some("boolOrd"))
      case TsModel.Number(_) => imports.fptsNumber("Ord", Some("numberOrd"))
      case TsModel.Eval(_, tpe) => ordInstance(tpe)
      case TsModel.Array(_, tpe, _) => imports.fptsReadonlyArray("getOrd(" |+| ordInstance(tpe) |+| ")")
      case TsModel.Option(_, tpe) => imports.fptsOption("Ord(" |+| ordInstance(tpe) |+| ")")
      case TsModel.Union(typeName, _, _) => imports.custom(typeName, unionOrdName(typeName.base))
      case TsModel.UnionRef(typeName, _) => imports.custom(typeName, unionOrdName(typeName.base))

      case (
        TsModel.TypeParam(_)
        | TsModel.Json(_)
        | TsModel.BigNumber(_)
        | TsModel.LocalDate(_)
        | TsModel.DateTime(_)
        | TsModel.Set(_, _)
        | TsModel.NonEmptyArray(_, _, _)
        | TsModel.Either(_, _, _, _)
        | TsModel.Ior(_, _, _, _)
        | TsModel.Map(_, _, _)
        | TsModel.Tuple(_, _)
        | TsModel.Interface(_, _, _, _)
        | TsModel.InterfaceRef(_, _)
        | TsModel.Object(_, _, _)
        | TsModel.ObjectRef(_)
        | TsModel.Unknown(_, _)
      ) => err
    }
  }

  /** Produces code that refers to a given type, represented by its `typeName` and `typeArgs` */
  private def generateTypeRef(state: State, typeName: TypeName, typeArgs: List[TsModel], isUnion: Boolean): List[(Option[TypeName], Generated)] =
    maybeWrapCodec(state, typeName, customType(typeName.raw).getOrElse(
      imports.custom(typeName, mkCodecName(typeName, isUnion)) |+|
      (if (typeArgs.isEmpty) Generated.empty
      else "(" |+| typeArgs.intercalateMap(imports.lift(", "))(generate(state.copy(top = false), _).foldMap(_._2)) |+| ")")
    ))

  private def tagField(tag: String): TsModel.ObjectField =
    TsModel.ObjectField("_tag", TsModel.Literal(TsModel.String(TypeName("String")), tag), tag)

  /** Produces code for a type with a given set of fields */
  private def generateFieldsCodec(state: State, fields: List[TsModel.Field]): Generated =
    imports.iotsTypeFunction(
      "{\n" |+|
      fields.intercalateMap(imports.lift(",\n"))(field =>
        "  " |+| field.name |+| ": " |+| generate(state.copy(top = false), field.tpe).foldMap(_._2)
      ) |+|
      "\n}"
    )

  /** Produces code for a scala `object` definition, represented as a `const` in TypeScript */
  private def generateObject(state: State, obj: TsModel.Object): List[(Option[TypeName], Generated)] = {
    val TsModel.Object(typeName, parent, fields0) = obj
    val name = typeName.base
    val constName = maybeDecap(name)
    val valueType = cap(constName)
    val taggedCodecName = constName + "TaggedC"
    val taggedValueType = cap(taggedCodecName).stripSuffix("C")
    val fields = parent.fold(Nil)(_ => List(tagField(name))) ++ fields0

    lazy val fullCodec: Generated = state.wrapCodec(
      generateFieldsCodec(state, fields.map(f => TsModel.ObjectField(f.name, TsModel.Literal(f.tpe, f.value), f.value)))
    )

    lazy val minimalTaggedCodec: Generated =
      // Codec with only `_tag` value
      "export const " |+| taggedCodecName |+| " = " |+|
      generateFieldsCodec(state, List(tagField(name))) |+|
      ";\n" |+|
      // Tagged codec type
      "export type " |+| cap(taggedCodecName) |+| " = typeof " |+| taggedCodecName |+| ";\n" |+|
      // Tagged value type
      "export type " |+| taggedValueType |+| " = " |+| imports.iotsTypeOf(cap(taggedCodecName)) |+| ";\n" |+|
      // Full codec type
      state.wrapCodec.custom(imports.fptsPipe(
        taggedCodecName |+| ", c => new " |+|
        imports.iotsTypeType |+|
        "<" |+| valueType |+| ", " |+| taggedValueType |+| ">(\n" |+|
        "  `" |+| name |+| "`,\n" |+|
        "  (u: unknown): u is " |+| valueType |+| " => " |+| imports.fptsEither("isRight(c.decode(u)),\n") |+|
        "  (u: unknown): " |+| imports.fptsEither("Either<") |+| imports.iotsErrors |+| ", " |+| valueType |+| "> => " |+|
        imports.fptsPipe("c.decode(u), " |+| imports.fptsEither("map(x => ({ ...x, ..." |+| constName |+| " }))")) |+|
        ",\n" |+|
        "  (x: " |+| valueType |+| "): " |+| taggedValueType |+| " => ({ ...x, _tag: `" |+| name |+| "`}),\n" |+|
        ")"
      ))(codec => codecType => _ /* we create our own value type */ =>
         // Full value type
         "export type " |+| valueType |+| " = " |+| taggedValueType |+| " & typeof " |+| constName |+| ";\n" |+|
         codec |+|
         codecType
      )

    val codec =
      // Const with all values
      "export const " |+| constName |+| " = {\n" |+|
      fields.intercalateMap(imports.lift(",\n")) {
        case TsModel.ObjectField(name, tpe, value) => s"  $name: " |+| tsValue(tpe, value)
      } |+|
      "\n} as const;\n\n" |+|
      parent.fold(fullCodec)(_ => minimalTaggedCodec)

    List((Some(typeName), codec))
  }

  /** Produces code for a scala `case class` definition */
  private def generateInterface(state: State, iface: TsModel.Interface): List[(Option[TypeName], Generated)] = {
    val TsModel.Interface(typeName, parent, typeArgs, fields0) = iface
    val fields = parent.fold(Nil)(_ => List(tagField(typeName.base))) ++ fields0

    List((Some(typeName), state.wrapCodec(generateFieldsCodec(state, fields))))
  }

  /** Produces code for a scala `enum`/`sealed trait`/`sealed class` */
  private def generateUnion(state: State, union: TsModel.Union): List[(Option[TypeName], Generated)] = {
    val TsModel.Union(typeName, typeArgs, possibilities) = union
    val valueType = typeName.base

    given mb: Monoid[Boolean] = Monoid.instance(true, _ && _)

    val (
      allConstant,
      memberNames,
      memberTypeArgs,
      memberConstNames,
      memberCodecNames,
      memberCodecs
    ) = possibilities.foldMap(x => {
      val name = x.typeName.base
      val codec = generateTopLevel(x)
      x match {
        case i: TsModel.Interface =>
          (false, List(name), List(i.typeArgs.toSet), Nil, List(mkCodecName(i.typeName, false)), List(codec))
        case o: TsModel.Object =>
          (true, List(name), List(o.typeArgs.toSet), List(maybeDecap(name)), List(mkCodecName(o.typeName, false)), List(codec))
      }
    })

    val allMemberCodecs = memberCodecNames.zipWithIndex.map { case (name, idx) =>
      val mtas = memberTypeArgs(idx)
      val tas = typeArgs.filter(mtas.contains)
      if (tas.isEmpty) imports.lift(name)
      else name |+| "(" |+| tas.intercalateMap(imports.lift(", "))(generate(state.copy(top = false), _).foldMap(_._2)) |+| ")"
    }
    val allMemberCodecsArr = "[" |+| allMemberCodecs.intercalate(imports.lift(", ")) |+| "]"
    val allNamesConstName = "all" |+| valueType |+| "Names"
    lazy val ordInst =
      "export const " |+| unionOrdName(valueType) |+| ": " |+|
      imports.fptsOrd("Ord<" |+| valueType |+| "U> = ") |+|
      imports.fptsPipe(imports.fptsString("Ord", Some("stringOrd")) |+| ", " |+| imports.fptsOrd("contramap(x => x._tag)")) |+|
      ";\n"

    Monoid[List[(Option[TypeName], Generated)]].combine(memberCodecs.flatten, List((Some(typeName),
      "export const all" |+| valueType |+| "C = " |+|
      typeArgsMixed(state.copy(top = false), typeArgs) |+|
      typeArgsFnParams(state.copy(top = false), typeArgs) |+|
      allMemberCodecsArr |+| " as const;\n" |+|
      "export const " |+| allNamesConstName |+| " = [" |+| memberNames.intercalateMap(", ")(s => s"`$s`") |+| "] as const;\n" |+|
      "export type " |+| valueType |+| "Name = (typeof " |+| allNamesConstName |+| ")[number];\n\n" |+|
      state.wrapCodec(allMemberCodecs match {
        case Nil => sys.error(s"Can't generate TS code for union `$typeName` with 0 members")
        case h :: Nil => h
        case _ :: _ => imports.iotsUnion(allMemberCodecsArr)
      }) |+|
      "\n" |+|
      (if (typeArgs.isEmpty) ordInst else Generated.empty) |+|
      (
        if (allConstant) "export const all" |+| valueType |+| " = [" |+| memberConstNames.mkString(", ") |+| "] as const;\n"
        else Generated.empty
      ) |+|
      (
        if (typeArgs.isEmpty) "export type " |+| valueType |+| "Map<A> = { [K in " |+| valueType |+| "Name]: A };\n"
        else Generated.empty
      )
    )))
  }

  /**
   * Produces TypeScript code for a given type
   *
   * When `state.top` is `true`, the generated code is passed to `state.wrapCodec`.
   * When `state.top` is `false`, the generated code is returned as is.
   *
   * @param state The generator state that specifies whether we're generating a top-level definiton
   * @param model The [[scalats.TsModel]] to generate TypeScript code for
   * @return A `List` of the [[scalats.TypeName]]s and [[scalats.Generated]] code produced
   */
  def generate(state: State, model: TsModel): List[(Option[TypeName], Generated)] =
    model match {
      case t @ TsModel.TypeParam(name) => maybeWrapCodec(state, t.typeName, name)
      case t @ TsModel.Literal(tpe, value) => maybeWrapCodec(state, t.typeName, imports.iotsLiteral(tsValue(tpe, value)))
      case TsModel.Json(typeName) => maybeWrapCodec(state, typeName, imports.iotsUnknown)
      case TsModel.Number(typeName) => maybeWrapCodec(state, typeName, imports.iotsNumber)
      case TsModel.BigNumber(typeName) => maybeWrapCodec(state, typeName, imports.iotsBigNumber)
      case TsModel.Boolean(typeName) => maybeWrapCodec(state, typeName, imports.iotsBoolean)
      case TsModel.String(typeName) => maybeWrapCodec(state, typeName, imports.iotsString)
      case TsModel.LocalDate(typeName) => maybeWrapCodec(state, typeName, imports.iotsLocalDate)
      case TsModel.DateTime(typeName) => maybeWrapCodec(state, typeName, imports.iotsDateTime)
      case TsModel.Eval(_, tpe) => generate(state, tpe)
      case TsModel.Array(typeName, tpe, _) => maybeWrapCodec(state, typeName, imports.iotsReadonlyArray(generate(state.copy(top = false), tpe).foldMap(_._2)))
      case TsModel.Set(typeName, tpe) => maybeWrapCodec(state, typeName, imports.iotsReadonlySetFromArray(generate(state.copy(top = false), tpe).foldMap(_._2), ordInstance(tpe)))
      case TsModel.NonEmptyArray(typeName, tpe, _) => maybeWrapCodec(state, typeName, imports.iotsReadonlyNonEmptyArray(generate(state.copy(top = false), tpe).foldMap(_._2)))
      case TsModel.Option(typeName, tpe) => maybeWrapCodec(state, typeName, imports.iotsOption(generate(state.copy(top = false), tpe).foldMap(_._2)))
      case TsModel.Either(typeName, left, right, _) => maybeWrapCodec(state, typeName, imports.iotsEither(generate(state.copy(top = false), left).foldMap(_._2), generate(state.copy(top = false), right).foldMap(_._2)))
      case TsModel.Ior(typeName, left, right, _) => maybeWrapCodec(state, typeName, imports.iotsThese(generate(state.copy(top = false), left).foldMap(_._2), generate(state.copy(top = false), right).foldMap(_._2)))
      case TsModel.Map(typeName, key, value) => maybeWrapCodec(state, typeName, imports.iotsRecord(recordKeyType(state.copy(top = false), key), generate(state.copy(top = false), value).foldMap(_._2)))
      case TsModel.Tuple(typeName, tpes) =>
        val updState = state.copy(top = false)
        maybeWrapCodec(state, typeName, imports.iotsTuple("[" |+|
          tpes.intercalateMap(imports.lift(", "))(t => generate(updState, t).foldMap(_._2)) |+|
        "]"))
      case i: TsModel.Interface =>
        if (state.top) generateInterface(state, i) else generateTypeRef(state, i.typeName, i.typeArgs, false)
      case TsModel.InterfaceRef(typeName, typeArgs) => generateTypeRef(state, typeName, typeArgs, false)
      case o: TsModel.Object => if (state.top) generateObject(state, o) else generateTypeRef(state, o.typeName, Nil, false)
      case TsModel.ObjectRef(typeName) => generateTypeRef(state, typeName, Nil, false)
      case u: TsModel.Union => if (state.top) generateUnion(state, u) else generateTypeRef(state, u.typeName, u.typeArgs, true)
      case TsModel.UnionRef(typeName, typeArgs) => generateTypeRef(state, typeName, typeArgs, true)
      case TsModel.Unknown(typeName, typeArgs) => generateTypeRef(state, typeName, typeArgs, false)
    }

  /**
   * Produces TypeScript code for a top-level definition of a given type
   *
   * @param model The [[scalats.TsModel]] to generate TypeScript code for
   * @return A `List` of the [[scalats.TypeName]]s and [[scalats.Generated]] code produced
   */
  def generateTopLevel(model: TsModel): List[(Option[TypeName], Generated)] = {
    val codecName = mkCodecName(model.typeName, model match {
      case _: TsModel.Union => true
      case _ => false
    })
    val codecType = cap(codecName)
    val valueType = codecType.replaceAll("C(U?)$", "$1")
    val className = codecName ++ "C"
    val hasTypeArgs = model match {
      case _: TsModel.Interface | _: TsModel.Object | _: TsModel.Union => model.typeArgs.nonEmpty
      case _ => false
    }

    if (hasTypeArgs) {
      val updState = State(false, WrapCodec.id)
      val tpArgsPlain = model.typeArgs.intercalateMap(imports.lift(","))(generate(updState, _).foldMap(_._2))
      val tpArgsIots = typeArgsMixed(updState, model.typeArgs)
      val fnArgs = typeArgsFnParams(updState, model.typeArgs)
      generate(State(true, WrapCodec(codec => f =>
        f(
          "export class " |+| className |+|
          tpArgsIots |+|
          "{ codec = " |+|
          fnArgs |+|
          codec |+|
          "}\n" |+|
          "export const " |+| codecName |+| " = " |+|
          tpArgsIots |+|
          fnArgs |+|
          "new " |+| className |+| "<" |+| tpArgsPlain |+| ">().codec(" |+| tpArgsPlain |+| ");\n"
        )(
          "export type " |+| codecType |+| tpArgsIots |+|
          " = ReturnType<" |+| className |+| "<" |+| tpArgsPlain |+| ">[\"codec\"]>;\n"
        )(
          "export type " |+| valueType |+| "<" |+| tpArgsPlain |+| "> = " |+|
          imports.iotsTypeOf(
            codecType |+| "<" |+|
            model.typeArgs.intercalateMap(imports.lift(","))(
              t => imports.iotsTypeType |+| "<" |+| generate(updState, t).foldMap(_._2) |+| ">"
            ) |+|
            ">"
          ) |+|
          ";"
        )
      )), model)
    } else
      generate(State(true, WrapCodec(codec => f =>
        f(
          "export const " |+| codecName |+| " = " |+|
          codec |+|
          ";\n"
        )(
          "export type " |+| codecType |+| " = typeof " |+| codecName |+| ";\n"
        )(
          "export type " |+| valueType |+| " = " |+| imports.iotsTypeOf(codecType) |+| ";\n"
        )
      )), model)
  }

  /** Parses the types that a scala `case class` refers to */
  private def referencedTypeNamesInterface(iface: TsModel.Interface, currType: TypeName): Map[TypeName, Set[TypeName]] =
    iface.fields.foldMap { case TsModel.InterfaceField(_, tpe) => referencedTypeNames(tpe, currType) }

  /** Parses the types that a scala `object` refers to */
  private def referencedTypeNamesObject(obj: TsModel.Object, currType: TypeName): Map[TypeName, Set[TypeName]] =
    obj.fields.foldMap { case TsModel.ObjectField(_, tpe, _) => referencedTypeNames(tpe, currType) }

  /** Parses the types that a scala `enum`/`sealed trait`/`sealed class` refers to */
  private def referencedTypeNamesUnion(union: TsModel.Union, currType: TypeName): Map[TypeName, Set[TypeName]] = {
    import cats.syntax.semigroup.*
    union.possibilities.foldMap(x => Map(currType -> Set(x.typeName)) |+| (x match {
      case i: TsModel.Interface => referencedTypeNamesInterface(i, i.typeName)
      case o: TsModel.Object => referencedTypeNamesObject(o, o.typeName)
    }))
  }

  /** Parses the types that a given [[scalats.TsModel]] refers to */
  private def referencedTypeNames(model: TsModel, currType: TypeName): Map[TypeName, Set[TypeName]] = {
    import cats.syntax.semigroup.*

    model match {
      case (
        TsModel.TypeParam(_)
        | TsModel.Json(_)
        | TsModel.Number(_)
        | TsModel.BigNumber(_)
        | TsModel.Boolean(_)
        | TsModel.String(_)
        | TsModel.LocalDate(_)
        | TsModel.DateTime(_)
      ) => Map.empty

      case TsModel.Literal(tpe, _) => referencedTypeNames(tpe, currType)
      case TsModel.Eval(_, tpe) => referencedTypeNames(tpe, currType)
      case TsModel.Array(_, tpe, _) => referencedTypeNames(tpe, currType)
      case TsModel.Set(_, tpe) => referencedTypeNames(tpe, currType)
      case TsModel.NonEmptyArray(_, tpe, _) => referencedTypeNames(tpe, currType)
      case TsModel.Option(_, tpe) => referencedTypeNames(tpe, currType)
      case TsModel.Either(_, left, right, _) => referencedTypeNames(left, currType) |+| referencedTypeNames(right, currType)
      case TsModel.Ior(_, left, right, _) => referencedTypeNames(left, currType) |+| referencedTypeNames(right, currType)
      case TsModel.Map(_, key, value) => referencedTypeNames(key, currType) |+| referencedTypeNames(value, currType)
      case TsModel.Tuple(_, tpes) => tpes.foldMap(referencedTypeNames(_, currType))
      case i: TsModel.Interface => referencedTypeNamesInterface(i, currType)
      case TsModel.InterfaceRef(typeName, typeArgs) => Map(currType -> Set(typeName)) |+| typeArgs.foldMap(referencedTypeNames(_, currType))
      case o: TsModel.Object => referencedTypeNamesObject(o, currType)
      case TsModel.ObjectRef(typeName) => Map(currType -> Set(typeName))
      case u: TsModel.Union => referencedTypeNamesUnion(u, currType)
      case TsModel.UnionRef(typeName, typeArgs) => Map(currType -> Set(typeName)) |+| typeArgs.foldMap(referencedTypeNames(_, currType))
      case TsModel.Unknown(typeName, typeArgs) => Map(currType -> Set(typeName)) |+| typeArgs.foldMap(referencedTypeNames(_, currType))
    }
  }

  /**
   * Produces a [[scalats.ReferencedTypes]] containing the list of [[scalats.TypeName]]s
   * that the given [[scalats.TypeName]] refers to
   *
   * @param model The [[scalats.TsModel]] to parse references for
   * @return The types the model references
   */
  def referencedTypes(model: TsModel): ReferencedTypes =
    new ReferencedTypes(referencedTypeNames(model, model.typeName))
}
