package scalats

import cats.{Foldable, Monoid}
import cats.syntax.foldable.*
import scala.language.implicitConversions
import scala.util.chaining.*
import org.slf4j.LoggerFactory

extension [F[_], A](fa: F[A])(using F: Foldable[F]) {
  private def intercalateMap[B](glue: B)(f: A => B)(implicit B: Monoid[B]): B =
    B.intercalate(glue).combineAllOption(F.toIterable(fa).map(f)).getOrElse(B.empty)
}

/**
 * Generates [[scalats.Generated]] from instances of [[scalats.TsModel]]
 *
 * [[scalats.TsGenerator.generateTopLevel]] produces TypeScript code with an `io-ts` codec defined as a const and
 * a type definition to match.
 *
 * [[scalats.TsGenerator.generateCodecValue]] just produces the code of the `io-ts` codec and  determines what to do with it
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
 * export type TestC = t.NumberC;
 * export type Test = number;
 * export const testC: TestC = t.number satisfies t.Type<Test, unknown>;
 * ```
 *
 * ### Using generateCodecValue with top = true
 *
 * ```scala
 * generateCodecValue(
 *   State(true, WrapCodec(codec => _ => "export const codec = " |+| codec |+| ";")),
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
 * ### Using generateCodecValue with top = false
 *
 * ```scala
 * generateCodecValue(
 *   State(false, g.WrapCodec(codec => _ => codec)),
 *   TsModel.Number(TypeName("Test"))
 * )
 * ```
 *
 * Produces
 *
 * ```ts
 * t.number
 * ```
 */
class TsGenerator(
  customType: TsCustomType,
  customOrd: TsCustomOrd,
  imports: TsImports.Available,
  debug: Boolean = true,
  debugFilter: String = "",
) {
  val logger = LoggerFactory.getLogger("scala-ts")
  private def filteredLog(logMsg: String): Unit = {
    if(logMsg.containsSlice(debugFilter)) logger.info(logMsg)
  }
  private def debugLog(ttype: String, name: String, extra: Option[String]): Unit = {
    if (this.debug)
      filteredLog(s"Generating $ttype for $name => ${name}C" + extra.getOrElse(""))
  }

  private def cap(s: String): String = s.take(1).toUpperCase + s.drop(1)
  private def decap(s: String): String = s.take(1).toLowerCase + s.drop(1)
  private val allCapsRx = """^[A-Z0-9_]+$""".r
  protected def maybeDecap(s: String): String =
    s match {
      case allCapsRx() => s
      case _ => decap(s)
    }

  case class WrapCodec(custom: Generated => (Generated => Generated => Generated => Generated) => Generated) {
    final def apply(codec: Generated): Generated =
      custom(codec)(codecType => valueType => codec => codecType |+| valueType |+| codec)
  }

  object WrapCodec {
    val id: WrapCodec = WrapCodec(codec => _ => codec)
  }

  case class State(top: Boolean, wrapCodec: WrapCodec)

  protected implicit def liftString(s: String): Generated = imports.lift(s)

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

  private def mkValueName(typeName: TypeName, isUnion: Boolean): String =
    cap(mkCodecName(typeName, isUnion)).replaceAll("C(U?)$", "$1")

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
      generateCodecValue(state, _).foldMap(_._2) |+| " extends " |+| imports.iotsMixed
    ) |+| ">"

  /**
   * Produces TypeScript function parameters, typically used in conjunction with `typeArgsMixed`
   * e.g. `(A1: A1, A2: A2, ...)[: RetType] => `
   */
  private def typeArgsFnParams(state: State, typeArgs: List[TsModel], retType: Option[Generated]): Generated =
    if (typeArgs.isEmpty) Generated.empty
    else "(" |+| typeArgs.intercalateMap(imports.lift(", "))(
      generateCodecValue(state, _).foldMap(_._2).pipe(x => x |+| ": " |+| x)
    ) |+| ")" |+| retType.fold(Generated.empty)(t => ": " |+| t) |+| " => "

  /**
   * The `io-ts` type corresponding to a Scala `Map`
   *
   * Produces a TypeScript `Record` when the keys are `string`s and a `ReadonlyMap` otherwise
   */
  private def recordCodecType(keyTpe: TsModel, valTpe: TsModel): Generated =
    keyTpe match {
      case TsModel.String(_) => imports.iotsRecordC(imports.iotsStringC, generateCodecType(valTpe))
      case _ => imports.iotsReadonlyMapFromEntriesType(generateCodecType(keyTpe), generateCodecType(valTpe))
    }

  private def recordValueType(keyTpe: TsModel, valTpe: TsModel): Generated =
    keyTpe match {
      case TsModel.String(_) => imports.recordType(imports.stringType, generateValueType(valTpe))
      case _ => imports.readonlyMapType(generateValueType(keyTpe), generateValueType(valTpe))
    }

  /**
   * The `io-ts` value corresponding to a Scala `Map`
   *
   * Produces a TypeScript `Record` when the keys are `string`s and a `ReadonlyMap` otherwise
   */
  private def recordCodecValue(state: State, keyTpe: TsModel, valTpe: TsModel): Generated =
    keyTpe match {
      case TsModel.String(_) => imports.iotsRecord(imports.iotsString, generateCodecValue(state, valTpe).foldMap(_._2))
      case _ =>
        imports.iotsReadonlyMapFromEntriesValue(
          generateCodecValue(state, keyTpe).foldMap(_._2),
          ordInstance(keyTpe),
          generateCodecValue(state, valTpe).foldMap(_._2),
        )
    }

  /** Converts a scala `value` to a TypeScript value. Used when generating objects with known values */
  protected def tsValue(tpe: TsModel, value: Any): Generated =
    tpe match {
      case TsModel.TypeParam(_) => sys.error(s"Encountered unexpected type parameter in `tsValue`, value: $value")
      case TsModel.Literal(tpe, value) => tsValue(tpe, value)
      case TsModel.Json(_) => value.asInstanceOf[io.circe.Json].noSpaces
      case TsModel.Number(_) | TsModel.BigNumber(_) | TsModel.Boolean(_) => value.toString
      case TsModel.String(_) | TsModel.LocalDate(_) | TsModel.DateTime(_) | TsModel.UUID(_) => s"`$value`"
      case TsModel.Eval(_, tpe) => tsValue(tpe, value.asInstanceOf[cats.Eval[?]].value)
      case TsModel.Array(_, tpe, toList) =>
        "[" |+| toList(value).intercalateMap(imports.lift(", "))(tsValue(tpe, _)) |+| "]"
      case TsModel.Set(_, tpe) =>
        imports.fptsReadonlySet("fromReadonlyArray(" |+| ordInstance(tpe) |+| ")([" |+|
          value.asInstanceOf[Set[?]].toList.intercalateMap(imports.lift(", "))(tsValue(tpe, _)) |+|
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
        | _: TsModel.UnionTypeRef
        | _: TsModel.TypeAlias
        | _: TsModel.Unknown
      ) =>
        value match {
          case p: Product => imports.custom(tpe.typeName, maybeDecap(p.productPrefix))
          case _ => imports.custom(tpe.typeName, maybeDecap(value.getClass.getSimpleName.stripSuffix("$")))
        }
    }

  private def unionOrdName(s: String): String = decap(s) ++ "Ord"

  private def isObject(m: TsModel.Object | TsModel.ObjectRef | TsModel.Interface | TsModel.InterfaceRef): Boolean =
    m match {
      case _: TsModel.Object | _: TsModel.ObjectRef => true
      case _: TsModel.Interface | _: TsModel.InterfaceRef => false
    }

  private def unionHasOrd(u: TsModel.Union | TsModel.UnionRef): Boolean =
    u match {
      case u: TsModel.Union => u.typeArgs.isEmpty && u.possibilities.forall(isObject)
      case u: TsModel.UnionRef => u.typeArgs.isEmpty && u.allObjects
    }

  /** Produces code that refers to an `fp-ts` `Ord` instance for a given type */
  private def ordInstance(tpe: TsModel): Generated = {
    lazy val err = sys.error(s"`Ord` instance requested for ${tpe.typeName.full} but not found")

    customOrd(tpe.typeName).getOrElse(tpe match {
      case TsModel.Literal(tpe, _) => ordInstance(tpe)
      case TsModel.String(_) => imports.fptsString("Ord", Some("stringOrd"))
      case TsModel.Boolean(_) => imports.fptsBoolean("Ord", Some("boolOrd"))
      case TsModel.Number(_) => imports.fptsNumber("Ord", Some("numberOrd"))
      case TsModel.Eval(_, tpe) => ordInstance(tpe)
      case TsModel.Array(_, tpe, _) => imports.fptsReadonlyArray("getOrd(" |+| ordInstance(tpe) |+| ")")
      case TsModel.Option(_, tpe) => imports.fptsOption("Ord(" |+| ordInstance(tpe) |+| ")")
      case u @ TsModel.Union(typeName, _, _) =>
        if (unionHasOrd(u)) imports.custom(typeName, unionOrdName(typeName.base)) else err
      case u @ TsModel.UnionRef(typeName, _, _) =>
        if (unionHasOrd(u)) imports.custom(typeName, unionOrdName(typeName.base)) else err

      case (
        TsModel.TypeParam(_)
        | TsModel.Json(_)
        | TsModel.BigNumber(_)
        | TsModel.LocalDate(_)
        | TsModel.DateTime(_)
        | TsModel.UUID(_)
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
        | TsModel.UnionTypeRef(_, _, _)
        | TsModel.TypeAlias(_, _)
        | TsModel.Unknown(_, _)
      ) => err
    })
  }

  /** Produces code that refers to a given codec type, represented by its `typeName` and `typeArgs` */
  private def generateCodecTypeRef(typeName: TypeName, typeArgs: List[TsModel], isUnion: Boolean): Generated =
    customType.codecType(typeName.raw).getOrElse(
      imports.custom(typeName, cap(mkCodecName(typeName, isUnion))) |+|
      (if (typeArgs.isEmpty) Generated.empty
      else "<" |+| typeArgs.intercalateMap(imports.lift(", "))(generateCodecType) |+| ">")
    )

  /** Produces code that refers to a given codec type, represented by its `typeName` and `typeArgs` */
  protected def generateValueTypeRef(typeName: TypeName, typeArgs: List[TsModel], isUnion: Boolean): Generated =
    customType.valueType(typeName.raw).getOrElse(
      imports.custom(typeName, cap(mkValueName(typeName, isUnion))) |+|
      (if (typeArgs.isEmpty) Generated.empty
      else "<" |+| typeArgs.intercalateMap(imports.lift(", "))(generateValueType) |+| ">")
    )

  /** Produces code that refers to a given value, represented by its `typeName` and `typeArgs` */
  private def generateCodecValueRef(state: State, typeName: TypeName, typeArgs: List[TsModel], isUnion: Boolean): List[(Option[TypeName], Generated)] =
    maybeWrapCodec(
      state,
      typeName,
      customType.value(typeName.raw).getOrElse(
        imports.custom(typeName, mkCodecName(typeName, isUnion)) |+|
        (if (typeArgs.isEmpty) Generated.empty
        else "(" |+| typeArgs.intercalateMap(imports.lift(", "))(genNotTop(state, _)) |+| ")")
      ),
    )

  protected def tagField(tag: String): TsModel.ObjectField =
    TsModel.ObjectField("_tag", TsModel.Literal(TsModel.String(TypeName("String")), tag), tag)

  private def generateFields0(
    wrap: Generated => Generated,
    fields: List[TsModel.Field],
    gen: TsModel => Generated,
  ): Generated =
    wrap(
      "{\n" |+|
      fields.intercalateMap(imports.lift(",\n"))(field => "  " |+| field.name |+| ": " |+| gen(field.tpe)) |+|
      "\n}"
    )

  /** Produces codec type code for a type with a given set of fields */
  private def generateFieldsCodecType(fields: List[TsModel.Field]): Generated =
    generateFields0(imports.iotsTypeTypeC(_), fields, generateCodecType)

  /** Produces value type code for a type with a given set of fields */
  private def generateFieldsValueType(fields: List[TsModel.Field]): Generated =
    generateFields0(identity, fields, generateValueType)

  /** Produces value code for a type with a given set of fields */
  private def generateFieldsCodecValue(state: State, fields: List[TsModel.Field]): Generated =
    generateFields0(imports.iotsTypeFunction(_), fields, genNotTop(state, _))

  private def objectFields(obj: TsModel.Object): (List[TsModel.ObjectField], List[TsModel.ObjectField]) =
    (tagField(obj.typeName.base) :: obj.fields).pipe(fs =>
      (fs, fs.map(f => TsModel.ObjectField(f.name, TsModel.Literal(f.tpe, f.value), f.value)))
    )

  /** Produces type code for a scala `object` definition, represented as a `const` in TypeScript */
  private def generateObjectCodecType(obj: TsModel.Object): Generated =
    obj.parent.fold(generateFieldsCodecType(objectFields(obj)._2)) { _ =>
      val valueType = cap(maybeDecap(obj.typeName.base))
      val taggedValueType = valueType + "Tagged"

      imports.iotsTypeType(valueType, taggedValueType)
    }

  /** Produces type code for a scala `object` definition, represented as a `const` in TypeScript */
  private def generateObjectValueType(obj: TsModel.Object): Generated = generateFieldsValueType(objectFields(obj)._2)

  /** Produces value code for a scala `object` definition, represented as a `const` in TypeScript */
  private def generateObjectCodecValue(state: State, obj: TsModel.Object): List[(Option[TypeName], Generated)] = {
    val name = obj.typeName.base
    val constName = maybeDecap(name)
    val valueType = cap(constName)
    val taggedCodecName = constName + "TaggedC"
    val taggedValueType = cap(taggedCodecName).stripSuffix("C")
    val (fields, literalFields) = objectFields(obj)
    
    debugLog("object", name, None)

    lazy val fullCodec: Generated = state.wrapCodec(generateFieldsCodecValue(state, literalFields))

    lazy val minimalTaggedFields = List(tagField(name))

    lazy val minimalTaggedCodec: Generated =
      // Tagged codec type
      "export type " |+| cap(taggedCodecName) |+| " = " |+| generateFieldsCodecType(minimalTaggedFields) |+| ";\n" |+|
      // Codec with only `_tag` value
      "export const " |+| taggedCodecName |+| ": " |+| cap(taggedCodecName) |+| " = " |+|
      generateFieldsCodecValue(state, minimalTaggedFields) |+|
      ";\n" |+|
      // Tagged value type
      "export type " |+| taggedValueType |+| " = " |+| imports.iotsTypeOf(cap(taggedCodecName)) |+| ";\n" |+|
      // Full codec type
      state.wrapCodec.custom(imports.fptsPipe(
        taggedCodecName |+| ", c => new " |+|
        imports.iotsTypeType(valueType |+| ", " |+| taggedValueType) |+| "(\n" |+|
        "  `" |+| name |+| "`,\n" |+|
        "  (u: unknown): u is " |+| valueType |+| " => " |+| imports.fptsEither("isRight(c.decode(u)),\n") |+|
        "  (u: unknown): " |+| imports.fptsEither("Either<") |+| imports.iotsErrors |+| ", " |+| valueType |+| "> => " |+|
        imports.fptsPipe("c.decode(u), " |+| imports.fptsEither("map(x => ({ ...x, ..." |+| constName |+| " }))")) |+|
        ",\n" |+|
        "  (x: " |+| valueType |+| "): " |+| taggedValueType |+| " => ({ ...x, _tag: `" |+| name |+| "`}),\n" |+|
        ")"
      ))(codecType => _ /* we create our own value type */ => codec =>
         // Full value type
         "export type " |+| valueType |+| " = " |+| taggedValueType |+| " & typeof " |+| constName |+| ";\n" |+|
         codecType |+|
         codec
      )

    val codec =
      // Const with all values
      "export const " |+| constName |+| " = {\n" |+|
      fields.intercalateMap(imports.lift(",\n")) {
        case TsModel.ObjectField(name, tpe, value) => s"  $name: " |+| tsValue(tpe, value)
      } |+|
      "\n} as const;\n\n" |+|
      obj.parent.fold(fullCodec)(_ => minimalTaggedCodec)

    List((Some(obj.typeName), codec))
  }

  private def interfaceFields(iface: TsModel.Interface): List[TsModel.Field] =
    iface.parent.fold(Nil)(_ => List(tagField(iface.typeName.base))) ++ iface.fields

  /** Produces codec type code for a scala `case class` definition */
  private def generateInterfaceCodecType(iface: TsModel.Interface): Generated =
    generateFieldsCodecType(interfaceFields(iface))

  /** Produces value type code for a scala `case class` definition */
  private def generateInterfaceValueType(iface: TsModel.Interface): Generated =
    generateFieldsValueType(interfaceFields(iface))

  /** Produces value code for a scala `case class` definition */
  private def generateInterfaceCodecValue(state: State, iface: TsModel.Interface): List[(Option[TypeName], Generated)] = {
    debugLog("interface", iface.typeName.base, None)

    List((Some(iface.typeName), state.wrapCodec(generateFieldsCodecValue(state, interfaceFields(iface)))))
  }

  private def withUnionPossibilityRefs[A](union: TsModel.Union | TsModel.UnionTypeRef)(
    onOne: TsModel => A,
    onMany: List[TsModel] => A
  ): A = {
    val (typeName, possibilities) = union match {
      case u: TsModel.Union => (u.typeName, u.possibilities)
      case u: TsModel.UnionTypeRef => (u.typeName, u.possibilities)
    }
    possibilities.map {
      case TsModel.Interface(typeName, _, typeArgs, _) => TsModel.InterfaceRef(typeName, typeArgs)
      case TsModel.Object(typeName, _, _) => TsModel.ObjectRef(typeName)
      case m => m
    } match {
      case Nil => sys.error(s"Can't generate TS code for union `$typeName` with 0 members")
      case h :: Nil => onOne(h)
      case l @ (_ :: _) => onMany(l)
    }
  }

  /** Produces codec type code for a scala `enum`/`sealed trait`/`sealed class` */
  private def generateUnionCodecType(union: TsModel.Union | TsModel.UnionTypeRef): Generated =
    withUnionPossibilityRefs(union)(
      generateCodecType,
      l => imports.iotsUnionC("[" |+| l.intercalateMap(imports.lift(", "))(generateCodecType) |+| "]")
    )

  /** Produces value type code for a scala `enum`/`sealed trait`/`sealed class` */
  private def generateUnionValueType(union: TsModel.Union | TsModel.UnionTypeRef): Generated =
    withUnionPossibilityRefs(union)(
      generateValueType,
      _.toList.intercalateMap(imports.lift(" | "))(generateValueType)
    )

  /** Produces value code for a scala `enum`/`sealed trait`/`sealed class` */
  private def generateUnionCodecValue(state: State, union: TsModel.Union): List[(Option[TypeName], Generated)] = {
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
      else name |+| "(" |+| tas.intercalateMap(imports.lift(", "))(genNotTop(state, _)) |+| ")"
    }

    debugLog("union", typeName.base, Some(memberCodecNames.map( name => f"\n  member ${name}").mkString))

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
      typeArgsFnParams(state.copy(top = false), typeArgs, None) |+|
      allMemberCodecsArr |+| " as const;\n" |+|
      "export const " |+| allNamesConstName |+| " = [" |+| memberNames.intercalateMap(", ")(s => s"`$s`") |+| "] as const;\n" |+|
      "export type " |+| valueType |+| "Name = (typeof " |+| allNamesConstName |+| ")[number];\n\n" |+|
      state.wrapCodec(allMemberCodecs match {
        case Nil => sys.error(s"Can't generate TS code for union `$typeName` with 0 members")
        case h :: Nil => h
        case _ :: _ => imports.iotsUnion(allMemberCodecsArr)
      }) |+|
      "\n" |+|
      (if (unionHasOrd(union)) ordInst else Generated.empty) |+|
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

  private def generateUnionTypeRefCodecValue(state: State, union: TsModel.UnionTypeRef): Generated =
    withUnionPossibilityRefs(union)(
      genNotTop(state, _),
      l => imports.iotsUnion("[" |+| l.intercalateMap(imports.lift(", "))(genNotTop(state, _)) |+| "]")
    )

  def generateCodecType(model: TsModel): Generated =
    model match {
      case t @ TsModel.TypeParam(name) =>
        name

      case t @ TsModel.Literal(tpe, value) =>
        imports.iotsLiteralC(tsValue(tpe, value))

      case TsModel.Json(_) =>
        imports.iotsUnknownC

      case TsModel.Number(_) =>
        imports.iotsNumberC

      case TsModel.BigNumber(_) =>
        imports.iotsBigNumberType

      case TsModel.Boolean(_) =>
        imports.iotsBooleanC

      case TsModel.String(_) =>
        imports.iotsStringC

      case TsModel.LocalDate(_) =>
        imports.iotsLocalDateType

      case TsModel.DateTime(_) =>
        imports.iotsDateTimeType

      case TsModel.UUID(_) =>
        imports.iotsUUIDType

      case TsModel.Eval(_, tpe) =>
        generateCodecType(tpe)

      case TsModel.Array(_, tpe, _) =>
        imports.iotsReadonlyArrayC(generateCodecType(tpe))

      case TsModel.Set(_, tpe) =>
        imports.iotsReadonlySetFromArrayType(generateCodecType(tpe))

      case TsModel.NonEmptyArray(_, tpe, _) =>
        imports.iotsReadonlyNonEmptyArrayType(generateCodecType(tpe))

      case TsModel.Option(_, tpe) =>
        imports.iotsOptionType(generateCodecType(tpe))

      case TsModel.Either(_, left, right, _) =>
        imports.iotsEitherType(generateCodecType(left), generateCodecType(right))

      case TsModel.Ior(_, left, right, _) =>
        imports.iotsTheseType(generateCodecType(left), generateCodecType(right))

      case TsModel.Map(_, key, value) =>
        recordCodecType(key, value)

      case TsModel.Tuple(_, tpes) =>
        imports.iotsTupleC("[" |+| tpes.intercalateMap(imports.lift(", "))(generateCodecType) |+| "]")

      case i: TsModel.Interface =>
        generateInterfaceCodecType(i)

      case TsModel.InterfaceRef(typeName, typeArgs) =>
        generateCodecTypeRef(typeName, typeArgs, false)

      case o: TsModel.Object =>
        generateObjectCodecType(o)

      case TsModel.ObjectRef(typeName) =>
        generateCodecTypeRef(typeName, Nil, false)

      case u: TsModel.Union =>
        generateUnionCodecType(u)

      case TsModel.UnionRef(typeName, typeArgs, _) =>
        generateCodecTypeRef(typeName, typeArgs, true)

      case u: TsModel.UnionTypeRef =>
        generateUnionCodecType(u)

      case TsModel.TypeAlias(_, dealiased) =>
        generateCodecType(dealiased)

      case TsModel.Unknown(typeName, typeArgs) =>
        generateCodecTypeRef(typeName, typeArgs, false)
    }

  def generateValueType(model: TsModel): Generated =
    model match {
      case t @ TsModel.TypeParam(name) =>
        name

      case t @ TsModel.Literal(tpe, value) =>
        tsValue(tpe, value)

      case TsModel.Json(_) =>
        imports.unknownType

      case TsModel.Number(_) =>
        imports.numberType

      case TsModel.BigNumber(_) =>
        imports.bigNumberType

      case TsModel.Boolean(_) =>
        imports.booleanType

      case TsModel.String(_) =>
        imports.stringType

      case TsModel.LocalDate(_) =>
        imports.localDateType

      case TsModel.DateTime(_) =>
        imports.dateTimeType

      case TsModel.UUID(_) =>
        imports.uuidType

      case TsModel.Eval(_, tpe) =>
        generateValueType(tpe)

      case TsModel.Array(_, tpe, _) =>
        imports.readonlyArrayType(generateValueType(tpe))

      case TsModel.Set(_, tpe) =>
        imports.readonlySetType(generateValueType(tpe))

      case TsModel.NonEmptyArray(_, tpe, _) =>
        imports.readonlyNonEmptyArrayType(generateValueType(tpe))

      case TsModel.Option(_, tpe) =>
        imports.optionType(generateValueType(tpe))

      case TsModel.Either(_, left, right, _) =>
        imports.eitherType(generateValueType(left), generateValueType(right))

      case TsModel.Ior(_, left, right, _) =>
        imports.theseType(generateValueType(left), generateValueType(right))

      case TsModel.Map(_, key, value) =>
        recordValueType(key, value)

      case TsModel.Tuple(_, tpes) =>
        "[" |+| tpes.intercalateMap(imports.lift(", "))(generateValueType) |+| "]"

      case i: TsModel.Interface =>
        generateInterfaceValueType(i)

      case TsModel.InterfaceRef(typeName, typeArgs) =>
        generateValueTypeRef(typeName, typeArgs, false)

      case o: TsModel.Object =>
        generateObjectValueType(o)

      case TsModel.ObjectRef(typeName) =>
        generateValueTypeRef(typeName, Nil, false)

      case u: TsModel.Union =>
        generateUnionValueType(u)

      case TsModel.UnionRef(typeName, typeArgs, _) =>
        generateValueTypeRef(typeName, typeArgs, true)

      case u: TsModel.UnionTypeRef =>
        generateUnionValueType(u)

      case TsModel.TypeAlias(_, dealiased) =>
        generateValueType(dealiased)

      case TsModel.Unknown(typeName, typeArgs) =>
        generateValueTypeRef(typeName, typeArgs, false)
    }

  private def genNotTop(state: State, model: TsModel): Generated =
    generateCodecValue(state.copy(top = false), model).foldMap(_._2)

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
  def generateCodecValue(state: State, model: TsModel): List[(Option[TypeName], Generated)] =
    model match {
      case t @ TsModel.TypeParam(name) =>
        maybeWrapCodec(state, t.typeName, name)

      case t @ TsModel.Literal(tpe, value) =>
        maybeWrapCodec(state, t.typeName, imports.iotsLiteral(tsValue(tpe, value)))

      case TsModel.Json(typeName) =>
        maybeWrapCodec(state, typeName, imports.iotsUnknown)

      case TsModel.Number(typeName) =>
        maybeWrapCodec(state, typeName, imports.iotsNumber)

      case TsModel.BigNumber(typeName) =>
        maybeWrapCodec(state, typeName, imports.iotsBigNumberValue)

      case TsModel.Boolean(typeName) =>
        maybeWrapCodec(state, typeName, imports.iotsBoolean)

      case TsModel.String(typeName) =>
        maybeWrapCodec(state, typeName, imports.iotsString)

      case TsModel.LocalDate(typeName) =>
        maybeWrapCodec(state, typeName, imports.iotsLocalDateValue)

      case TsModel.DateTime(typeName) =>
        maybeWrapCodec(state, typeName, imports.iotsDateTimeValue)

      case TsModel.UUID(typeName) =>
        maybeWrapCodec(state, typeName, imports.iotsUUIDValue)

      case TsModel.Eval(_, tpe) =>
        generateCodecValue(state, tpe)

      case TsModel.Array(typeName, tpe, _) =>
        maybeWrapCodec(state, typeName, imports.iotsReadonlyArray(genNotTop(state, tpe)))

      case TsModel.Set(typeName, tpe) =>
        maybeWrapCodec(state, typeName, imports.iotsReadonlySetFromArrayValue(genNotTop(state, tpe), ordInstance(tpe)))

      case TsModel.NonEmptyArray(typeName, tpe, _) =>
        maybeWrapCodec(state, typeName, imports.iotsReadonlyNonEmptyArrayValue(genNotTop(state, tpe)))

      case TsModel.Option(typeName, tpe) =>
        maybeWrapCodec(state, typeName, imports.iotsOptionValue(genNotTop(state, tpe)))

      case TsModel.Either(typeName, left, right, _) =>
        maybeWrapCodec(state, typeName, imports.iotsEitherValue(genNotTop(state, left), genNotTop(state, right)))

      case TsModel.Ior(typeName, left, right, _) =>
        maybeWrapCodec(state, typeName, imports.iotsTheseValue(genNotTop(state, left), genNotTop(state, right)))

      case TsModel.Map(typeName, key, value) =>
        maybeWrapCodec(state, typeName, recordCodecValue(state.copy(top = false), key, value))

      case TsModel.Tuple(typeName, tpes) =>
        maybeWrapCodec(state, typeName, imports.iotsTuple("[" |+|
          tpes.intercalateMap(imports.lift(", "))(genNotTop(state, _)) |+|
        "]"))

      case i: TsModel.Interface =>
        if (state.top) generateInterfaceCodecValue(state, i) else generateCodecValueRef(state, i.typeName, i.typeArgs, false)

      case TsModel.InterfaceRef(typeName, typeArgs) =>
        generateCodecValueRef(state, typeName, typeArgs, false)

      case o: TsModel.Object =>
        if (state.top) generateObjectCodecValue(state, o) else generateCodecValueRef(state, o.typeName, Nil, false)

      case TsModel.ObjectRef(typeName) =>
        generateCodecValueRef(state, typeName, Nil, false)

      case u: TsModel.Union =>
        if (state.top) generateUnionCodecValue(state, u) else generateCodecValueRef(state, u.typeName, u.typeArgs, true)

      case TsModel.UnionRef(typeName, typeArgs, _) =>
        generateCodecValueRef(state, typeName, typeArgs, true)

      case u: TsModel.UnionTypeRef =>
        List((None, generateUnionTypeRefCodecValue(state, u)))

      case TsModel.TypeAlias(typeName, dealiased) =>
        maybeWrapCodec(state, typeName, genNotTop(state, dealiased))

      case TsModel.Unknown(typeName, typeArgs) =>
        generateCodecValueRef(state, typeName, typeArgs, false)
    }

  /**
   * Produces TypeScript code for a top-level definition of a given type
   *
   * @param model The [[scalats.TsModel]] to generate TypeScript code for
   * @return A `List` of the [[scalats.TypeName]]s and [[scalats.Generated]] code produced
   */
  def generateTopLevel(model: TsModel): List[(Option[TypeName], Generated)] = {
    val isUnion = model match {
      case _: TsModel.Union => true
      case _ => false
    }
    val codecName = mkCodecName(model.typeName, isUnion)
    val codecType = cap(codecName)
    val valueType = mkValueName(model.typeName, isUnion)
    val hasTypeArgs = model match {
      case _: TsModel.Interface | _: TsModel.Object | _: TsModel.Union => model.typeArgs.nonEmpty
      case _ => false
    }

    if (hasTypeArgs) {
      val updState = State(false, WrapCodec.id)
      val tpArgsPlain = "<" |+| model.typeArgs.intercalateMap(imports.lift(", "))(genNotTop(updState, _)) |+| ">"
      val tpArgsIots = typeArgsMixed(updState, model.typeArgs)
      val fnArgs = typeArgsFnParams(updState, model.typeArgs, Some(codecType |+| tpArgsPlain))
      generateCodecValue(State(true, WrapCodec(codec => f =>
        f(
          "export type " |+| codecType |+| tpArgsIots |+| " = " |+| generateCodecType(model) |+| ";\n"
        )(
          "export type " |+| valueType |+| tpArgsPlain |+| " = " |+| generateValueType(model) |+| ";\n"
        )(
          "export const " |+| codecName |+| " = " |+| tpArgsIots |+| fnArgs |+| codec |+| " satisfies " |+|
          imports.iotsTypeType(
            valueType |+| "<" |+|
              model.typeArgs.intercalateMap(imports.lift(", "))(t => imports.iotsTypeOf(generateValueType(t))) |+|
            ">",
            imports.unknownType
          ) |+|
          ";\n"
        )
      )), model)
    } else
      generateCodecValue(State(true, WrapCodec(codec => f =>
        f(
          "export type " |+| codecType |+| " = " |+| generateCodecType(model) |+| ";\n"
        )(
          "export type " |+| valueType |+| " = " |+| generateValueType(model) |+| ";\n"
        )(
          "export const " |+| codecName |+| ": " |+| codecType |+| " = " |+| codec |+| " satisfies " |+|
          imports.iotsTypeType(valueType, imports.unknownType) |+|
          ";\n"
        )
      )), model)
  }
}
