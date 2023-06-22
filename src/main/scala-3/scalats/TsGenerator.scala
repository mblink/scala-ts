package scalats

import cats.Monoid
import cats.syntax.foldable.*
import scala.language.implicitConversions
import scala.util.chaining.*

class TsGenerator(customType: TsCustomType, imports: TsImports.Available) {
  private def cap(s: String): String = s.take(1).toUpperCase + s.drop(1)
  private def decap(s: String): String = s.take(1).toLowerCase + s.drop(1)
  private val allCapsRx = """^[A-Z0-9_]+$""".r
  private def maybeDecap(s: String): String =
    s match {
      case allCapsRx() => s
      case _ => decap(s)
    }

  case class GenState(top: Boolean, wrapCodec: Generated => Generated)

  private implicit def liftString(s: String): Generated = imports.lift(s)

  private def mkCodecName(typeName: TypeName, isUnion: Boolean): String =
    if (isUnion) typeName.base ++ "CU"
    else maybeDecap(typeName.base) ++ "C"

  private def maybeWrapCodec(state: GenState, typeName: TypeName, codec: Generated): List[(Option[TypeName], Generated)] =
    List(if (state.top) (Some(typeName), state.wrapCodec(codec)) else (None, codec))

  private def typeArgsMixed(state: GenState, typeArgs: List[TsModel]): Generated =
    if (typeArgs.isEmpty) Generated.empty
    else "<" |+| typeArgs.intercalateMap(imports.lift(", "))(
      generate(state, _).foldMap(_._2) |+| " extends " |+| imports.iotsMixed
    ) |+| ">"

  private def typeArgsFnParams(state: GenState, typeArgs: List[TsModel]): Generated =
    if (typeArgs.isEmpty) Generated.empty
    else "(" |+| typeArgs.intercalateMap(imports.lift(", "))(
      generate(state, _).foldMap(_._2).pipe(x => x |+| ": " |+| x)
    ) |+| ") => "

  private def recordKeyType(state: GenState, tpe: TsModel): Generated =
    tpe match {
      case TsModel.Number(_) => imports.iotsNumberFromString
      case _ => generate(state, tpe).foldMap(_._2)
    }

  private def tsValue(tpe: TsModel, value: Any): Generated =
    tpe match {
      case TsModel.TypeParam(_) => sys.error(s"Encountered unexpected type parameter in `tsValue`, value: $value")
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

  private def ordInstance(tpe: TsModel): Generated = {
    lazy val err = sys.error(s"`Ord` instance requested for ${tpe.typeName.full} but not found")

    tpe match {
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

  private def generateTypeRef(state: GenState, typeName: TypeName, typeArgs: List[TsModel], isUnion: Boolean): List[(Option[TypeName], Generated)] =
    maybeWrapCodec(state, typeName, customType(typeName.raw).getOrElse(
      imports.custom(typeName, mkCodecName(typeName, isUnion)) |+|
      (if (typeArgs.isEmpty) Generated.empty
      else "(" |+| typeArgs.intercalateMap(imports.lift(", "))(generate(state.copy(top = false), _).foldMap(_._2)) |+| ")")
    ))

  private def generateObject(obj: TsModel.Object): List[(Option[TypeName], Generated)] = {
    val TsModel.Object(typeName, parent, fields) = obj
    val name = typeName.base
    val constName = maybeDecap(name)
    val codecName = constName + "C"
    val valueType = cap(constName)
    val taggedCodecName = constName + "TaggedC"
    val taggedValueType = cap(taggedCodecName).stripSuffix("C")
    val genFields = (fields ++ parent.map(_ => TsModel.ObjectField("_tag", TsModel.String(TypeName("String")), typeName.base)))
      .map { case TsModel.ObjectField(name, tpe, value) => s"  $name: " |+| tsValue(tpe, value) }

    val codec =
      // Const with all values
      "export const " |+| constName |+| " = {\n" |+|
      genFields.intercalate(imports.lift(",\n")) |+|
      "\n} as const;\n\n" |+|
      // Const with only `_tag` value
      "export const " |+| taggedCodecName |+| " = " |+|
      imports.iotsTypeFunction("{ _tag: " |+| imports.iotsLiteral("`" |+| name |+| "`") |+| " }") |+|
      ";\n" |+|
      // Tagged codec type
      "export type " |+| cap(taggedCodecName) |+| " = typeof " |+| taggedCodecName |+| ";\n" |+|
      // Tagged value type
      "export type " |+| taggedValueType |+| " = " |+| imports.iotsTypeOf(cap(taggedCodecName)) |+| ";\n" |+|
      // Full value type
      "export type " |+| valueType |+| " = " |+| taggedValueType |+| " & typeof " |+| constName |+| ";\n" |+|
      // Full codec type
      "export const " |+| codecName |+| " = " |+|
      imports.fptsPipe(
        taggedCodecName |+| ", c => new " |+|
        imports.iotsTypeType |+|
        "<" |+| valueType |+| ", " |+| taggedValueType |+| ">(\n" |+|
        "  `" |+| name |+| "`,\n" |+|
        "  (u: unknown): u is " |+| valueType |+| " => " |+| imports.fptsEither("isRight(c.decode(u)),\n") |+|
        "  (u: unknown): " |+| imports.fptsEither("Either<") |+| imports.iotsErrors |+| ", " |+| valueType |+| "> =>" |+|
        imports.fptsPipe("c.decode(u), " |+| imports.fptsEither("map(x => ({ ...x, ..." |+| constName |+| " }))")) |+|
        ",\n" |+|
        "  (x: " |+| valueType |+| "): " |+| taggedValueType |+| " => ({ ...x, _tag: `" |+| name |+| "`}),\n" |+|
        ")"
      ) |+|
      ";"

    List((Some(typeName), codec))
  }

  private def generateInterface(state: GenState, iface: TsModel.Interface): List[(Option[TypeName], Generated)] = {
    val TsModel.Interface(typeName, parent, typeArgs, fields) = iface
    val genFields = parent.fold(Nil)(_ => List("  _tag: " |+| imports.iotsLiteral("`" |+| typeName.base |+| "`"))) ++
      fields.map { case TsModel.InterfaceField(name, tpe) => "  " |+| name |+| ": " |+| generate(state.copy(top = false), tpe).foldMap(_._2) }

    List((Some(typeName), state.wrapCodec(imports.iotsTypeFunction(
      "{\n" |+|
      genFields.intercalate(imports.lift(",\n")) |+|
      "\n}"
    ))))
  }


  private def generateUnion(state: GenState, union: TsModel.Union): List[(Option[TypeName], Generated)] = {
    val TsModel.Union(typeName, typeArgs, possibilities) = union
    val valueType = typeName.base

    given mb: Monoid[Boolean] = Monoid.instance(true, _ && _)

    val (allConstant, memberNames, memberConstNames, memberCodecNames, memberCodecs) = possibilities.foldMap {
      case i: TsModel.Interface =>
        val name = i.typeName.base
        val codecName = mkCodecName(i.typeName, false)
        val codec = generateTopLevel(i)
        (false, List(name), Nil, List(codecName), List(codec))

      case o: TsModel.Object =>
        val name = o.typeName.base
        val constName = maybeDecap(name)
        val codecName = mkCodecName(o.typeName, false)
        val codec = generateTopLevel(o)
        (true, List(name), List(constName), List(codecName), List(codec))
    }

    val allMemberCodecs =
      if (typeArgs.isEmpty) memberCodecNames.map(imports.lift)
      else memberCodecNames.map(n => n |+| "(" |+| typeArgs.intercalateMap(imports.lift(", "))(generate(state.copy(top = false), _).foldMap(_._2)) |+| ")")
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

  def generate(state: GenState, model: TsModel): List[(Option[TypeName], Generated)] =
    model match {
      case t @ TsModel.TypeParam(name) => maybeWrapCodec(state, t.typeName, name)
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
      case o: TsModel.Object => if (state.top) generateObject(o) else generateTypeRef(state, o.typeName, Nil, false)
      case TsModel.ObjectRef(typeName) => generateTypeRef(state, typeName, Nil, false)
      case u: TsModel.Union => if (state.top) generateUnion(state, u) else generateTypeRef(state, u.typeName, u.typeArgs, true)
      case TsModel.UnionRef(typeName, typeArgs) => generateTypeRef(state, typeName, typeArgs, true)
      case TsModel.Unknown(typeName, typeArgs) => generateTypeRef(state, typeName, typeArgs, false)
    }

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
      val tpArgsPlain = model.typeArgs.intercalateMap(imports.lift(","))(generate(GenState(false, identity), _).foldMap(_._2))
      val tpArgsIots = typeArgsMixed(GenState(false, identity), model.typeArgs)
      val fnArgs = typeArgsFnParams(GenState(false, identity), model.typeArgs)
      generate(GenState(true, codec =>
        "export class " |+| className |+|
        tpArgsIots |+|
        "{ codec = " |+|
        fnArgs |+|
        codec |+|
        "}\n" |+|
        "export const " |+| codecName |+| " = " |+|
        tpArgsIots |+|
        fnArgs |+|
        "new " |+| className |+| "<" |+| tpArgsPlain |+| ">().codec(" |+| tpArgsPlain |+| ");\n" |+|
        "export type " |+| codecType |+| tpArgsIots |+|
        " = ReturnType<" |+| className |+| "<" |+| tpArgsPlain |+| ">[\"codec\"]>;\n" |+|
        "export type " |+| valueType |+| "<" |+| tpArgsPlain |+| "> = " |+|
        imports.iotsTypeOf(
          codecType |+| "<" |+|
          model.typeArgs.intercalateMap(imports.lift(","))(
            t => imports.iotsTypeType |+| "<" |+| generate(GenState(false, identity), t).foldMap(_._2) |+| ">"
          ) |+|
          ">"
        ) |+|
        ";"
      ), model)
    } else
      generate(GenState(true, codec =>
        "export const " |+| codecName |+| " = " |+|
        codec |+|
        ";\n" |+|
        "export type " |+| codecType |+| " = typeof " |+| codecName |+| ";\n" |+|
        "export type " |+| valueType |+| " = " |+| imports.iotsTypeOf(codecType) |+| ";\n"
      ), model)
  }

  private def referencedTypeNamesInterface(iface: TsModel.Interface, currType: TypeName): Map[TypeName, Set[TypeName]] =
    iface.fields.foldMap { case TsModel.InterfaceField(_, tpe) => referencedTypeNames(tpe, currType) }

  private def referencedTypeNamesObject(obj: TsModel.Object, currType: TypeName): Map[TypeName, Set[TypeName]] =
    obj.fields.foldMap { case TsModel.ObjectField(_, tpe, _) => referencedTypeNames(tpe, currType) }

  private def referencedTypeNamesUnion(union: TsModel.Union, currType: TypeName): Map[TypeName, Set[TypeName]] = {
    import cats.syntax.semigroup.*
    union.possibilities.foldMap(x => Map(currType -> Set(x.typeName)) |+| (x match {
      case i: TsModel.Interface => referencedTypeNamesInterface(i, i.typeName)
      case o: TsModel.Object => referencedTypeNamesObject(o, o.typeName)
    }))
  }

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

  def referencedTypes(model: TsModel): ReferencedTypes =
    ReferencedTypes(referencedTypeNames(model, model.typeName))
}
