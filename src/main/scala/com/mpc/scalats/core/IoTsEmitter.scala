package com.mpc.scalats.core

import cats.syntax.foldable._
import com.mpc.scalats.configuration.Config
import scala.collection.immutable.ListSet
import scala.reflect.runtime.universe.Type

final class IoTsEmitter(val config: Config) extends Emitter {
  import TypeScriptModel._
  import Internals.list

  def typeAsValArg(s: String): String = s"_${s}val"

  def emit(declarations: ListSet[Declaration])(implicit ctx: TsImports.Ctx): Lines =
    declarations.toList.foldMap {
      case decl: InterfaceDeclaration =>
        emitIoTsInterfaceDeclaration(decl)
      case UnionDeclaration(name, members, possibilities, superInterface, emitAll) =>
        emitUnionDeclaration(name, members, possibilities, superInterface, emitAll)
      case SingletonDeclaration(name, members, superInterface) =>
        emitSingletonDeclaration(name, members, superInterface)
      case decl: TypeDeclaration =>
        emitTypeDeclaration(decl)
    }

  private def iotsTypeParamArgs(typeParams: List[String]): TsImports.With[String] =
    if (typeParams.isEmpty) (imports.empty, "") else
      typeParams.joinTypeParams(p => s"$p extends " |+| imports.iotsMixed) |+|
      typeParams.joinParens(", ")(p => s"${typeAsValArg(p)}: $p") |+| " => "

  private def interfaceTypeParamArgs(typeParams: List[String]): TsImports.With[String] =
    if (typeParams.isEmpty) (imports.empty, "") else typeParams.joinTypeParams(p => p)

  def emitIoTsInterfaceDeclaration(decl: InterfaceDeclaration)(implicit ctx: TsImports.Ctx): Lines = {
    val InterfaceDeclaration(name, fields, typeParams, superInterface) = decl
    val typeVal = codecName(name)
    val typeType = codecType(name)

    imports.iotsTypeFunction.lines(
      s => s"export const $typeVal = " |+| iotsTypeParamArgs(list(typeParams)) |+| s"$s{",
      fields.joinLines(",")(v => s"${indent}${v.name}: " |+| getIoTsTypeString(v.typeRef)),
      "}" ++ _ ++ ";"
    ) |+|
    (
      if (typeParams.isEmpty) {
        line(s"export type $typeType = typeof $typeVal;")
      } else {
        line() |+|
        line(s"export interface ${interfaceName(name)}" |+| interfaceTypeParamArgs(list(typeParams)) |+| s"${superInterface.fold("")(i => s" extends ${i.name}")} {") |+|
        emitMembers(fields, true, false) |+|
        line("}") |+|
        line(s"export type $typeType" |+| typeParams.joinTypeParams(t => s"$t extends " |+| imports.iotsTypeTypeC |+| "<any>") |+| s" = ${interfaceName(name)}<" |+| imports.iotsTypeOf |+| s"<${typeParams.mkString(", ")}>>;")
      }
    ) |+|
    (if (typeParams.isEmpty) line(s"export type $name = " |+| imports.iotsTypeOf |+| s"<$typeType>;") else Nil) |+|
    List("")
  }

  def emitInterfaceDeclaration(name: String, members: ListSet[Member], superInterface: Option[InterfaceDeclaration])(implicit ctx: TsImports.Ctx): Lines =
    if (!config.emitInterfaces) Nil else
      line(s"export interface ${interfaceName(name)}${superInterface.fold("")(i => s" extends ${i.name}")} {") |+|
      emitMembers(members, true, false) |+|
      line("}") |+|
      line()

  private def tsUnionName(name: String): String = s"${codecType(name)}U"

  private def emitUnionDeclaration(
    name: String,
    members: ListSet[Member],
    possibilities: ListSet[CustomTypeRef],
    superInterface: Option[InterfaceDeclaration],
    emitAll: Boolean
  )(implicit ctx: TsImports.Ctx): Lines = {
    def union(nameFn: String => String, prefix: String, glue: String, suffix: String) =
      possibilities.join(prefix, glue, suffix)(p => nameFn(p.name))

    val codecs = union(codecName, "[", ", ", "]")

    line(s"export const all${name}C = " |+| codecs |+| " as const;") |+|
    line(s"export const ${tsUnionName(name)} = " |+| (
      if (possibilities.size == 1) codecName(possibilities.head.name)
      else if (possibilities.size == 0) sys.error(s"Can't create $name union composed of 0 members")
      else imports.iotsUnion(codecs)) |+| ";") |+|
    (if (emitAll) line(s"export const all$name = " |+| union(objectName, "[", ", ", "]") |+| " as const;") else Nil) |+|
    line(s"export type ${name}U = " |+| imports.iotsTypeOf |+| s"<typeof ${tsUnionName(name)}>;") |+|
    line() |+|
    emitInterfaceDeclaration(name, members, superInterface)
  }

  private def isAbstractMember(member: Member, superInterface: Option[InterfaceDeclaration]): Boolean = {
    val Member(n, t, _) = member
    superInterface.exists(_.fields.collectFirst { case Member(`n`, `t`, None) => () }.isDefined)
  }

  private def singletonIdentifier(name: String, members: ListSet[Member], superInterface: Option[InterfaceDeclaration]): Member =
    members.collectFirst {
      // The member is an abstract `val id: Int` on the parent
      case m @ Member("id", NumberRef, _) if isAbstractMember(m, superInterface) => m

      // The member is the only abstract `val` on the parent
      case m if superInterface.exists(_.fields.size == 1) && isAbstractMember(m, superInterface) => m
    }.getOrElse(sys.error(s"""
      |Can't choose identifier for singleton type
      |
      |const ${name}${superInterface.fold("")(i => s": ${i.name}")} = {
      |  ${members.map(m => s"${m.name}: ${m.typeRef}${m.value.fold("")(v => s" = $v")}").mkString(",\n  ")}
      |}
      |
      |Make sure the `sealed trait` interface has either
      |
      |  1. An abstract `val id: Int` member
      |  2. A single abstract `val` member, expected to be a unique identifier
      |""".stripMargin))

  private def emitSingletonDeclaration(
    name: String,
    members: ListSet[Member],
    superInterface: Option[InterfaceDeclaration]
  )(implicit ctx: TsImports.Ctx): Lines = {
    val (objName, ifaceName, cName, partialCName) =
      (objectName(name), interfaceName(name), codecName(name), codecName(s"${name}Partial"))

    val identifierMember = singletonIdentifier(name, members, superInterface)

    line(s"export const $objName = {") |+|
    emitMembers(members, false, false) |+|
    line("} as const;") |+|
    line() |+|
    line(s"export type $ifaceName = typeof $objName;") |+|
    line() |+|
    imports.iotsTypeFunction.lines(
      s => s"export const $partialCName = $s{",
      emitMembers(ListSet(identifierMember), false, true),
      "}" ++ _ ++ ";"
    ) |+|
    line(s"export const $cName = new " |+| imports.iotsTypeType |+| s"<$ifaceName>(") |+|
    line(s"""  "${superInterface.fold("")(i => s"${i.name} ")}$ifaceName",""") |+|
    line(s"  (u: unknown): u is $ifaceName => " |+| imports.fptsEither(s"isRight($partialCName.decode(u)),")) |+|
    line("  (u: unknown): " |+| imports.fptsEither("Either") |+| "<" |+| imports.iotsErrors |+|
      s", $ifaceName> => " |+| imports.fptsPipe(s"$partialCName.decode(u), " |+|
        imports.fptsEither(s"map(() => $objName)")) |+| ",") |+|
    line("  t.identity") |+|
    line(");")
  }

  private def emitTypeDeclaration(decl: TypeDeclaration)(implicit ctx: TsImports.Ctx): Lines = {
    val TypeDeclaration(name, typeRef, typeParams) = decl
    val typeVal = codecName(name)

    line(s"export const $typeVal = " |+| iotsTypeParamArgs(list(typeParams)) |+| getIoTsTypeString(typeRef) |+| ";") |+|
    (if (typeParams.isEmpty) line(s"export type $name = " |+| imports.iotsTypeOf |+| s"<typeof $typeVal>;") else Nil ) |+|
    line()
  }

  def emitMembers(members: ListSet[Member], interfaceContext: Boolean, iotsContext: Boolean)(implicit ctx: TsImports.Ctx): Lines =
    members.joinLines(",")(m => s"${indent}${m.name}: " |+|
      m.value.fold(getTypeRefString(m.typeRef))(v =>
        if (iotsContext) getIoTsTypeWrappedVal(v, m.typeRef)
        else getTypeWrappedVal(v, m.typeRef, interfaceContext)))

  def getIoTsTypeString(typeRef: TypeRef)(implicit ctx: TsImports.Ctx): TsImports.With[String] = typeRef match {
    case NumberRef => imports.iotsNumber
    case BooleanRef => imports.iotsBoolean
    case StringRef => imports.iotsString
    case DateRef => imports.iotsLocalDate
    case DateTimeRef => imports.iotsDateTime
    case ArrayRef(innerType) => imports.iotsReadonlyArray(getIoTsTypeString(innerType))
    case NonEmptyArrayRef(innerType) => imports.iotsReadonlyNonEmptyArray(getIoTsTypeString(innerType))
    case CustomTypeRef(name, params, scalaType) =>
      customIoTsTypes(scalaType, name, codecName) |+|
        (if (params.isEmpty) "" else params.joinParens(", ")(getIoTsTypeString))
    case UnknownTypeRef(unknown) => (imports.empty, unknown)
    case SimpleTypeRef(param) => (imports.empty, typeAsValArg(param))
    case UnionType(name, possibilities, scalaType) =>
      imports.custom(scalaType, tsUnionName(name)).orElse(imports.iotsUnion(possibilities.joinArray(getIoTsTypeString)))
    case OptionType(iT) =>
      imports.iotsOption(getIoTsTypeString(iT))
    case EitherType(lT, rT) =>
      imports.iotsEither(List(lT, rT).join(", ")(getIoTsTypeString))
    case TheseType(lT, rT) =>
      imports.iotsThese(List(lT, rT).join(", ")(getIoTsTypeString))
    case MapType(keyType, valueType) =>
      imports.iotsRecord(getIoTsRecordKeyTypeString(keyType) |+| ", " |+| getIoTsTypeString(valueType))
    case TupleType(types) =>
      imports.iotsTuple(types.joinArray(getIoTsTypeString))
    case NullRef => imports.iotsNull
    case UndefinedRef => imports.iotsUndefined
  }

  def getIoTsRecordKeyTypeString(typeRef: TypeRef)(implicit ctx: TsImports.Ctx): TsImports.With[String] = typeRef match {
    case NumberRef => imports.iotsNumberFromString
    case _ => getIoTsTypeString(typeRef)
  }

  def getIoTsTypeWrappedVal(value: Any, typeRef: TypeRef)(implicit ctx: TsImports.Ctx): TsImports.With[String] = typeRef match {
    case NumberRef => imports.iotsLiteral(value.toString)
    case BooleanRef => imports.iotsLiteral(value.toString)
    case StringRef => imports.iotsLiteral(s"`${value.toString.trim}`")
    case DateRef => imports.iotsLiteral(s"`${value.toString.trim}`")
    case DateTimeRef => imports.iotsLiteral(s"new Date(`${value.toString.trim}`)")
    case ArrayRef(_) => imports.iotsLiteral(s"[$value]")
    case NonEmptyArrayRef(_) => imports.iotsLiteral(imports.iotsReadonlyNonEmptyArray.value |+| s".of([$value])")
    case CustomTypeRef(name, params, scalaType) =>
      customIoTsTypes(scalaType, name, codecName) |+|
        (if (params.isEmpty) "" else params.joinParens(", ")(getIoTsTypeWrappedVal(value, _)))
    case UnknownTypeRef(unknown) => unknown
    case SimpleTypeRef(param) => imports.iotsStrict(typeAsValArg(param))
    case UnionType(name, possibilities, scalaType) =>
      imports.custom(scalaType, name).orElse(imports.iotsStrict(
        imports.iotsUnion(possibilities.joinArray(getIoTsTypeWrappedVal(value, _)))))
    case OptionType(iT) =>
      imports.iotsOption(getIoTsTypeWrappedVal(value, iT))
    case EitherType(lT, rT) =>
      imports.iotsStrict(imports.iotsUnion(
        List(lT, rT).joinArray(getIoTsTypeWrappedVal(value, _))))
    case TheseType(lT, rT) =>
      imports.iotsStrict(imports.iotsUnion(
        List(lT, rT, TupleType(ListSet(lT, rT))).joinArray(getIoTsTypeWrappedVal(value, _))))
    case MapType(keyType, valueType) =>
      imports.iotsStrict(imports.iotsRecord(
        List(keyType, valueType).join(", ")(getIoTsTypeWrappedVal(value, _))))
    case TupleType(types) =>
      imports.iotsStrict(imports.iotsTuple(
        types.joinArray(getIoTsTypeWrappedVal(value, _))))
    case NullRef => imports.iotsLiteral("null")
    case UndefinedRef => imports.iotsLiteral("undefined")
  }

  def customIoTsTypes(scalaType: Type, name: String, customName: String => String)(implicit ctx: TsImports.Ctx): TsImports.With[String] = name match {
    case "NonEmptyList" => imports.iotsReadonlyNonEmptyArray.value
    case _ => imports.custom(scalaType, customName(name))
  }

  def getTypeWrappedVal(value: Any, typeRef: TypeRef, interfaceContext: Boolean)(implicit ctx: TsImports.Ctx): TsImports.With[String] = typeRef match {
    case NumberRef => value.toString
    case BooleanRef => value.toString
    case StringRef => s"`${value.toString.trim}`"
    case DateRef => s"`${value.toString.trim}`"
    case DateTimeRef => s"`${value.toString.trim}`"
    case ArrayRef(t) => value.asInstanceOf[Iterable[Any]].joinArray(getTypeWrappedVal(_, t, interfaceContext))
    case NonEmptyArrayRef(t) =>
      imports.iotsReadonlyNonEmptyArray.value |+| ".of(" |+| getTypeWrappedVal(value, ArrayRef(t), interfaceContext) |+| ")"
    case CustomTypeRef(name, params, scalaType) =>
      imports.custom(scalaType, if (interfaceContext) interfaceName(name) else objectName(name)) |+|
        (if (params.isEmpty) "" else params.joinParens(", ")(getIoTsTypeString))
    case UnknownTypeRef(unknown) => unknown
    case SimpleTypeRef(param) => if (interfaceContext) param else typeAsValArg(param)
    case u @ UnionType(_, possibilities, _) =>
      val valueType = scala.reflect.runtime.currentMirror.classSymbol(value.getClass).toType
      val (customTypeName, customType) = possibilities.collectFirst {
        case CustomTypeRef(n, _, t) if t =:= valueType => (n, t)
      }.getOrElse(sys.error(s"Value $value of type $valueType is not a member of union $u"))
      imports.custom(customType, objectName(customTypeName))
    case OptionType(iT) => getIoTsTypeString(iT)
    case EitherType(lT, rT) => imports.iotsUnion(List(lT, rT).joinArray(getIoTsTypeString))
    case TheseType(lT, rT) => imports.iotsUnion(List(lT, rT, TupleType(ListSet(lT, rT))).joinArray(getIoTsTypeString))
    case MapType(keyType, valueType) => imports.iotsRecord(List(keyType, valueType).join(", ")(getIoTsTypeString))
    case TupleType(types) => imports.iotsTuple(types.joinArray(getIoTsTypeString))
    case NullRef => "null"
    case UndefinedRef => "undefined"
  }
}
