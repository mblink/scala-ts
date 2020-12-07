package com.mpc.scalats.core

import cats.syntax.foldable._
import com.mpc.scalats.configuration.Config
import scala.collection.immutable.ListSet
import scala.reflect.runtime.universe.Type

final class IoTsEmitter(val config: Config) extends Emitter {
  import TypeScriptModel._
  import Internals.list

  def typeAsValArg(s: String): String = s"_${s}val"

  def emit(declarations: ListSet[Declaration]): Lines =
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

  private def typeParamArgs(typeParams: List[String]): TsImports.With[String] =
    if (typeParams.isEmpty) (imports.empty, "") else
      typeParams.joinTypeParams(p => s"$p extends " |+| imports.iotsMixed) |+|
      typeParams.joinParens(", ")(p => s"${typeAsValArg(p)}: $p") |+| " => "

  def emitIoTsInterfaceDeclaration(decl: InterfaceDeclaration): Lines = {
    val InterfaceDeclaration(name, fields, typeParams, _) = decl
    val typeVal = codecName(name)

    imports.iotsTypeFunction.lines(
      s => s"export const $typeVal = " |+| typeParamArgs(list(typeParams)) |+| s"$s{",
      fields.joinLines(",")(v => s"${indent}${v.name}: " |+| getIoTsTypeString(v.typeRef)),
      "}" ++ _ ++ ";"
    ) |+|
    (if (typeParams.isEmpty) line(s"export type $name = " |+| imports.iotsTypeOf |+| s"<typeof $typeVal>;") else Nil) |+|
    List("")
  }

  def emitInterfaceDeclaration(name: String, members: ListSet[Member], superInterface: Option[InterfaceDeclaration]): Lines =
    if (!config.emitInterfaces) Nil else
      line(s"export interface ${interfaceName(name)}${superInterface.fold("")(i => s" extends ${i.name}")} {") |+|
      emitMembers(members, true, false) |+|
      line("}") |+|
      line()

  private def emitUnionDeclaration(
                                    name: String,
                                    members: ListSet[Member],
                                    possibilities: ListSet[CustomTypeRef],
                                    superInterface: Option[InterfaceDeclaration],
                                    emitAll: Boolean): Lines = {
    def union(nameFn: String => String, prefix: String, glue: String, suffix: String) =
      possibilities.join(prefix, glue, suffix)(p => nameFn(p.name))

    lazy val codecs = union(codecName, "[", ", ", "]")
    lazy val uName = s"${codecType(name)}U"

    line(s"export const all${name}C = " |+| codecs |+| " as const;") |+|
    line(s"export const $uName = " |+| (
      if (possibilities.size == 1) codecName(possibilities.head.name)
      else if (possibilities.size == 0) sys.error(s"Can't create $name union composed of 0 members")
      else imports.iotsUnion(codecs)) |+| ";") |+|
    (if (emitAll) line(s"export const all$name = " |+| union(objectName, "[", ", ", "]") |+| " as const;") else Nil) |+|
    line(s"export type ${name}U = " |+| imports.iotsTypeOf |+| s"<typeof $uName>") |+|
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
                                        superInterface: Option[InterfaceDeclaration]): Lines = {
    val (objName, ifaceName, cName, partialCName) =
      (objectName(name), interfaceName(name), codecName(name), codecName(s"${name}Partial"))

    val identifierMember = singletonIdentifier(name, members, superInterface)

    line(s"export const $objName = {") |+|
    emitMembers(members, false, false) |+|
    line("} as const;") |+|
    line() |+|
    line(s"export type $ifaceName = typeof $objName;") |+|
    line() |+|
    imports.iotsStrict.lines(
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

  private def emitTypeDeclaration(decl: TypeDeclaration): Lines = {
    val TypeDeclaration(name, typeRef, typeParams) = decl
    val typeVal = codecName(name)

    line(s"export const $typeVal = " |+| typeParamArgs(list(typeParams)) |+| getIoTsTypeString(typeRef) |+| ";") |+|
    (if (typeParams.isEmpty) line(s"export type $name = " |+| imports.iotsTypeOf |+| s"<typeof $typeVal>;") else Nil ) |+|
    line()
  }

  def emitMembers(members: ListSet[Member], interfaceContext: Boolean, iotsContext: Boolean): Lines =
    members.joinLines(",")(m => s"${indent}${m.name}: " |+|
      m.value.fold(getTypeRefString(m.typeRef))(v =>
        if (iotsContext) getIoTsTypeWrappedVal(v, m.typeRef)
        else getTypeWrappedVal(v, m.typeRef, interfaceContext)))

  def getIoTsTypeString(typeRef: TypeRef): TsImports.With[String] = typeRef match {
    case NumberRef => imports.iotsNumber
    case BooleanRef => imports.iotsBoolean
    case StringRef => imports.iotsString
    case DateRef => imports.iotsLocalDate
    case DateTimeRef => imports.iotsDateTime
    case ArrayRef(innerType) => imports.iotsArray(getIoTsTypeString(innerType))
    case NonEmptyArrayRef(innerType) => imports.iotsNonEmptyArray(getIoTsTypeString(innerType))
    case CustomTypeRef(name, params, scalaType) =>
      if (params.isEmpty) customIoTsTypes(scalaType, name)
      else customIoTsTypes(scalaType, name) |+| params.joinParens(", ")(getIoTsTypeString)
    case UnknownTypeRef(unknown) => (imports.empty, unknown)
    case SimpleTypeRef(param) => (imports.empty, typeAsValArg(param))
    case UnionType(possibilities) =>
      imports.iotsUnion(possibilities.joinArray(getIoTsTypeString))
    case EitherType(lT, rT) =>
      imports.iotsEither(List(lT, rT).join(", ")(getIoTsTypeString))
    case TheseType(lT, rT) =>
      imports.iotsThese(List(lT, rT).join(", ")(getIoTsTypeString))
    case MapType(keyType, valueType) =>
      imports.iotsRecord(List(keyType, valueType).join(", ")(getIoTsTypeString))
    case TupleType(types) =>
      imports.iotsTuple(types.joinArray(getIoTsTypeString))
    case NullRef => imports.iotsNull
    case UndefinedRef => imports.iotsUndefined
  }

  def getIoTsRecordKeyTypeString(typeRef: TypeRef): TsImports.With[String] = typeRef match {
    case NumberRef => imports.iotsNumberFromString
    case _ => getIoTsTypeString(typeRef)
  }

  def getIoTsTypeWrappedVal(value: Any, typeRef: TypeRef): TsImports.With[String] = typeRef match {
    case NumberRef => imports.iotsLiteral(value.toString)
    case BooleanRef => imports.iotsLiteral(value.toString)
    case StringRef => imports.iotsLiteral(s"`${value.toString.trim}`")
    case DateRef => imports.iotsLiteral(s"`${value.toString.trim}`")
    case DateTimeRef => imports.iotsLiteral(s"new Date(`${value.toString.trim}`)")
    case ArrayRef(_) => imports.iotsLiteral(s"[$value]")
    case NonEmptyArrayRef(_) => imports.iotsLiteral(imports.iotsNonEmptyArray.value |+| s".of([$value])")
    case CustomTypeRef(name, params, scalaType) =>
      if (params.isEmpty) imports.custom(scalaType, codecName(name))
      else imports.custom(scalaType, codecName(name)) |+| params.joinParens(", ")(getIoTsTypeWrappedVal(value, _))
    case UnknownTypeRef(unknown) => unknown
    case SimpleTypeRef(param) => imports.iotsStrict(typeAsValArg(param))
    case UnionType(possibilities) =>
      imports.iotsStrict(imports.iotsUnion(
        possibilities.joinArray(getIoTsTypeWrappedVal(value, _))))
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

  def customIoTsTypes(scalaType: Type, name: String): TsImports.With[String] = name match {
    case "NonEmptyList" => imports.iotsNonEmptyArray.value
    case "optionFromNullable" => imports.iotsOption.value
    case _ => imports.custom(scalaType, codecName(name))
  }

  def getTypeWrappedVal(value: Any, typeRef: TypeRef, interfaceContext: Boolean): TsImports.With[String] = typeRef match {
    case NumberRef => value.toString
    case BooleanRef => value.toString
    case StringRef => s"`${value.toString.trim}`"
    case DateRef => s"`${value.toString.trim}`"
    case DateTimeRef => s"`${value.toString.trim}`"
    case ArrayRef(_) => s"[${value}]"
    case NonEmptyArrayRef(_) => imports.iotsNonEmptyArray.value |+| s".of([$value])"
    case CustomTypeRef(name, params, scalaType) =>
      imports.custom(scalaType, if (interfaceContext) interfaceName(name) else objectName(name)) |+|
        (if (params.isEmpty) "" else params.joinParens(", ")(getIoTsTypeString))
    case UnknownTypeRef(unknown) => unknown
    case SimpleTypeRef(param) => if (interfaceContext) param else typeAsValArg(param)
    case UnionType(possibilities) => imports.iotsUnion(possibilities.joinArray(getIoTsTypeString))
    case EitherType(lT, rT) => imports.iotsUnion(List(lT, rT).joinArray(getIoTsTypeString))
    case TheseType(lT, rT) => imports.iotsUnion(List(lT, rT, TupleType(ListSet(lT, rT))).joinArray(getIoTsTypeString))
    case MapType(keyType, valueType) => imports.iotsRecord(List(keyType, valueType).join(", ")(getIoTsTypeString))
    case TupleType(types) => imports.iotsTuple(types.joinArray(getIoTsTypeString))
    case NullRef => "null"
    case UndefinedRef => "undefined"
  }
}
