package com.mpc.scalats
package core

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
      case decl: UnionDeclaration =>
        emitUnionDeclaration(decl)
      case decl: SingletonDeclaration =>
        emitSingletonDeclaration(decl)
      case decl: TypeDeclaration =>
        emitTypeDeclaration(decl)
    }

  private def iotsTypeParams(typeParams: ListSet[String]): TsImports.With[String] =
    typeParams.joinTypeParams(p => s"$p extends " |+| imports.iotsMixed)

  private def iotsTypeParamVals(typeParams: ListSet[String]): TsImports.With[String] =
    typeParams.joinParens(", ")(p => s"${typeAsValArg(p)}: $p")

  private def iotsTypeParamArgs(typeParams: ListSet[String]): TsImports.With[String] =
    if (typeParams.isEmpty) (imports.emptyStr)
    else iotsTypeParams(typeParams) |+| iotsTypeParamVals(typeParams) |+| " => "

  private def interfaceTypeParamArgs(typeParams: ListSet[String]): TsImports.With[String] =
    if (typeParams.isEmpty) (imports.emptyStr) else typeParams.joinTypeParams(p => p)

  private case class CodecCtx(
    codecName: String,
    tpeName: String,
    typeParams: ListSet[String],
    modType: String => String
  ) {
    def cType: String = modType(codecType(tpeName))
    def className = s"${codecName}C"
    def tps: ListSet[String] = typeParams
    def hasTps: Boolean = typeParams.nonEmpty
    def tpe: TsImports.With[String] = modType(tpeName) |+| typeParams.joinTypeParams(identity)
    def classTpe: TsImports.With[String] = className |+| typeParams.joinTypeParams(identity)

    def codecValDef(body: CodecCtx => Lines): Lines =
      "export " +: (if (hasTps) (s"class $className" |+| iotsTypeParams(tps) |+|
        " { codec = " |+| iotsTypeParamVals(typeParams) |+| " => ") +: body(this) :+ "};"
      else s"const $codecName = " +: body(this) :+ ";") |+|
      (if (hasTps) line(s"export const $codecName = " |+| iotsTypeParamArgs(tps) |+|
        "new " |+| classTpe |+| "().codec" |+| tps.joinParens(", ")(typeAsValArg) |+| ";") else Nil)

    def codecTypeDef: Lines =
      line(s"export type $cType" |+| iotsTypeParams(tps) |+| " = " |+| (
        if (hasTps) "ReturnType<" |+| classTpe |+| """["codec"]>;"""
        else s"typeof $codecName;"
      ))

    def typeDefPrefix: TsImports.With[String] =
      "export type " |+| tpe |+| " = "

    def typeDef: Lines =
      line(typeDefPrefix |+| imports.iotsTypeOf(cType |+| tps.joinTypeParams(imports.iotsTypeType |+| "<" |+| _ |+| ">")) |+| ";")

    def emit(body: CodecCtx => Lines): Lines =
      codecValDef(body) |+|
      codecTypeDef |+|
      typeDef

    def withNames(cName: String, tName: String): CodecCtx =
      copy(codecName = cName, tpeName = tName)
  }

  private def membersCodec(members: ListSet[Member])(
    onNoVal: TypeRef => TsImports.With[String],
    modIndent: String => String = identity,
    wrapDef: Lines => Lines = identity,
  )(implicit ctx: TsImports.Ctx): CodecCtx => Lines =
    _ => imports.iotsTypeFunction.lines(
      _ ++ "{",
      wrapDef(emitMembers(members, getIoTsTypeWrappedVal, onNoVal, modIndent = modIndent)),
      "}" ++ _)

  private def unionCodec(possibilities: ListSet[CustomTypeRef])(implicit ctx: TsImports.Ctx): CodecCtx => Lines = c => {
    val memberCodec = (t: CustomTypeRef) => codecName(t.name) |+|
      (if (t.typeArgs.isEmpty) "" else t.typeArgs.joinParens(", ")(getIoTsTypeString))

    if (possibilities.size == 1) line(memberCodec(possibilities.head))
    else if (possibilities.size == 0) sys.error(s"Can't create ${c.codecName} union composed of 0 members")
    else line(imports.iotsUnion(possibilities.joinArray(memberCodec)))
  }

  private def emitCodec0(
    name: String,
    tpeName: String,
    typeParams: ListSet[String],
    mkCodecName: String => String = codecName,
    modType: String => String = identity
  )(emit: CodecCtx => Lines): Lines =
    emit(CodecCtx(mkCodecName(name), tpeName, typeParams, modType))

  private def emitCodec(
    name: String,
    tpeName: String,
    typeParams: ListSet[String],
    mkCodecName: String => String = codecName,
    modType: String => String = identity
  )(body: CodecCtx => Lines): Lines =
    emitCodec0(name, tpeName, typeParams, mkCodecName, modType)(_.emit(body))

  private def _tagMember(name: String): Member = Member("_tag", StringRef, Some(name))

  @annotation.nowarn("msg=never used")
  private def emitDiscriminatedCodec(
    name: String,
    tpeName: String,
    members: ListSet[Member],
    typeParams: ListSet[String],
    superInterface: UnionDeclaration,
  )(
    onNoVal: TypeRef => TsImports.With[String],
    constantValue: Option[TsImports.With[String]] = None
  )(implicit ctx: TsImports.Ctx): Lines = {
    val (tps, hasTps) = (list(typeParams), typeParams.nonEmpty)
    val idTps = tps.joinTypeParams(identity)
    val (dscrName, dscrTpeName) = (s"${name}Discr", s"${tpeName}Discr")
    val (cName, dscrCName) = (codecName(name), codecName(dscrName))
    val (tpe, dscrTpe) = (tpeName |+| idTps, dscrTpeName |+| idTps)
    val tpeOfTps = tps.joinTypeParams(imports.iotsTypeOf(_))
    val (tpeOfTpe, tpeOfDscrTpe) = (tpeName |+| tpeOfTps, dscrTpeName |+| tpeOfTps)

    emitCodec(name, tpeName, typeParams)(membersCodec(
      ListSet(Member("_tag", StringRef, Some(name))) ++ members)(onNoVal))
  }

  private def emitImappedCodec(
    origName: String,
    origTpeName: String,
    newName: String,
    newTpeName: String,
    typeParams: ListSet[String],
    superInterface: Option[UnionDeclaration]
  )(newToOld: String => TsImports.With[String], oldToNew: String => TsImports.With[String]): Lines = {
    val (origCName, newCName) = (codecName(origName), codecName(newName))
    val tpeOfTps = typeParams.joinTypeParams(imports.iotsTypeOf(_))
    val (origTpeOfTpe, newTpeOfTpe) = (origTpeName |+| tpeOfTps, newTpeName |+| tpeOfTps)

    imports.fptsPipe.lines(
      s"export const $newCName = " |+| iotsTypeParamArgs(typeParams) |+| _ |+| origCName |+|
        (if (typeParams.nonEmpty) typeParams.joinParens(", ")(typeAsValArg) else "") |+|
        ", c => new " |+| imports.iotsTypeType |+| List(newTpeOfTpe, origTpeOfTpe).joinTypeParams(identity) |+| "(",
      line(s"""$indent"${superInterface.fold("")(_.name ++ " ")}$newName",""") |+|
      line(s"$indent(u: unknown): u is " |+| newTpeOfTpe |+| " => " |+| imports.fptsEither("isRight(c.decode(u))") |+| ",") |+|
      line(s"$indent(u: unknown): " |+|
        imports.fptsEither("Either" |+| List(imports.iotsErrors, newTpeOfTpe).joinTypeParams(identity)) |+| " => " |+|
        imports.fptsPipe("c.decode(u), " |+| imports.fptsEither("map(x => " |+| oldToNew("x") |+| ")")) |+| ",") |+|
      line(s"$indent(x: " |+| newTpeOfTpe |+| "): " |+| origTpeOfTpe |+| " => " |+| newToOld("x")),
      ")" ++ _ ++ ";")
  }

  def emitIoTsInterfaceDeclaration(decl: InterfaceDeclaration)(implicit ctx: TsImports.Ctx): Lines = {
    val InterfaceDeclaration(name, fields, typeParams, superInterface) = decl

    emitCodec(name, name, typeParams)(membersCodec(fields ++ superInterface.map(_ => _tagMember(name)))(getIoTsTypeString)) |+|
    line()
  }

  def emitInterfaceDeclaration(name: String, members: ListSet[Member], typeParams: ListSet[String], superInterface: Option[UnionDeclaration])(implicit ctx: TsImports.Ctx): Lines =
    if (!config.emitInterfaces) Nil else
      line(s"export interface ${interfaceName(name)}" |+| interfaceTypeParamArgs(typeParams) |+|
        superInterface.fold((imports.emptyStr))(i => s" extends ${interfaceName(i.name)}" |+| interfaceTypeParamArgs(i.typeParams)) |+| " {") |+|
      emitMembers(members, getTypeWrappedVal(_, _, true)) |+|
      line("}") |+|
      line()

  private def tsUnionName(name: String): String = s"${codecType(name)}U"

  private def emitUnionDeclaration(decl: UnionDeclaration)(implicit ctx: TsImports.Ctx): Lines = {
    val UnionDeclaration(name, _, typeParams, possibilities, _, emitAll) = decl

    def union(nameFn: CustomTypeRef => TsImports.With[String], prefix: String, glue: String, suffix: String) =
      possibilities.join(prefix, glue, suffix)(nameFn)

    val memberCodec = (t: CustomTypeRef) => codecName(t.name) |+|
      (if (t.typeArgs.isEmpty) "" else t.typeArgs.joinParens(", ")(getIoTsTypeString))
    val codecs = union(memberCodec, "[", ", ", "]")
    val (allCName, allNamesConst, nameType, ordInst, tpeMap) =
      (s"all${name}C", s"all${name}Names", s"${name}Name", unionOrdName(name), s"${name}Map")

    line(s"export const $allCName = " |+| iotsTypeParamArgs(typeParams) |+| codecs |+| " as const;") |+|
    line(s"export const $allNamesConst = " |+| possibilities.joinArray(p => s""""${p.name}"""") |+| " as const;") |+|
    line(s"export type $nameType = (typeof $allNamesConst)[number];") |+|
    emitCodec(name, name, typeParams, tsUnionName, _ ++ "U")(unionCodec(possibilities)) |+|
    (if (typeParams.isEmpty) line(s"export const $ordInst: " |+| imports.fptsOrd(s"Ord<${name}U>") |+| " = " |+| iotsTypeParamArgs(typeParams) |+| imports.fptsPipe(
      imports.fptsString("Ord", Some("stringOrd")) |+| ", " |+| imports.fptsOrd("contramap(x => x._tag)")
    )) else Nil) |+|
    (if (emitAll) line(s"export const all$name = " |+| union(p => objectName(p.name), "[", ", ", "]") |+| " as const;") else Nil) |+|
    (if (typeParams.isEmpty) line(s"export type $tpeMap<A> = { [K in $nameType]: A };") else Nil) |+|
    line()
  }

  private def emitSingletonDeclaration(decl: SingletonDeclaration)(implicit ctx: TsImports.Ctx): Lines = {
    val SingletonDeclaration(name, members, superInterface) = decl
    val (ifaceName, objName, taggedName) = (interfaceName(name), objectName(name), s"${name}Tagged")
    val (taggedIfaceName, tagMmbr) = (interfaceName(taggedName), _tagMember(name))

    line(s"export const $objName = {") |+|
    emitMembers(members ++ superInterface.map(_ => tagMmbr), getTypeWrappedVal(_, _, false)) |+|
    line("} as const;") |+|
    line() |+|
    superInterface.fold(
      emitCodec(name, interfaceName(name), ListSet.empty)(membersCodec(members)(getTypeRefString)))(i =>
      emitCodec0(taggedName, taggedIfaceName, ListSet())(c =>
        c.emit(membersCodec(ListSet(tagMmbr))(getTypeRefString)) |+|
        line(c.withNames(name, ifaceName).typeDefPrefix |+| c.tpe |+| s" & typeof $objName; ")) |+|
      emitImappedCodec(taggedName, taggedIfaceName, name, ifaceName, ListSet(),Some(i))(
        s => s"""({ ...$s, ${tagMmbr.name}: "$name" })""", s => s"({ ...$s, ...$objName })")) |+|
    line()
  }

  private def emitTypeDeclaration(decl: TypeDeclaration)(implicit ctx: TsImports.Ctx): Lines = {
    val TypeDeclaration(name, typeRef, typeParams) = decl
    val typeVal = codecName(name)

    line(s"export const $typeVal = " |+| iotsTypeParamArgs(typeParams) |+| getIoTsTypeString(typeRef) |+| ";") |+|
    (if (typeParams.isEmpty) line(s"export type $name = " |+| imports.iotsTypeOf(s"typeof $typeVal") |+| ";") else Nil ) |+|
    line()
  }

  def emitMembers(
    members: ListSet[Member],
    onVal: (Any, TypeRef) => TsImports.With[String],
    onNoVal: TypeRef => TsImports.With[String],
    modIndent: String => String
  ): Lines =
    members.joinLines(",")(m => s"${modIndent(indent)}${m.name}: " |+|
      m.value.fold(onNoVal(m.typeRef))(onVal(_, m.typeRef)))

  def emitMembers(
    members: ListSet[Member],
    onVal: (Any, TypeRef) => TsImports.With[String]
  )(implicit ctx: TsImports.Ctx): Lines =
    emitMembers(members, onVal, getTypeRefString, identity)

  def getIoTsTypeString(typeRef: TypeRef)(implicit ctx: TsImports.Ctx): TsImports.With[String] = typeRef match {
    case NumberRef => imports.iotsNumber
    case BooleanRef => imports.iotsBoolean
    case StringRef => imports.iotsString
    case DateRef => imports.iotsLocalDate
    case DateTimeRef => imports.iotsDateTime
    case ArrayRef(innerType) => imports.iotsReadonlyArray(getIoTsTypeString(innerType))
    case SetRef(innerType) =>
      imports.iotsReadonlySetFromArray(getIoTsTypeString(innerType), imports.fptsOrdInstance(innerType))
    case NonEmptyArrayRef(innerType) => imports.iotsReadonlyNonEmptyArray(getIoTsTypeString(innerType))
    case CustomTypeRef(name, params, scalaType) =>
      customIoTsTypes(scalaType, name, codecName) |+|
        (if (params.isEmpty) "" else params.joinParens(", ")(getIoTsTypeString))
    case UnknownTypeRef(unknown) => (imports.empty, unknown)
    case SimpleTypeRef(param) => (imports.empty, typeAsValArg(param))
    case UnionType(name, typeParams, possibilities, scalaType) =>
      imports.custom(scalaType, tsUnionName(name))
        .getOrElse(imports.iotsUnion(possibilities.joinArray(getIoTsTypeString))) |+|
        (if (typeParams.isEmpty) imports.emptyStr else typeParams.joinParens(", ")(getIoTsTypeString))
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
    case SetRef(typeRef) => imports.iotsLiteral(imports.fptsReadonlySet(
      s"fromReadonlyArray(" |+| imports.fptsOrdInstance(typeRef) |+| s")([$value])"
    ))
    case NonEmptyArrayRef(_) => imports.iotsLiteral(imports.iotsReadonlyNonEmptyArray.value |+| s".of([$value])")
    case CustomTypeRef(name, params, scalaType) =>
      customIoTsTypes(scalaType, name, codecName) |+|
        (if (params.isEmpty) "" else params.joinParens(", ")(getIoTsTypeWrappedVal(value, _)))
    case UnknownTypeRef(unknown) => unknown
    case SimpleTypeRef(param) => imports.iotsStrict(typeAsValArg(param))
    case UnionType(name, typeParams, possibilities, scalaType) =>
      imports.custom(scalaType, name)
        .getOrElse(imports.iotsStrict(imports.iotsUnion(possibilities.joinArray(getIoTsTypeWrappedVal(value, _))))) |+|
        (if (typeParams.isEmpty) imports.emptyStr else typeParams.joinParens(", ")(getIoTsTypeWrappedVal(value, _)))
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
    case _ => imports.custom(scalaType, customName(name)).getOrElse(imports.lift(customName(name)))
  }

  def getTypeWrappedVal(value: Any, typeRef: TypeRef, interfaceContext: Boolean)(implicit ctx: TsImports.Ctx): TsImports.With[String] = typeRef match {
    case NumberRef => value.toString
    case BooleanRef => value.toString
    case StringRef => s"`${value.toString.trim}`"
    case DateRef => s"`${value.toString.trim}`"
    case DateTimeRef => s"`${value.toString.trim}`"
    case ArrayRef(t) => value.asInstanceOf[Iterable[Any]].joinArray(getTypeWrappedVal(_, t, interfaceContext))
    case SetRef(t) => imports.fptsReadonlySet(s"fromReadonlyArray(" |+| imports.fptsOrdInstance(t) |+| s")(" |+|
      value.asInstanceOf[Iterable[Any]].joinArray(getTypeWrappedVal(_, t, interfaceContext)) |+|
    ")")
    case NonEmptyArrayRef(t) =>
      imports.iotsReadonlyNonEmptyArray.value |+| ".of(" |+| getTypeWrappedVal(value, ArrayRef(t), interfaceContext) |+| ")"
    case CustomTypeRef(name, params, scalaType) =>
      val typeName = if (interfaceContext) interfaceName(name) else objectName(name)
      imports.custom(scalaType, typeName).getOrElse(imports.lift(typeName)) |+|
        (if (params.isEmpty) "" else params.joinParens(", ")(getIoTsTypeString))
    case UnknownTypeRef(unknown) => unknown
    case SimpleTypeRef(param) => if (interfaceContext) param else typeAsValArg(param)
    case u @ UnionType(_, params, possibilities, _) =>
      val valueType = scala.reflect.runtime.currentMirror.classSymbol(value.getClass).toType
      val (customTypeName, customType) = possibilities.collectFirst {
        case CustomTypeRef(n, _, t) if t =:= valueType => (n, t)
      }.getOrElse(sys.error(s"Value $value of type $valueType is not a member of union $u"))
      val objName = objectName(customTypeName)
      imports.custom(customType, objName).getOrElse(imports.lift(objName)) |+|
        (if (params.isEmpty) "" else params.joinParens(", ")(getIoTsTypeString))
    case OptionType(iT) => getIoTsTypeString(iT)
    case EitherType(lT, rT) => imports.iotsUnion(List(lT, rT).joinArray(getIoTsTypeString))
    case TheseType(lT, rT) => imports.iotsUnion(List(lT, rT, TupleType(ListSet(lT, rT))).joinArray(getIoTsTypeString))
    case MapType(keyType, valueType) => imports.iotsRecord(List(keyType, valueType).join(", ")(getIoTsTypeString))
    case TupleType(types) => imports.iotsTuple(types.joinArray(getIoTsTypeString))
    case NullRef => "null"
    case UndefinedRef => "undefined"
  }
}
