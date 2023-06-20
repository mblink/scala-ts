package sts

import cats.{Eval, Monoid}
import cats.data.{Chain, Ior, NonEmptyChain, NonEmptyList, NonEmptyVector}
import cats.syntax.foldable._
import io.circe.Json
import java.time.{Instant, LocalDate => JLocalDate, LocalDateTime, ZonedDateTime}
import org.joda.time.{DateTime, LocalDate}
import scala.annotation.experimental
import scala.collection.immutable.SortedSet
import scala.quoted._

private case class ScalaTs(
  customType: Expr[ScalaTs.CustomType],
  imports: Expr[TsImports.available],
)(using override val ctx: Quotes) extends ReflectionUtils {
  import ctx.reflect._
  import ScalaTs._

  private def fullTypeName(tpe: TypeRepr): String =
    tpe.show.split('[').head

  private def baseTypeName(tpe: TypeRepr): String =
    fullTypeName(tpe).split('.').last

  private def ordInstance[A: Type]: Expr[Generated] = {
    val typeRepr = TypeRepr.of[A]
    typeRepr.asType match {
      case '[String] => '{ $imports.fptsString("Ord", Some("stringOrd")) }
      case '[Boolean] => '{ $imports.fptsBoolean("Ord", Some("boolOrd")) }
      case NumberType() => '{ $imports.fptsNumber("Ord", Some("numberOrd")) }
      case '[Option[a]] => '{ $imports.fptsOption($imports.lift("Ord(") |+| ${ ordInstance[a] } |+| $imports.lift(")")) }
      case _ =>
        Mirror(typeRepr) match {
          case Some(m) if m.mirrorType == MirrorType.Sum =>
            '{ $imports.custom(TypeName(${ Expr(fullTypeName(typeRepr)) }), unionOrdName(${ Expr(baseTypeName(typeRepr)) })) }

          case _ =>
            report.errorAndAbort(s"`Ord` instance requested for ${typeRepr.show} but not found")
        }
    }
  }

  private def mkCodecName(tpe: TypeRepr): String =
    decap(baseTypeName(tpe)) + "C"

  private object NumberType {
    def unapply[A](t: Type[A]): Boolean =
      t match {
        case '[Byte] | '[Short] | '[Int] | '[Long] | '[Double] | '[Float] => true
        case _ => false
      }
  }

  @experimental
  private def tsRecordKeyType[A: Type](state: GenerateState): Expr[Generated] =
    TypeRepr.of[A].asType match {
      case NumberType() => '{ $imports.iotsNumberFromString }
      case _ => generate[A](state)
    }

  private def tsValue(typeRepr: TypeRepr, value: Expr[Any]): Expr[Generated] =
    typeRepr.asType match {
      case '[Json] => '{ $imports.lift($value.asInstanceOf[Json].noSpaces) }
      case NumberType() | '[BigDecimal] | '[Boolean] => '{ $imports.lift($value.toString) }
      case '[String] | '[LocalDate] | '[JLocalDate] | '[DateTime] | '[LocalDateTime] | '[ZonedDateTime] | '[Instant] => '{ $imports.lift("`" + $value.toString + "`") }
      case '[Eval[t]] => tsValue(TypeRepr.of[t], '{ $value.asInstanceOf[Eval[t]].value })
      case '[Chain[t]] =>
        val t = TypeRepr.of[t]
        '{ $imports.lift("[") |+| $value.asInstanceOf[Chain[t]].intercalateMap($imports.lift(", "))(a => ${ tsValue(t, 'a) }) |+| $imports.lift("]") }
      case '[List[t]] =>
        val t = TypeRepr.of[t]
        '{ $imports.lift("[") |+| $value.asInstanceOf[List[t]].intercalateMap($imports.lift(", "))(a => ${ tsValue(t, 'a) }) |+| $imports.lift("]") }
      case '[Vector[t]] =>
        val t = TypeRepr.of[t]
        '{ $imports.lift("[") |+| $value.asInstanceOf[Vector[t]].intercalateMap($imports.lift(", "))(a => ${ tsValue(t, 'a) }) |+| $imports.lift("]") }
      case '[Seq[t]] =>
        val t = TypeRepr.of[t]
        '{ $imports.lift("[") |+| $value.asInstanceOf[Seq[t]].intercalateMap($imports.lift(", "))(a => ${ tsValue(t, 'a) }) |+| $imports.lift("]") }
      case '[Set[t]] =>
        val t = TypeRepr.of[t]
        '{
          $imports.fptsReadonlySet(
            $imports.lift("fromReadonlyArray(") |+|
              ${ ordInstance[t] } |+|
              ")([" |+|
              $value.asInstanceOf[Set[t]].toList.intercalateMap($imports.lift(", "))(a => ${ tsValue(t, 'a) }) |+|
              "])"
          )
        }
      case '[NonEmptyChain[t]] => tsValue(TypeRepr.of[Chain[t]], '{ $value.asInstanceOf[NonEmptyChain[t]].toChain })
      case '[NonEmptyList[t]] => tsValue(TypeRepr.of[List[t]], '{ $value.asInstanceOf[NonEmptyList[t]].toList })
      case '[NonEmptyVector[t]] => tsValue(TypeRepr.of[Vector[t]], '{ $value.asInstanceOf[NonEmptyVector[t]].toVector })
      case '[scalaz.NonEmptyList[t]] => tsValue(TypeRepr.of[List[t]], '{ $value.asInstanceOf[scalaz.NonEmptyList[t]].list.toList })
      case '[Option[t]] =>
        '{
          $imports.fptsOption($value.asInstanceOf[Option[t]].fold($imports.lift("none"))(
            a => $imports.lift("some(") |+| ${ tsValue(TypeRepr.of[t], 'a) } |+| $imports.lift(")")))
        }
      case '[Either[l, r]] =>
        '{
          $imports.fptsEither($value.asInstanceOf[Either[l, r]].fold(
            l => $imports.lift("left(") |+| ${ tsValue(TypeRepr.of[l], 'l) } |+| $imports.lift(")"),
            r => $imports.lift("right(") |+| ${ tsValue(TypeRepr.of[r], 'r) } |+| $imports.lift(")"),
          ))
        }
      case '[scalaz.\/[l, r]] => tsValue(TypeRepr.of[Either[l, r]], '{ $value.asInstanceOf[scalaz.\/[l, r]].toEither })
      case '[Ior[l, r]] =>
        val (lt, rt) = (TypeRepr.of[l], TypeRepr.of[r])
        '{
          $imports.fptsThese($value.asInstanceOf[Ior[l, r]].fold(
            l => $imports.lift("left(") |+| ${ tsValue(lt, 'l) } |+| $imports.lift(")"),
            r => $imports.lift("right(") |+| ${ tsValue(rt, 'r) } |+| $imports.lift(")"),
            (l, r) => $imports.lift("both(") |+| ${ tsValue(lt, 'l) } |+| $imports.lift(", ") |+| ${ tsValue(rt, 'r) } |+| $imports.lift(")"),
          ))
        }
      case '[scalaz.\&/[l, r]] =>
        tsValue(
          TypeRepr.of[Ior[l, r]],
          '{ $value.asInstanceOf[scalaz.\&/[l, r]].fold(Ior.Left(_), Ior.Right(_), Ior.Both(_, _)) })
      case '[t] =>
        val tpe = TypeRepr.of[t]
        '{
          $imports.custom(
            TypeName(${ Expr(fullTypeName(tpe)) }, List(
              ${ Expr(fullTypeName(tpe.widen)) },
              ${ Expr(fullTypeName(tpe.dealias)) },
              ${ Expr(fullTypeName(tpe.simplified)) },
            )),
            decap($value.toString())
          )
        }
    }

  @experimental
  def generateEnum[A: Type](mirror: Mirror, state: GenerateState): Expr[Generated] = {
    val typeRepr = TypeRepr.of[A]
    val valueType = Expr(baseTypeName(typeRepr))

    def parseMembers(m: Mirror): (List[String], List[String], List[String], Boolean) = {
      given mb: Monoid[Boolean] = Monoid.instance(true, _ && _)

      m.mirrorType match {
        case MirrorType.Sum => m.types.toList.zipWithIndex.foldMap { case (t, i) =>
          t.asType match {
            case '[t] => Expr.summon[ValueOf[t]] match {
              case Some(_) =>
                val tag = m.labels(i)
                val constName = decap(tag)
                val codecName = constName + "C"
                (List(tag), List(constName), List(codecName), true)

              case None =>
                Mirror(t).fold((Nil, Nil, Nil, false))(parseMembers)
            }
          }
        }
        case MirrorType.Product => (Nil, Nil, Nil, false)
      }
    }

    val (memberNames, memberConstNames, memberCodecNames, allConstant) = parseMembers(mirror)
    val memberCodecs = mirror.types.zipWithIndex.map { case (t, i) =>
      t.asType match {
        case '[t] =>
          val typeSym = t.typeSymbol
          val tag = mirror.labels(i)
          val tagExpr = Expr(tag)
          val constName = decap(tag)
          val constNameExpr = Expr(constName)
          val codecNameExpr = Expr(constName + "C")
          val valueTypeExpr = Expr(cap(constName))
          val taggedCodecName = constName + "TaggedC"
          val taggedCodecNameExpr = Expr(taggedCodecName)
          val taggedValueTypeExpr = Expr(cap(taggedCodecName).stripSuffix("C"))
          Expr.summon[ValueOf[t]] match {
            case Some(v) =>
              val members = (typeSym.fieldMembers ++ typeSym.methodMembers).filter(s =>
                s.isValDef &&
                  !s.isClassConstructor &&
                  !(
                    s.privateWithin.nonEmpty ||
                    s.protectedWithin.nonEmpty ||
                    s.flags.is(Flags.Private) ||
                    s.flags.is(Flags.PrivateLocal) ||
                    s.flags.is(Flags.Protected)
                  )
              ).sortBy(_.name)
              val memberVals = Expr.ofList(members.map(s =>
                '{
                  $imports.lift("  " + ${ Expr(s.name) } + ": ") |+|
                    ${ tsValue(s.info, Select('{ $v.value }.asTerm, s).asExpr) }
                }
              ))
              val codec = '{
                // Const with all values
                $imports.lift("export const " + $constNameExpr + " = {\n") |+|
                  ("  _tag: `" + $tagExpr + "`,\n") |+|
                  intercalate($imports.lift(",\n"))($memberVals) |+|
                  "\n} as const;\n\n" |+|
                  // Const with only `_tag` value
                  ("export const " + $taggedCodecNameExpr + " = ") |+|
                  $imports.iotsTypeFunction(
                    $imports.lift("{ _tag: ") |+|
                      $imports.iotsLiteral("`" + $tagExpr + "`") |+|
                      " }"
                  ) |+|
                  ";\n" |+|
                  // Tagged codec type
                  ("export type " + cap($taggedCodecNameExpr) + " = typeof " + $taggedCodecNameExpr + ";\n") |+|
                  // Tagged value type
                  ("export type " + $taggedValueTypeExpr + " = ") |+|
                  $imports.iotsTypeOf(cap($taggedCodecNameExpr)) |+|
                  ";\n" |+|
                  // Full value type
                  ("export type " + $valueTypeExpr + " = " +
                    $taggedValueTypeExpr + " & typeof " + $constNameExpr + ";\n") |+|
                  // Full codec type
                  ("export const " + $codecNameExpr + " = ") |+|
                  $imports.fptsPipe(
                    $imports.lift($taggedCodecNameExpr + ", c => new ") |+|
                      $imports.iotsTypeType |+|
                      ("<" + $valueTypeExpr + ", " + $taggedValueTypeExpr + ">(\n") |+|
                      ${ Expr("  `" + baseTypeName(typeRepr) + " " + tag + "`,\n") } |+|
                      ("  (u: unknown): u is " + $valueTypeExpr + " => ") |+| $imports.fptsEither("isRight(c.decode(u)),\n") |+|
                      ("  (u: unknown): ") |+|
                      $imports.fptsEither("Either<") |+| $imports.iotsErrors |+| $imports.lift(", " + $valueTypeExpr + "> => ") |+|
                      $imports.fptsPipe(
                        $imports.lift("c.decode(u), ") |+|
                        $imports.fptsEither("map(x => ({ ...x, ..." + $constNameExpr + " }))")
                      ) |+|
                      ",\n" |+|
                      ("  (x: " + $valueTypeExpr + "): " + $taggedValueTypeExpr + " => ({ ...x, _tag: `" + $tagExpr + "` }),\n") |+|
                      ")"
                  ) |+|
                  ";"
              }
              codec

            case None =>
              generateTopLevel[t](true)
          }
      }
    }

    val allMemberCodecs =
      if (state.typeParamNames.isEmpty) memberCodecNames
      else memberCodecNames.map(n => n + "(" + state.typeParamNames.mkString(", ") + ")")
    val allMemberCodecsArr = '{ "[" + ${ Expr(allMemberCodecs.mkString(", ")) } + "]" }
    val allNamesConstName = '{ "all" + $valueType + "Names" }
    lazy val ordInst = '{
      $imports.lift("export const " + decap($valueType) + "Ord: ") |+|
        $imports.fptsOrd("Ord<" + $valueType + "> = ") |+|
        $imports.fptsPipe(
          $imports.fptsString("Ord", Some("stringOrd")) |+|
            ", " |+|
            $imports.fptsOrd("contramap(x => x._tag)")
        ) |+|
        ";\n"
    }

    '{
      intercalate($imports.lift("\n\n"))(${ Expr.ofList(memberCodecs) }) |+|
      "\n\n" |+|
      ("export const all" + $valueType + "C = ") |+|
      ${ state.typeParamsTypes } |+|
      ${ state.typeParamsFnArgs } |+|
      (if (${ Expr(state.typeParamNames.nonEmpty) }) " => " else "") |+|
      ($allMemberCodecsArr + " as const;\n") |+|
      ("export const " + $allNamesConstName + " = [" +
        ${ Expr(memberNames.map(s => s"`$s`").mkString(", ")) } + "] as const;\n") |+|
      ("export type " + $valueType + "Name = (typeof " + $allNamesConstName + ")[number];\n") |+|
      "\n" |+|
      ${ state.wrapCodec('{ $imports.iotsUnion($allMemberCodecsArr) }) } |+|
      "\n" |+|
      (if (${ Expr(state.typeParamNames.isEmpty) }) $ordInst else Generated.empty) |+|
      (
        if (${ Expr(allConstant) })
          $imports.lift("export const all" + $valueType + " = [" +
            ${ Expr(memberConstNames.mkString(", ")) } + "] as const;\n")
        else Generated.empty
      ) |+|
      (
        if (${ Expr(state.typeParamNames.isEmpty) })
          $imports.lift("export type " + $valueType + "Map<A> = { [K in " + $valueType + "Name]: A };\n")
        else
          Generated.empty
      )
    }
  }

  @experimental
  private def generateCaseClass[A: Type](mirror: Mirror, state: GenerateState): Expr[Generated] = {
    val typesAndLabels0 = mirror.types.toList.zip(mirror.labels).map {
      case (tpe, label) =>
        val res = tpe.asType match {
          case '[t] => '{ $imports.lift("  " + ${Expr(label)} + ": ") |+| ${ generate[t](state.copy(top = false)) } }
        }
        res
    }
    val typesAndLabels = Expr.ofList(
      if (state.inEnum)
        '{
          $imports.lift("  _tag: ") |+|
          $imports.iotsLiteral("`" + ${ Expr(baseTypeName(TypeRepr.of[A])) } + "`")
        } :: typesAndLabels0
      else
        typesAndLabels0
    )

    state.wrapCodec('{
      $imports.iotsTypeFunction(
        $imports.lift("{\n") |+|
        intercalate($imports.lift(",\n"))($typesAndLabels) |+|
        "\n}"
      )
    })
  }

  case class GenerateState(
    top: Boolean,
    inEnum: Boolean,
    typeParamNames: List[String],
    typeParamsTypes: Expr[Generated],
    typeParamsFnArgs: Expr[String],
    wrapCodec: Expr[Generated] => Expr[Generated],
  )

  @experimental
  def generate[A: Type](state: GenerateState): Expr[Generated] = {
    val typeRepr = TypeRepr.of[A]
    typeRepr.asType match {
      case '[TypeParam[a]] =>
        TypeRepr.of[a] match {
          case ConstantType(c) => c.value match { case s: String => '{ $imports.lift(${ Expr(s) }) } }
        }: @annotation.nowarn("msg=match may not be exhaustive")

      case '[Json] => '{ $imports.iotsUnknown }
      case NumberType() => '{ $imports.iotsNumber }
      case '[BigDecimal] => '{ $imports.iotsBigNumber }
      case '[Boolean] => '{ $imports.iotsBoolean }
      case '[String] => '{ $imports.iotsString }
      case '[LocalDate] => '{ $imports.iotsLocalDate }
      case '[JLocalDate] => '{ $imports.iotsLocalDate }
      case '[DateTime] => '{ $imports.iotsDateTime }
      case '[LocalDateTime] => '{ $imports.iotsDateTime }
      case '[ZonedDateTime] => '{ $imports.iotsDateTime }
      case '[Instant] => '{ $imports.iotsDateTime }
      case '[Eval[a]] => generate[a](state.copy(top = false))
      case '[Chain[a]] => '{ $imports.iotsReadonlyArray(${ generate[a](state.copy(top = false)) }) }
      case '[List[a]] => '{ $imports.iotsReadonlyArray(${ generate[a](state.copy(top = false)) }) }
      case '[Vector[a]] => '{ $imports.iotsReadonlyArray(${ generate[a](state.copy(top = false)) }) }
      case '[Seq[a]] => '{ $imports.iotsReadonlyArray(${ generate[a](state.copy(top = false)) }) }
      case '[Set[a]] => '{ $imports.iotsReadonlySetFromArray(${ generate[a](state.copy(top = false)) }, ${ ordInstance[a] }) }
      case '[SortedSet[a]] => '{ $imports.iotsReadonlySetFromArray(${ generate[a](state.copy(top = false)) }, ${ ordInstance[a] }) }
      case '[NonEmptyChain[a]] => '{ $imports.iotsReadonlyNonEmptyArray(${ generate[a](state.copy(top = false)) }) }
      case '[NonEmptyList[a]] => '{ $imports.iotsReadonlyNonEmptyArray(${ generate[a](state.copy(top = false)) }) }
      case '[NonEmptyVector[a]] => '{ $imports.iotsReadonlyNonEmptyArray(${ generate[a](state.copy(top = false)) }) }
      case '[scalaz.NonEmptyList[a]] => '{ $imports.iotsReadonlyNonEmptyArray(${ generate[a](state.copy(top = false)) }) }
      case '[Option[a]] => '{ $imports.iotsOption(${ generate[a](state.copy(top = false)) }) }
      case '[Either[l, r]] => '{ $imports.iotsEither(${ generate[l](state.copy(top = false)) }, ${ generate[r](state.copy(top = false)) }) }
      case '[scalaz.\/[l, r]] => '{ $imports.iotsEither(${ generate[l](state.copy(top = false)) }, ${ generate[r](state.copy(top = false)) }) }
      case '[Ior[l, r]] => '{ $imports.iotsThese(${ generate[l](state.copy(top = false)) }, ${ generate[r](state.copy(top = false)) }) }
      case '[scalaz.\&/[l, r]] => '{ $imports.iotsThese(${ generate[l](state.copy(top = false)) }, ${ generate[r](state.copy(top = false)) }) }
      case '[Map[k, v]] => '{ $imports.iotsRecord(${ tsRecordKeyType[k](state) }, ${ generate[v](state.copy(top = false)) }) }
      case _ if typeRepr <:< TypeRepr.of[Tuple] =>
        val tpes = typeRepr.typeArgs.map(_.asType match { case '[t] => generate[t](state.copy(top = false)) })
        '{ $imports.iotsTuple($imports.lift("[") |+| intercalate($imports.lift(", "))(${ Expr.ofList(tpes) })) }
      case _ =>
        if (state.top) {
          Mirror(typeRepr) match {
            case Some(mirror) =>
              mirror.mirrorType match {
                case MirrorType.Sum => generateEnum[A](mirror, state)
                case MirrorType.Product => generateCaseClass[A](mirror, state)
              }

            case None =>
              report.errorAndAbort(s"Can't generate TS code for ${typeRepr.show}")
          }
        } else {
          val typeParams = typeRepr match {
            case AppliedType(_, params) =>
              val tpeParamVals = Expr.ofList(params.map(_.asType match { case '[t] => generate[t](state.copy(top = false)) }))
              '{ $imports.lift("(") |+| intercalate($imports.lift(", "))($tpeParamVals) |+| ")" }

            case _ =>
              '{ Generated.empty }
          }

          '{
            $customType(${ Expr(typeRepr.show) })
              .getOrElse($imports.custom(
                TypeName(${ Expr(fullTypeName(typeRepr)) }, List(${ Expr(fullTypeName(typeRepr.dealias)) })),
                ${ Expr(mkCodecName(typeRepr)) }
              ) |+| $typeParams)
          }
        }
    }
  }

  private val typeParamTypes = IndexedSeq(
    TypeRepr.of[TypeParam["A1"]],
    TypeRepr.of[TypeParam["A2"]],
    TypeRepr.of[TypeParam["A3"]],
    TypeRepr.of[TypeParam["A4"]],
    TypeRepr.of[TypeParam["A5"]],
    TypeRepr.of[TypeParam["A6"]],
    TypeRepr.of[TypeParam["A7"]],
    TypeRepr.of[TypeParam["A8"]],
    TypeRepr.of[TypeParam["A9"]],
    TypeRepr.of[TypeParam["A10"]],
  )

  private def generatedTypeNames[A: Type]: Set[String] = {
    val typeRepr = TypeRepr.of[A]
    val typeName = fullTypeName(typeRepr)
    Mirror(typeRepr) match {
      case Some(m) =>
        m.mirrorType match {
          case MirrorType.Sum =>
            m.types.flatMap(_.asType match { case '[t] => generatedTypeNames[t] }).toSet + typeName
          case MirrorType.Product =>
            Set(typeName)
        }
      case None =>
        Set(typeName)
    }
  }

  def generatedTypes[A: Type]: Expr[Set[TypeName]] =
    '{ ${ Expr(generatedTypeNames[A]) }.map(TypeName(_)) }

  @experimental
  def generateTopLevel[A: Type](inEnum: Boolean): Expr[Generated] = {
    val typeRepr = TypeRepr.of[A]
    val (typeParamNames, typeParams, fnArgs, genCodec) = typeRepr match {
      case AppliedType(tpe, params) if params.nonEmpty =>
        val tps = params.zipWithIndex.map { case (_, i) => typeParamTypes(i) }
        val tpNames = tps.map(_.asType match {
          case '[TypeParam[a]] =>
            TypeRepr.of[a] match { case ConstantType(c) => c.value match { case s: String => s } }
        }): @annotation.nowarn("msg=match may not be exhaustive")
        val tpTypes = Expr.ofList(tpNames.map(t => '{ $imports.lift(${ Expr(t) } + " extends ") |+| $imports.iotsMixed }))
        val tpVals = Expr.ofList(tpNames.map(t => '{ ${ Expr(t) } + ": " + ${ Expr(t) } }))
        val tpsCode = '{
          $imports.lift("<") |+|
            intercalate($imports.lift(", "))($tpTypes) |+|
            ">"
        }
        val fnArgsCode = '{ $tpVals.mkString("(", ", ", ")") }
        val codec = AppliedType(tpe, tps).asType match { case '[t] => generate[t] }
        (tpNames, tpsCode, fnArgsCode, codec)

      case _ =>
        (Nil, '{ Generated.empty }, '{""}, generate[A])
    }

    val codecName = mkCodecName(typeRepr)
    val codecNameExpr = Expr(codecName)
    val codecType = Expr(cap(codecName))
    val valueType = Expr(cap(codecName).stripSuffix("C"))
    val className = Expr(codecName + "C")
    val joinedTPNames = Expr(typeParamNames.mkString(", "))
    val genState = GenerateState(true, inEnum, typeParamNames, typeParams, fnArgs, _)

    if (typeParamNames.nonEmpty)
      genCodec(genState(codec => '{
        $imports.lift("export class " + $className) |+|
          $typeParams |+|
          " { codec = " |+|
          $fnArgs |+|
          " => " |+|
          $codec |+|
          "}\n" |+|
          ("export const " + $codecNameExpr + " = ") |+|
          $typeParams |+|
          $fnArgs |+|
          (" => new " + $codecNameExpr + "C<" + $joinedTPNames + ">().codec(" + $joinedTPNames + ");\n") |+|
          ("export type " + $codecType) |+|
          $typeParams |+|
          (" = ReturnType<" + $className + "<" + $joinedTPNames + ">[\"codec\"]>;\n") |+|
          ("export type " + $valueType + "<" + $joinedTPNames + "> = ") |+|
          $imports.iotsTypeOf(
            $imports.lift($codecType + "<") |+|
              ${ Expr(typeParamNames) }.intercalateMap($imports.lift(", "))(
                t => $imports.iotsTypeType |+| $imports.lift("<" + t + ">")) |+|
              ">"
          ) |+|
          ";"
      }))
    else
      genCodec(genState(codec => '{
        $imports.lift("export const " + $codecNameExpr + " = ") |+|
          $codec |+|
          ";\n" |+|
          ("export type " + $codecType + " = typeof " + $codecNameExpr + ";\n") |+|
          ("export type " + $valueType + " = ") |+|
          $imports.iotsTypeOf($codecType) |+|
          ";\n"
      }))
  }
}

object ScalaTs {
  private[ScalaTs] case class TypeParam[Name]()

  def intercalate(glue: Generated)(xs: List[Generated]): Generated =
    xs match {
      case Nil => Generated.empty
      case h :: t => t.foldLeft(h)((acc, x) => acc |+| glue |+| x)
    }

  def cap(s: String): String = s.take(1).toUpperCase + s.drop(1)
  def decap(s: String): String = s.take(1).toLowerCase + s.drop(1)
  def unionOrdName(s: String): String = decap(s) + "Ord"

  @experimental
  private def generateImpl[A: Type](customType: Expr[CustomType], imports: Expr[TsImports.available])(using ctx: Quotes): Expr[(Set[TypeName], Generated)] = {
    val sts = new ScalaTs(customType, imports)
    Expr.ofTuple((sts.generatedTypes[A], sts.generateTopLevel[A](false)))
  }

  trait CustomType {
    def apply(name: String): Option[Generated]
  }

  object CustomType {
    val none = new CustomType {
      def apply(name: String) = None
    }
  }

  inline def generate[A](inline customType: CustomType, inline imports: TsImports.available): (Set[TypeName], Generated) =
    ${ generateImpl[A]('customType, 'imports) }
}
