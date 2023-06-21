package sts

import cats.{Eval, Monoid}
import cats.data.{Chain, Ior, NonEmptyChain, NonEmptyList, NonEmptyVector}
import cats.syntax.foldable.*
import cats.syntax.semigroup.*
import io.circe.Json
import java.io.{File, PrintStream}
import java.time.{Instant, LocalDate => JLocalDate, LocalDateTime, ZonedDateTime}
import org.joda.time.{DateTime, LocalDate}
import scala.annotation.experimental
import scala.collection.immutable.SortedSet
import scala.quoted.*
import scala.util.Using

case class ReferencedTypes(types: Map[TypeName, Set[TypeName]])

object ReferencedTypes {
  val empty = ReferencedTypes(Map.empty)

  given monoid: Monoid[ReferencedTypes] = Monoid.instance(empty, (a, b) => ReferencedTypes(a.types |+| b.types))
}

private case class ScalaTs(
  customType: Expr[ScalaTs.CustomType],
  imports: Expr[TsImports.available],
)(using override val ctx: Quotes) extends ReflectionUtils {
  import ctx.reflect.*
  import ScalaTs.*

  // Full type name, including package, excluding type parameters
  // e.g. `fullTypeName("foo.bar.Baz[Quux]")` => `"foo.bar.Baz"`
  private def fullTypeName(tpe: TypeRepr): String =
    tpe.show.split('[').head

  // Base type name, excluding type paramters
  // e.g. `baseTypeName("foo.bar.Baz[Quux]")` => `"Baz"`
  private def baseTypeName(tpe: TypeRepr): String =
    fullTypeName(tpe).split('.').last

  private def ordInstance[A: Type]: Expr[Generated] = {
    val typeRepr = TypeRepr.of[A]
    typeRepr.asType match {
      case '[String] => '{ $imports.fptsString("Ord", Some("stringOrd")) }
      case '[Boolean] => '{ $imports.fptsBoolean("Ord", Some("boolOrd")) }
      case NumberType() => '{ $imports.fptsNumber("Ord", Some("numberOrd")) }
      case '[Option[a]] => '{ $imports.fptsOption($imports.lift("Ord(") |+| ${ ordInstance[a] } |+| ")") }
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
    if (tpe.show != tpe.dealias.show)
      maybeDecap(baseTypeName(tpe)) + "C"
    else
      Mirror(tpe).map(_.mirrorType) match {
        case Some(MirrorType.Sum) => baseTypeName(tpe) + "CU"
        case Some(MirrorType.Product) | None => maybeDecap(baseTypeName(tpe)) + "C"
      }

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
      case _ => '{ ${ generate[A](state) }.foldMap(_._2) }
    }

  private def tsValue(typeRepr: TypeRepr, value: Expr[Any]): Expr[Generated] =
    typeRepr.asType match {
      case '[Json] => '{ $imports.lift($value.asInstanceOf[Json].noSpaces) }
      case NumberType() | '[BigDecimal] | '[Boolean] => '{ $imports.lift($value.toString) }
      case '[String] | '[LocalDate] | '[JLocalDate] | '[DateTime] | '[LocalDateTime] | '[ZonedDateTime] | '[Instant] => '{ $imports.lift("`" + $value.toString + "`") }
      case '[Eval[t]] => tsValue(TypeRepr.of[t], '{ $value.asInstanceOf[Eval[t]].value })
      case '[Chain[t]] =>
        val t = TypeRepr.of[t]
        '{ $imports.lift("[") |+| $value.asInstanceOf[Chain[t]].intercalateMap($imports.lift(", "))(a => ${ tsValue(t, 'a) }) |+| "]" }
      case '[List[t]] =>
        val t = TypeRepr.of[t]
        '{ $imports.lift("[") |+| $value.asInstanceOf[List[t]].intercalateMap($imports.lift(", "))(a => ${ tsValue(t, 'a) }) |+| "]" }
      case '[Vector[t]] =>
        val t = TypeRepr.of[t]
        '{ $imports.lift("[") |+| $value.asInstanceOf[Vector[t]].intercalateMap($imports.lift(", "))(a => ${ tsValue(t, 'a) }) |+| "]" }
      case '[Seq[t]] =>
        val t = TypeRepr.of[t]
        '{ $imports.lift("[") |+| $value.asInstanceOf[Seq[t]].intercalateMap($imports.lift(", "))(a => ${ tsValue(t, 'a) }) |+| "]" }
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
            a => $imports.lift("some(") |+| ${ tsValue(TypeRepr.of[t], 'a) } |+| ")"))
        }
      case '[Either[l, r]] =>
        '{
          $imports.fptsEither($value.asInstanceOf[Either[l, r]].fold(
            l => $imports.lift("left(") |+| ${ tsValue(TypeRepr.of[l], 'l) } |+| ")",
            r => $imports.lift("right(") |+| ${ tsValue(TypeRepr.of[r], 'r) } |+| ")",
          ))
        }
      case '[scalaz.\/[l, r]] => tsValue(TypeRepr.of[Either[l, r]], '{ $value.asInstanceOf[scalaz.\/[l, r]].toEither })
      case '[Ior[l, r]] =>
        val (lt, rt) = (TypeRepr.of[l], TypeRepr.of[r])
        '{
          $imports.fptsThese($value.asInstanceOf[Ior[l, r]].fold(
            l => $imports.lift("left(") |+| ${ tsValue(lt, 'l) } |+| ")",
            r => $imports.lift("right(") |+| ${ tsValue(rt, 'r) } |+| ")",
            (l, r) => $imports.lift("both(") |+| ${ tsValue(lt, 'l) } |+| ", " |+| ${ tsValue(rt, 'r) } |+| ")",
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
            TypeName(${ Expr(fullTypeName(tpe)) }),
            maybeDecap(clsNme($value))
          )
        }
    }

  private def valMembers(typeRepr: TypeRepr): List[Symbol] = {
    val typeSym = typeRepr.typeSymbol
    (typeSym.fieldMembers ++ typeSym.methodMembers).filter(s =>
      s.isValDef &&
        !s.isClassConstructor &&
        s.name != "toString" &&
        !(
          s.privateWithin.nonEmpty ||
          s.protectedWithin.nonEmpty ||
          s.flags.is(Flags.Private) ||
          s.flags.is(Flags.PrivateLocal) ||
          s.flags.is(Flags.Protected)
        )
    ).sortBy(_.name)
  }

  private def valDefType(typeRepr: TypeRepr, sym: Symbol): TypeRepr = {
    def unAnd(tpe: TypeRepr): TypeRepr = tpe match {
      case AndType(t, _) => unAnd(t)
      case AppliedType(t, ts) => AppliedType(t, ts.map(unAnd))
      case t => t
    }

    sym.tree match {
      case ValDef(_, _, Some(term)) => unAnd(term.tpe)
      case _ => report.errorAndAbort(s"Failed to get type of ${typeRepr.show}#${sym.name}")
    }
  }

  @experimental
  private def generateEnum[A: Type](mirror: Mirror, state: GenerateState): Expr[List[(Option[TypeName], Generated)]] = {
    val typeRepr = TypeRepr.of[A]
    val valueType = Expr(baseTypeName(typeRepr))

    def parseMembers(m: Mirror): (Boolean, List[String], List[String], List[String], List[Expr[List[(Option[TypeName], Generated)]]]) = {
      given mb: Monoid[Boolean] = Monoid.instance(true, _ && _)

      m.mirrorType match {
        case MirrorType.Sum => m.types.toList.zipWithIndex.foldMap { case (t, i) =>
          t.asType match {
            case '[t] =>
              val name = m.labels(i)
              val nameExpr = Expr(name)
              val constName = maybeDecap(name)
              val constNameExpr = Expr(constName)
              val codecName = constName + "C"
              val codecNameExpr = Expr(codecName)
              val valueTypeExpr = Expr(cap(constName))
              val taggedCodecName = constName + "TaggedC"
              val taggedCodecNameExpr = Expr(taggedCodecName)
              val taggedValueTypeExpr = Expr(cap(taggedCodecName).stripSuffix("C"))

              Expr.summon[ValueOf[t]] match {
                case Some(v) =>
                  val members = valMembers(t)
                  val memberVals = Expr.ofList(members.map(s =>
                    '{
                      $imports.lift("  " + ${ Expr(s.name) } + ": ") |+|
                        ${ tsValue(valDefType(t, s), Select('{ $v.value }.asTerm, s).asExpr) }
                    }
                  ))
                  val codec = '{
                    // Const with all values
                    $imports.lift("export const " + $constNameExpr + " = {\n") |+|
                      ("  _tag: `" + $nameExpr + "`,\n") |+|
                      intercalate($imports.lift(",\n"))($memberVals) |+|
                      "\n} as const;\n\n" |+|
                      // Const with only `_tag` value
                      ("export const " + $taggedCodecNameExpr + " = ") |+|
                      $imports.iotsTypeFunction(
                        $imports.lift("{ _tag: ") |+|
                          $imports.iotsLiteral("`" + $nameExpr + "`") |+|
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
                          ${ Expr("  `" + baseTypeName(typeRepr) + " " + name + "`,\n") } |+|
                          ("  (u: unknown): u is " + $valueTypeExpr + " => ") |+| $imports.fptsEither("isRight(c.decode(u)),\n") |+|
                          ("  (u: unknown): ") |+|
                          $imports.fptsEither("Either<") |+| $imports.iotsErrors |+| (", " + $valueTypeExpr + "> => ") |+|
                          $imports.fptsPipe(
                            $imports.lift("c.decode(u), ") |+|
                            $imports.fptsEither("map(x => ({ ...x, ..." + $constNameExpr + " }))")
                          ) |+|
                          ",\n" |+|
                          ("  (x: " + $valueTypeExpr + "): " + $taggedValueTypeExpr + " => ({ ...x, _tag: `" + $nameExpr + "` }),\n") |+|
                          ")"
                      ) |+|
                      ";"
                  }

                  (true, List(name), List(constName), List(codecName), List('{ List((Some(TypeName(${ Expr(fullTypeName(t)) })), $codec)) }))

                case None =>
                  Mirror(t).fold((false, Nil, Nil, Nil, Nil))(parseMembers)
              }
          }
        }
        case MirrorType.Product =>
          val name = baseTypeName(m.mirroredType)
          val codecName = mkCodecName(m.mirroredType)
          m.mirroredType.asType match {
            case '[t] => (false, List(name), Nil, List(codecName), List(generateTopLevel[t](true)))
          }
      }
    }

    val (allConstant, memberNames, memberConstNames, memberCodecNames, memberCodecs) = parseMembers(mirror)
    val allMemberCodecs =
      if (state.typeParamNames.isEmpty) memberCodecNames
      else memberCodecNames.map(n => n + "(" + state.typeParamNames.mkString(", ") + ")")
    val allMemberCodecsArr = '{ "[" + ${ Expr(allMemberCodecs.mkString(", ")) } + "]" }
    val allNamesConstName = '{ "all" + $valueType + "Names" }
    lazy val ordInst = '{
      $imports.lift("export const " + decap($valueType) + "Ord: ") |+|
        $imports.fptsOrd("Ord<" + $valueType + "U> = ") |+|
        $imports.fptsPipe(
          $imports.fptsString("Ord", Some("stringOrd")) |+|
            ", " |+|
            $imports.fptsOrd("contramap(x => x._tag)")
        ) |+|
        ";\n"
    }

    '{
      ${ Expr.ofList(memberCodecs) }.flatten |+| List((
        Some(TypeName(${ Expr(fullTypeName(typeRepr)) })),
        $imports.lift("export const all" + $valueType + "C = ") |+|
        ${ state.typeParamsTypes } |+|
        ${ state.typeParamsFnArgs } |+|
        (if (${ Expr(state.typeParamNames.nonEmpty) }) " => " else "") |+|
        ($allMemberCodecsArr + " as const;\n") |+|
        ("export const " + $allNamesConstName + " = [" +
          ${ Expr(memberNames.map(s => s"`$s`").mkString(", ")) } + "] as const;\n") |+|
        ("export type " + $valueType + "Name = (typeof " + $allNamesConstName + ")[number];\n") |+|
        "\n" |+|
        ${ state.wrapCodec(
          if (allMemberCodecs.length == 1) '{ $imports.lift(${ Expr(allMemberCodecs.head) }) }
          else '{ $imports.iotsUnion($allMemberCodecsArr) }
        ) } |+|
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
      ))
    }
  }

  @experimental
  private def generateCaseClass[A: Type](mirror: Mirror, state: GenerateState): Expr[List[(Option[TypeName], Generated)]] = {
    val typeRepr = TypeRepr.of[A]
    val typesAndLabels0 = mirror.types.toList.zip(mirror.labels).map {
      case (tpe, label) =>
        val res = tpe.asType match {
          case '[t] => '{ $imports.lift("  " + ${Expr(label)} + ": ") |+| ${ generate[t](state.copy(top = false)) }.foldMap(_._2) }
        }
        res
    }
    val typesAndLabels = Expr.ofList(
      if (state.inEnum)
        '{
          $imports.lift("  _tag: ") |+|
          $imports.iotsLiteral("`" + ${ Expr(baseTypeName(typeRepr)) } + "`")
        } :: typesAndLabels0
      else
        typesAndLabels0
    )

    val codec = state.wrapCodec('{
      $imports.iotsTypeFunction(
        $imports.lift("{\n") |+|
        intercalate($imports.lift(",\n"))($typesAndLabels) |+|
        "\n}"
      )
    })

    '{ List((Some(TypeName(${ Expr(fullTypeName(typeRepr)) })), $codec)) }
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
  def generate[A: Type](state: GenerateState): Expr[List[(Option[TypeName], Generated)]] = {
    val typeRepr = TypeRepr.of[A]
    val handleTop = (gen: Expr[Generated]) =>
      if (state.top) '{ List((Some(TypeName(${ Expr(fullTypeName(typeRepr)) })), ${ state.wrapCodec(gen) })) }
      else '{ List((None, $gen)) }
    typeRepr.asType match {
      case '[TypeParam[a]] =>
        TypeRepr.of[a] match {
          case ConstantType(c) => c.value match {
            case s: String => handleTop('{ $imports.lift(${ Expr(s) }) })
          }
        }: @annotation.nowarn("msg=match may not be exhaustive")

      case '[Json] => handleTop('{ $imports.iotsUnknown })
      case NumberType() => handleTop('{ $imports.iotsNumber })
      case '[BigDecimal] => handleTop('{ $imports.iotsBigNumber })
      case '[Boolean] => handleTop('{ $imports.iotsBoolean })
      case '[String] => handleTop('{ $imports.iotsString })
      case '[LocalDate] => handleTop('{ $imports.iotsLocalDate })
      case '[JLocalDate] => handleTop('{ $imports.iotsLocalDate })
      case '[DateTime] => handleTop('{ $imports.iotsDateTime })
      case '[LocalDateTime] => handleTop('{ $imports.iotsDateTime })
      case '[ZonedDateTime] => handleTop('{ $imports.iotsDateTime })
      case '[Instant] => handleTop('{ $imports.iotsDateTime })
      case '[Eval[a]] => handleTop('{ ${ generate[a](state.copy(top = false)) }.foldMap(_._2) })
      case '[Chain[a]] => handleTop('{ $imports.iotsReadonlyArray(${ generate[a](state.copy(top = false)) }.foldMap(_._2)) })
      case '[List[a]] => handleTop('{ $imports.iotsReadonlyArray(${ generate[a](state.copy(top = false)) }.foldMap(_._2)) })
      case '[Vector[a]] => handleTop('{ $imports.iotsReadonlyArray(${ generate[a](state.copy(top = false)) }.foldMap(_._2)) })
      case '[Seq[a]] => handleTop('{ $imports.iotsReadonlyArray(${ generate[a](state.copy(top = false)) }.foldMap(_._2)) })
      case '[Set[a]] => handleTop('{ $imports.iotsReadonlySetFromArray(${ generate[a](state.copy(top = false)) }.foldMap(_._2), ${ ordInstance[a] }) })
      case '[SortedSet[a]] => handleTop('{ $imports.iotsReadonlySetFromArray(${ generate[a](state.copy(top = false)) }.foldMap(_._2), ${ ordInstance[a] }) })
      case '[NonEmptyChain[a]] => handleTop('{ $imports.iotsReadonlyNonEmptyArray(${ generate[a](state.copy(top = false)) }.foldMap(_._2)) })
      case '[NonEmptyList[a]] => handleTop('{ $imports.iotsReadonlyNonEmptyArray(${ generate[a](state.copy(top = false)) }.foldMap(_._2)) })
      case '[NonEmptyVector[a]] => handleTop('{ $imports.iotsReadonlyNonEmptyArray(${ generate[a](state.copy(top = false)) }.foldMap(_._2)) })
      case '[scalaz.NonEmptyList[a]] => handleTop('{ $imports.iotsReadonlyNonEmptyArray(${ generate[a](state.copy(top = false)) }.foldMap(_._2)) })
      case '[Option[a]] => handleTop('{ $imports.iotsOption(${ generate[a](state.copy(top = false)) }.foldMap(_._2)) })
      case '[Either[l, r]] => handleTop('{ $imports.iotsEither(${ generate[l](state.copy(top = false)) }.foldMap(_._2), ${ generate[r](state.copy(top = false)) }.foldMap(_._2)) })
      case '[scalaz.\/[l, r]] => handleTop('{ $imports.iotsEither(${ generate[l](state.copy(top = false)) }.foldMap(_._2), ${ generate[r](state.copy(top = false)) }.foldMap(_._2)) })
      case '[Ior[l, r]] => handleTop('{ $imports.iotsThese(${ generate[l](state.copy(top = false)) }.foldMap(_._2), ${ generate[r](state.copy(top = false)) }.foldMap(_._2)) })
      case '[scalaz.\&/[l, r]] => handleTop('{ $imports.iotsThese(${ generate[l](state.copy(top = false)) }.foldMap(_._2), ${ generate[r](state.copy(top = false)) }.foldMap(_._2)) })
      case '[Map[k, v]] => handleTop('{ $imports.iotsRecord(${ tsRecordKeyType[k](state.copy(top = false)) }, ${ generate[v](state.copy(top = false)) }.foldMap(_._2)) })
      case _ if typeRepr <:< TypeRepr.of[Tuple] =>
        val tpes = typeRepr.typeArgs.map(_.asType match { case '[t] => generate[t](state.copy(top = false)) })
        handleTop('{ $imports.iotsTuple($imports.lift("[") |+| intercalate($imports.lift(", "))(${ Expr.ofList(tpes) }.map(_.foldMap(_._2))) |+| "]") })
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
              val tpeParamVals = Expr.ofList(params.map(_.asType match {
                case '[t] => '{ ${ generate[t](state.copy(top = false)) }.foldMap(_._2) }
              }))
              '{ $imports.lift("(") |+| intercalate($imports.lift(", "))($tpeParamVals) |+| ")" }

            case _ =>
              '{ Generated.empty }
          }

          '{
            List((None, $customType(${ Expr(typeRepr.show) })
              .getOrElse($imports.custom(
                TypeName(${ Expr(fullTypeName(typeRepr)) }),
                ${ Expr(mkCodecName(typeRepr)) }
              ) |+| $typeParams)))
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
    TypeRepr.of[TypeParam["A11"]],
    TypeRepr.of[TypeParam["A12"]],
    TypeRepr.of[TypeParam["A13"]],
    TypeRepr.of[TypeParam["A14"]],
    TypeRepr.of[TypeParam["A15"]],
    TypeRepr.of[TypeParam["A16"]],
    TypeRepr.of[TypeParam["A17"]],
    TypeRepr.of[TypeParam["A18"]],
    TypeRepr.of[TypeParam["A19"]],
    TypeRepr.of[TypeParam["A20"]],
    TypeRepr.of[TypeParam["A21"]],
    TypeRepr.of[TypeParam["A22"]],
  )

  @experimental
  def generateTopLevel[A: Type](inEnum: Boolean): Expr[List[(Option[TypeName], Generated)]] = {
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
    val valueType = Expr(cap(codecName).replaceAll("C(U?)$", "$1"))
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
                t => $imports.iotsTypeType |+| ("<" + t + ">")) |+|
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

  @experimental
  private def referencedTypeNamesEnum[A: Type](mirror: Mirror, currType: String): Map[String, Set[String]] =
    mirror.types.foldMap { t =>
      val typeName = fullTypeName(t)
      Map(currType -> Set(typeName)) |+| (t.asType match {
        case '[t] => Expr.summon[ValueOf[t]] match {
          case Some(_) => valMembers(t).foldMap(valDefType(t, _).asType match { case '[t] => referencedTypeNames[t](false, typeName) })
          case None => referencedTypeNames[t](true, typeName)
        }
      })
    }

  @experimental
  private def referencedTypeNamesCaseClass[A: Type](mirror: Mirror, currType: String): Map[String, Set[String]] =
    mirror.types.foldMap(_.asType match { case '[t] => referencedTypeNames[t](false, currType) })

  @experimental
  private def referencedTypeNames[A: Type](top: Boolean, currType: String): Map[String, Set[String]] = {
    val typeRepr = TypeRepr.of[A]
    typeRepr.asType match {
      case '[TypeParam[_]] => Map.empty
      case '[Json] => Map.empty
      case NumberType() => Map.empty
      case '[BigDecimal] => Map.empty
      case '[Boolean] => Map.empty
      case '[String] => Map.empty
      case '[LocalDate] => Map.empty
      case '[JLocalDate] => Map.empty
      case '[DateTime] => Map.empty
      case '[LocalDateTime] => Map.empty
      case '[ZonedDateTime] => Map.empty
      case '[Instant] => Map.empty
      case '[Eval[a]] => referencedTypeNames[a](false, currType)
      case '[Chain[a]] => referencedTypeNames[a](false, currType)
      case '[List[a]] => referencedTypeNames[a](false, currType)
      case '[Vector[a]] => referencedTypeNames[a](false, currType)
      case '[Seq[a]] => referencedTypeNames[a](false, currType)
      case '[Set[a]] => referencedTypeNames[a](false, currType)
      case '[SortedSet[a]] => referencedTypeNames[a](false, currType)
      case '[NonEmptyChain[a]] => referencedTypeNames[a](false, currType)
      case '[NonEmptyList[a]] => referencedTypeNames[a](false, currType)
      case '[NonEmptyVector[a]] => referencedTypeNames[a](false, currType)
      case '[scalaz.NonEmptyList[a]] => referencedTypeNames[a](false, currType)
      case '[Option[a]] => referencedTypeNames[a](false, currType)
      case '[Either[l, r]] => referencedTypeNames[l](false, currType) |+| referencedTypeNames[r](false, currType)
      case '[scalaz.\/[l, r]] => referencedTypeNames[l](false, currType) |+| referencedTypeNames[r](false, currType)
      case '[Ior[l, r]] => referencedTypeNames[l](false, currType) |+| referencedTypeNames[r](false, currType)
      case '[scalaz.\&/[l, r]] => referencedTypeNames[l](false, currType) |+| referencedTypeNames[r](false, currType)
      case '[Map[k, v]] => referencedTypeNames[k](false, currType) |+| referencedTypeNames[v](false, currType)
      case _ if typeRepr <:< TypeRepr.of[Tuple] => typeRepr.typeArgs.foldMap(_.asType match { case '[t] => referencedTypeNames[t](false, currType) })
      case _ =>
        if (top) {
          Mirror(typeRepr) match {
            case Some(m) =>
              m.mirrorType match {
                case MirrorType.Sum => referencedTypeNamesEnum[A](m, currType)
                case MirrorType.Product => referencedTypeNamesCaseClass[A](m, currType)
              }

            case None =>
              Map.empty
          }
        } else {
          Map(currType -> Set(fullTypeName(typeRepr))) |+| typeRepr.typeArgs.foldMap(_.asType match {
            case '[t] => referencedTypeNames[t](false, currType)
          })
        }
    }
  }

  @experimental
  def referencedTypes[A: Type]: Expr[ReferencedTypes] =
    '{
      ReferencedTypes(
        ${ Expr(referencedTypeNames[A](true, fullTypeName(TypeRepr.of[A]))) }
          .map { case (t, ts) => (TypeName(t), ts.map(TypeName(_))) }
      )
    }
}

object ScalaTs {
  private[ScalaTs] case class TypeParam[Name]()

  def intercalate(glue: Generated)(xs: List[Generated]): Generated =
    xs match {
      case Nil => Generated.empty
      case h :: t => t.foldLeft(h)((acc, x) => acc |+| glue |+| x)
    }

  def clsNme(a: Any): String = a.getClass.getSimpleName.stripSuffix("$")

  def cap(s: String): String = s.take(1).toUpperCase + s.drop(1)
  def decap(s: String): String = s.take(1).toLowerCase + s.drop(1)
  private val allCapsRx = """^[A-Z0-9_]+$""".r
  def maybeDecap(s: String): String =
    s match {
      case allCapsRx() => s
      case _ => decap(s)
    }
  def unionOrdName(s: String): String = decap(s) + "Ord"

  trait CustomType {
    def apply(name: String): Option[Generated]
  }

  object CustomType {
    val none = new CustomType {
      def apply(name: String) = None
    }
  }

  @experimental
  private def generateImpl[A: Type](
    customType: Expr[CustomType],
    imports: Expr[TsImports.available],
  )(using ctx: Quotes): Expr[(List[(Option[TypeName], Generated)], ReferencedTypes)] = {
    val sts = new ScalaTs(customType, imports)
    Expr.ofTuple((sts.generateTopLevel[A](false), sts.referencedTypes[A]))
  }

  inline def generate[A](
    inline customType: CustomType,
    inline imports: TsImports.available,
  ): (List[(Option[TypeName], Generated)], ReferencedTypes) =
    ${ generateImpl[A]('customType, 'imports) }

  @experimental
  private def referenceCodeImpl[A: Type](
    customType: Expr[CustomType],
    imports: Expr[TsImports.available],
  )(using ctx: Quotes): Expr[Generated] = {
    val sts = new ScalaTs(customType, imports)
    val gen = sts.generate[A](sts.GenerateState(false, false, Nil, '{ Generated.empty }, '{""}, identity))
    '{ $gen.foldMap(_._2) }
  }

  inline def referenceCode[A](
    inline customType: CustomType,
    inline imports: TsImports.available,
  ): Generated =
    ${ referenceCodeImpl[A]('customType, 'imports) }

  private def sortTypes(types: List[(Option[TypeName], Generated)], refs: ReferencedTypes): List[Generated] = {
    val all = types.flatMap { case (typeName, generated) => typeName.map((_, generated)) }.toMap

    def addType(
      acc: Eval[(List[Generated], Set[TypeName])],
      typeName: TypeName,
      generated: Generated,
    ): Eval[(List[Generated], Set[TypeName])] = {
      acc.flatMap { case (accGen, accSkip) =>
        if (accSkip.contains(typeName))
          Eval.now((accGen, accSkip))
        else
          refs.types.get(typeName) match {
            case Some(types) =>
              types.foldLeft(Eval.now((accGen, accSkip + typeName)))(
                (acc, ref) => all.get(ref).fold(acc)(addType(acc, ref, _))
              ).map { case (x, y) => (x :+ generated, y) }
            case None =>
              Eval.now((accGen :+ generated, accSkip + typeName))
          }
      }
    }

    types.foldLeft(Eval.now(List.empty[Generated], Set.empty[TypeName])) {
      case (acc, (Some(typeName), generated)) => addType(acc, typeName, generated)
      case (acc, (None, generated)) => acc.map { case (accGen, accSkip) => (accGen :+ generated, accSkip) }
    }.value._1
  }

  private def withFileWriter[A](file: File)(f: PrintStream => A): A = {
    new File(file.getParent).mkdirs
    Using.resource(new PrintStream(file))(f)
  }

  private def mkAllTypesByFile(allGenerated: List[(File, (List[(Option[TypeName], Generated)], ReferencedTypes))]): Map[String, Set[TypeName]] =
    allGenerated.foldMap { case (f, ts) => Map(f.toString -> ts._1.flatMap(_._1).toSet) }

  def resolve(
    currFile: File,
    generated: Generated,
    allGenerated: List[(File, (List[(Option[TypeName], Generated)], ReferencedTypes))],
  ): (Map[TsImport.Resolved, TsImport], String) = {
    val allTypesByFile = mkAllTypesByFile(allGenerated)
    val Generated(imports, code) = generated
    imports.resolve(currFile.toString, allTypesByFile, code)
  }

  def write(allGenerated: List[(File, (List[(Option[TypeName], Generated)], ReferencedTypes))]): Unit = {
    val allTypesByFile = mkAllTypesByFile(allGenerated)
    val all = allGenerated.foldMap { case (file, l) => Map(file.toString -> l) }.map { case (file, (types, refs)) =>
      val (allImports, allCode) = sortTypes(types, refs).foldMap { case Generated(imports, code) =>
        val (updImports, updCode) = imports.resolve(file.toString, allTypesByFile, code)
        (updImports, updCode + "\n\n")
      }
      (file, allImports.map { case (loc, imprt) =>
        s"""import ${imprt.str} from "${loc.location}";"""
      }.mkString("\n") + "\n\n" + allCode)
    }
    all.foreach { case (file, code) => withFileWriter(new File(file))(_.print(code)) }
  }
}
