package scalats

import scala.quoted.*
import scala.util.chaining.*

case class TsParser()(using override val ctx: Quotes) extends ReflectionUtils {
  import ctx.reflect.*

  private def parseEnum[A: Type](mirror: Mirror): Expr[TsModel] = {
    val typeRepr = TypeRepr.of[A]

    def parseMembers(m: Mirror): List[Expr[TsModel.Object | TsModel.Interface]] = {
      m.mirrorType match {
        case MirrorType.Sum =>
          m.types.toList.flatMap(Mirror(_).fold(Nil)(parseMembers))

        case MirrorType.Product =>
          List(m.mirroredType.asType match { case '[t] => parseCaseClass[t](m, Some(typeRepr)) })

        case MirrorType.Singleton =>
          List(m.mirroredType.asType match { case '[t] => parseObject[t](Some(typeRepr)) })
      }
    }

    '{
      TsModel.Union(
        TypeName(${ Expr(typeRepr.show) }),
        ${ Expr.ofList(typeRepr.typeArgs.map(_.asType match { case '[t] => parse[t](false) })) },
        ${ Expr.ofList(parseMembers(mirror)) }
      )
    }
  }

  private def parseCaseClass[A: Type](mirror: Mirror, parent: Option[TypeRepr]): Expr[TsModel.Interface] = {
    val typeRepr = TypeRepr.of[A]
    val fields = mirror.types.toList.zip(mirror.labels).map { case (tpe, name) =>
      tpe.asType match { case '[t] => '{ TsModel.InterfaceField(${ Expr(name) }, ${ parse[t](false) }) } }
    }
    '{
      TsModel.Interface(
        TypeName(${ Expr(typeRepr.show) }),
        ${ Expr(parent.map(_.show)) }.map(TypeName(_)),
        ${ Expr.ofList(typeRepr.typeArgs.map(_.asType match { case '[t] => parse[t](false) })) },
        ${ Expr.ofList(fields) },
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

  private def parseObject[A: Type](parent: Option[TypeRepr]): Expr[TsModel.Object] = {
    val typeRepr = TypeRepr.of[A]
    val value = Expr.summon[ValueOf[A]].getOrElse(report.errorAndAbort(s"Unable to summon `ValueOf[${typeRepr.show}]`"))
    val members = valMembers(typeRepr)
    val fields = Expr.ofList(members.map(s => valDefType(typeRepr, s).asType match {
      case '[a] => '{ TsModel.ObjectField(${ Expr(s.name) }, ${ parse[a](false) }, ${ Select('{ $value.value }.asTerm, s).asExpr }) }
    }))
    '{
      TsModel.Object(
        TypeName(${ Expr(typeRepr.show) }),
        ${ Expr(parent.map(_.show)) }.map(TypeName(_)),
        $fields,
      )
    }
  }

  def parse[A: Type](top: Boolean): Expr[TsModel] = {
    val typeRepr = TypeRepr.of[A]
    val typeName = '{ TypeName(${ Expr(typeRepr.show) }) }

    typeRepr.asType match {
      case '[Nothing] => '{ TsModel.Unknown($typeName, Nil) }
      case '[TypeParam[a]] =>
        val name = TypeRepr.of[a] match {
          case ConstantType(c) => c.value match { case s: String => s }
        }: @annotation.nowarn("msg=match may not be exhaustive")
        '{ TsModel.TypeParam(${ Expr(name) }) }
      case '[io.circe.Json] => '{ TsModel.Json($typeName) }
      case '[Byte] | '[Short] | '[Int] | '[Long] | '[Double] | '[Float] => '{ TsModel.Number($typeName) }
      case '[BigDecimal] => '{ TsModel.BigNumber($typeName) }
      case '[Boolean] => '{ TsModel.Boolean($typeName) }
      case '[String] => '{ TsModel.String($typeName) }
      case '[java.time.LocalDate] | '[org.joda.time.LocalDate] => '{ TsModel.LocalDate($typeName) }
      case '[java.time.LocalDateTime] | '[java.time.ZonedDateTime] | '[java.time.Instant] | '[org.joda.time.DateTime] => '{ TsModel.DateTime($typeName) }
      case '[cats.Eval[a]] => '{ TsModel.Eval($typeName, ${ parse[a](false) }) }
      case '[cats.data.Chain[a]] => '{ TsModel.Array($typeName, ${ parse[a](false) }, (_: Any).asInstanceOf[cats.data.Chain[Any]].toList) }
      case '[List[a]] => '{ TsModel.Array($typeName, ${ parse[a](false) }, (_: Any).asInstanceOf[List[Any]]) }
      case '[Vector[a]] => '{ TsModel.Array($typeName, ${ parse[a](false) }, (_: Any).asInstanceOf[Vector[Any]].toList) }
      case '[Seq[a]] => '{ TsModel.Array($typeName, ${ parse[a](false) }, (_: Any).asInstanceOf[Seq[Any]].toList) }
      case '[Set[a]] => '{ TsModel.Set($typeName, ${ parse[a](false) }) }
      case '[collection.immutable.SortedSet[a]] => '{ TsModel.Set($typeName, ${ parse[a](false) }) }
      case '[cats.data.NonEmptyChain[a]] => '{ TsModel.NonEmptyArray($typeName, ${ parse[a](false) }, (_: Any).asInstanceOf[cats.data.NonEmptyChain[Any]].toNonEmptyList) }
      case '[cats.data.NonEmptyList[a]] => '{ TsModel.NonEmptyArray($typeName, ${ parse[a](false) }, (_: Any).asInstanceOf[cats.data.NonEmptyList[Any]]) }
      case '[cats.data.NonEmptyVector[a]] => '{ TsModel.NonEmptyArray($typeName, ${ parse[a](false) }, (_: Any).asInstanceOf[cats.data.NonEmptyChain[Any]].toNonEmptyList) }
      case '[scalaz.NonEmptyList[a]] => '{ TsModel.NonEmptyArray($typeName, ${ parse[a](false) }, (_: Any).asInstanceOf[scalaz.NonEmptyList[Any]].pipe(l => cats.data.NonEmptyList(l.head, l.tail.toList))) }
      case '[Option[a]] => '{ TsModel.Option($typeName, ${ parse[a](false) }) }
      case '[Either[l, r]] => '{ TsModel.Either($typeName, ${ parse[l](false) }, ${ parse[r](false) }, (_: Any).asInstanceOf[Either[Any, Any]]) }
      case '[scalaz.\/[l, r]] => '{ TsModel.Either($typeName, ${ parse[l](false) }, ${ parse[r](false) }, (_: Any).asInstanceOf[scalaz.\/[Any, Any]].toEither) }
      case '[cats.data.Ior[l, r]] => '{ TsModel.Ior($typeName, ${ parse[l](false) }, ${ parse[r](false) }, (_: Any).asInstanceOf[cats.data.Ior[Any, Any]]) }
      case '[scalaz.\&/[l, r]] => '{
        TsModel.Ior($typeName, ${ parse[l](false) }, ${ parse[r](false) }, (_: Any).asInstanceOf[scalaz.\&/[Any, Any]]
          .fold(cats.data.Ior.Left(_), cats.data.Ior.Right(_), cats.data.Ior.Both(_, _)))
      }
      case '[Map[k, v]] => '{ TsModel.Map($typeName, ${ parse[k](false) }, ${ parse[v](false) }) }
      case '[EmptyTuple] => '{ TsModel.Tuple($typeName, Nil) }
      case '[*:[h, t]] =>
        def unroll[T <: Tuple: Type]: List[Expr[TsModel]] =
          Type.of[T] match {
            case '[*:[h, t]] => parse[h](false) :: unroll[t]
            case '[EmptyTuple] => Nil
          }
        '{ TsModel.Tuple($typeName, ${ Expr.ofList(unroll[h *: t]) }) }
      case '[t] if typeRepr <:< TypeRepr.of[Tuple] =>
        '{ TsModel.Tuple($typeName, ${ Expr.ofList(TypeRepr.of[t].typeArgs.map(_.asType match { case '[a] => parse[a](false) })) }) }
      case '[t] =>
        lazy val typeArgs = Expr.ofList(typeRepr.typeArgs.map(_.asType match { case '[a] => parse[a](false) }))
        Mirror(typeRepr) match {
          case Some(m) =>
            m.mirrorType match {
              case MirrorType.Sum => if (top) parseEnum[t](m) else '{ TsModel.UnionRef($typeName, $typeArgs) }
              case MirrorType.Product => if (top) parseCaseClass[t](m, None) else '{ TsModel.InterfaceRef($typeName, $typeArgs) }
              case MirrorType.Singleton => if (top) parseObject[t](None) else '{ TsModel.ObjectRef($typeName) }
            }

          case None =>
            '{ TsModel.Unknown($typeName, $typeArgs) }
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

  def parseTopLevel[A: Type]: Expr[TsModel] =
    TypeRepr.of[A] match {
      case AppliedType(tpe, params) if params.nonEmpty =>
        AppliedType(tpe, params.zipWithIndex.map { case (_, i) => typeParamTypes(i) }).asType match {
          case '[t] => parse[t](true)
        }
      case _ =>
        parse[A](true)
    }
}
