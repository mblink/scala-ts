package scalats

import scala.annotation.tailrec
import scala.quoted.*
import scala.util.chaining.*

trait ReflectionUtils {
  implicit val ctx: Quotes

  import ctx.reflect.*

  /** An `enum` representing the three different kinds of `scala.deriving.Mirror`s */
  enum MirrorType {
    case Sum
    case Product
    case Singleton
  }

  object MirrorType {
    /** Parse a [[scalats.ReflectionUtils.MirrorType]] from a `scala.deriving.Mirror` */
    def from(mirror: Expr[scala.deriving.Mirror]): MirrorType =
      mirror match {
        case '{ ${_}: scala.deriving.Mirror.Singleton } => MirrorType.Singleton
        case '{ ${_}: scala.deriving.Mirror.Product } => MirrorType.Product
        case '{ ${_}: scala.deriving.Mirror.Sum } => MirrorType.Sum
      }
  }

  /**
   * A compile-time representation of a `scala.deriving.Mirror` using `scala.quoted.Quotes.TypeRepr`s
   * in place of actual types.
   */
  case class Mirror(
    mirroredType: TypeRepr,
    monoType: TypeRepr,
    types: List[TypeRepr],
    label: String,
    labels: List[String],
    mirrorType: MirrorType
  )

  object Mirror {
    /** Parse a [[scalats.ReflectionUtils.Mirror]] from a `scala.deriving.Mirror` */
    def apply(mirror: Expr[scala.deriving.Mirror]): Option[Mirror] = {
      val mirrorTpe = mirror.asTerm.tpe.widen
      for {
        mt   <- findMemberType(mirrorTpe, "MirroredType")
        mmt  <- findMemberType(mirrorTpe, "MirroredMonoType")
        mets <- findMemberType(mirrorTpe, "MirroredElemTypes").map(tupleTypeElements(_)).orElse(Some(Nil))
        ml   <- findMemberType(mirrorTpe, "MirroredLabel")
        mels <- findMemberType(mirrorTpe, "MirroredElemLabels").map { mels =>
            tupleTypeElements(mels).map { case ConstantType(StringConstant(l)) => l }
          }.orElse(Some(Nil))
      } yield {
        val ConstantType(StringConstant(ml0)) = ml: @unchecked
        Mirror(mt, mmt, mets, ml0, mels, MirrorType.from(mirror))
      }
    }

    /** Parse a [[scalats.ReflectionUtils.Mirror]] from a `scala.quoted.Quotes.TypeRepr` */
    def apply(tpe: TypeRepr): Option[Mirror] =
      tpe.asType match { case '[t] => Expr.summon[scala.deriving.Mirror.Of[t]].flatMap(Mirror(_)) }
  }

  private def tupleTypeElements(tp: TypeRepr): List[TypeRepr] = {
    @tailrec def loop(tp: TypeRepr, acc: List[TypeRepr]): List[TypeRepr] = tp match {
      case AppliedType(_, List(hd: TypeRepr, tl: TypeRepr)) => loop(tl, hd :: acc)
      case _ => acc
    }
    loop(tp, Nil).reverse
  }

  private def low(tp: TypeRepr): TypeRepr = tp match {
    case tp: TypeBounds => tp.low
    case tp => tp
  }

  private def findMemberType(tp: TypeRepr, name: String): Option[TypeRepr] = tp match {
    case Refinement(_, `name`, tp) => Some(low(tp))
    case Refinement(parent, _, _) => findMemberType(parent, name)
    case AndType(left, right) => findMemberType(left, name).orElse(findMemberType(right, name))
    case _ => None
  }
}

object HListRecordType {
  private class ConcreteLabelled[K, V]

  def unapply[A](tpe: Type[A])(using ctx: Quotes): Option[(List[(String, ctx.reflect.TypeRepr)])] = {
    import ctx.reflect.*
    import formless.hlist.{::, HNil}
    import formless.record.->>

    val typeRepr = tpe.pipe(implicit t => TypeRepr.of[A])
    val labelled = TypeRepr.of[->>]
    val labelledSym = labelled.typeSymbol
    val concreteLabelled = TypeRepr.of[ConcreteLabelled]
    val concreteLabelledSym = concreteLabelled.typeSymbol

    def concrete(t: TypeRepr): TypeRepr = t.substituteTypes(List(labelledSym), List(concreteLabelled))
    def unConcrete(t: TypeRepr): TypeRepr = t.substituteTypes(List(concreteLabelledSym), List(labelled))

    @tailrec def fieldTypes(t: TypeRepr, acc: List[(String, TypeRepr)]): Option[List[(String, TypeRepr)]] =
      t.asType match {
        case '[HNil] => Some(acc.reverse)
        case '[ConcreteLabelled[k, v] :: t] =>
          TypeRepr.of[k] match {
            case ConstantType(StringConstant(k)) =>
              fieldTypes(TypeRepr.of[t], (k, unConcrete(TypeRepr.of[v])) :: acc)
            case _ =>
              None
          }
        case _ =>
          None
      }

    fieldTypes(concrete(typeRepr), Nil).orElse(fieldTypes(concrete(typeRepr.dealias), Nil))
  }
}
