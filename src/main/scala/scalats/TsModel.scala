package scalats

/**
 * Representation of all the TypeScript types we support generating code for
 *
 * Note: `interface`s, `object`s, and `union`s are represented one of two ways:
 *
 *   1. As the type definition itself via [[scalats.TsModel.Interface]], [[scalats.TsModel.Object]], and [[scalats.TsModel.Union]]
 *   2. As a reference to the type via [[scalats.TsModel.InterfaceRef]], [[scalats.TsModel.ObjectRef]], and [[scalats.TsModel.UnionRef]]
 *
 * This is done to prevent unnecessary parsing in [[scalats.TsParser]].
 * The full type definition is only needed when parsing the type itself,
 * we only need a reference to the type when parsing a type that refers to it.
 */
sealed trait TsModel {
  val typeName: TypeName
  val typeArgs: List[TsModel]

  final def withTypeEls(typeName: TypeName, typeArgs: List[TsModel]): TsModel =
    this match {
      case x: TsModel.TypeParam => x
      case x: TsModel.Literal => x
      case x: TsModel.Json => x.copy(typeName = typeName)
      case x: TsModel.Number => x.copy(typeName = typeName)
      case x: TsModel.BigNumber => x.copy(typeName = typeName)
      case x: TsModel.Boolean => x.copy(typeName = typeName)
      case x: TsModel.String => x.copy(typeName = typeName)
      case x: TsModel.LocalDate => x.copy(typeName = typeName)
      case x: TsModel.DateTime => x.copy(typeName = typeName)
      case x: TsModel.UUID => x.copy(typeName = typeName)
      case x: TsModel.Eval => x.copy(typeName = typeName)
      case x: TsModel.Array => x.copy(typeName = typeName)
      case x: TsModel.Set => x.copy(typeName = typeName)
      case x: TsModel.NonEmptyArray => x.copy(typeName = typeName)
      case x: TsModel.Option => x.copy(typeName = typeName)
      case x: TsModel.Either => x.copy(typeName = typeName)
      case x: TsModel.Ior => x.copy(typeName = typeName)
      case x: TsModel.Map => x.copy(typeName = typeName)
      case x: TsModel.Tuple => x.copy(typeName = typeName)
      case x: TsModel.Interface => x.copy(typeName = typeName, typeArgs = typeArgs)
      case x: TsModel.InterfaceRef => x.copy(typeName = typeName, typeArgs = typeArgs)
      case x: TsModel.Object => x.copy(typeName = typeName)
      case x: TsModel.ObjectRef => x.copy(typeName = typeName)
      case x: TsModel.Union => x.copy(typeName = typeName, typeArgs = typeArgs)
      case x: TsModel.UnionRef => x.copy(typeName = typeName, typeArgs = typeArgs)
      case x: TsModel.Unknown => x.copy(typeName = typeName)
    }
}

object TsModel {
  sealed trait Field {
    val name: scala.Predef.String
    val tpe: TsModel
  }
  case class ObjectField(name: scala.Predef.String, tpe: TsModel, value: Any) extends Field
  case class InterfaceField(name: scala.Predef.String, tpe: TsModel) extends Field

  case class TypeParam(name: scala.Predef.String) extends TsModel {
    val typeName = TypeName(name)
    val typeArgs = Nil
  }
  case class Literal(tpe: TsModel, value: Any) extends TsModel {
    val typeName = tpe.typeName
    val typeArgs = Nil
  }
  case class Json(typeName: TypeName) extends TsModel {
    val typeArgs = Nil
  }
  case class Number(typeName: TypeName) extends TsModel {
    val typeArgs = Nil
  }
  case class BigNumber(typeName: TypeName) extends TsModel {
    val typeArgs = Nil
  }
  case class Boolean(typeName: TypeName) extends TsModel {
    val typeArgs = Nil
  }
  case class String(typeName: TypeName) extends TsModel {
    val typeArgs = Nil
  }
  case class LocalDate(typeName: TypeName) extends TsModel {
    val typeArgs = Nil
  }
  case class DateTime(typeName: TypeName) extends TsModel {
    val typeArgs = Nil
  }
  case class UUID(typeName: TypeName) extends TsModel {
    val typeArgs = Nil
  }
  case class Eval(typeName: TypeName, tpe: TsModel) extends TsModel {
    val typeArgs = Nil
  }
  case class Array(typeName: TypeName, tpe: TsModel, toList: Any => List[Any]) extends TsModel {
    val typeArgs = List(tpe)
  }
  case class Set(typeName: TypeName, tpe: TsModel) extends TsModel {
    val typeArgs = List(tpe)
  }
  case class NonEmptyArray(typeName: TypeName, tpe: TsModel, toNel: Any => cats.data.NonEmptyList[Any]) extends TsModel {
    val typeArgs = List(tpe)
  }
  case class Option(typeName: TypeName, tpe: TsModel) extends TsModel {
    val typeArgs = List(tpe)
  }
  case class Either(typeName: TypeName, left: TsModel, right: TsModel, toEither: Any => scala.Either[Any, Any]) extends TsModel {
    val typeArgs = List(left, right)
  }
  case class Ior(typeName: TypeName, left: TsModel, right: TsModel, toIor: Any => cats.data.Ior[Any, Any]) extends TsModel {
    val typeArgs = List(left, right)
  }
  case class Map(typeName: TypeName, key: TsModel, value: TsModel) extends TsModel {
    val typeArgs = List(key, value)
  }
  case class Tuple(typeName: TypeName, tpes: List[TsModel]) extends TsModel {
    val typeArgs = tpes
  }
  case class Interface(typeName: TypeName, parent: scala.Option[TypeName], typeArgs: List[TsModel], fields: List[TsModel.InterfaceField]) extends TsModel
  case class InterfaceRef(typeName: TypeName, typeArgs: List[TsModel]) extends TsModel
  case class Object(typeName: TypeName, parent: scala.Option[TypeName], fields: List[TsModel.ObjectField]) extends TsModel {
    val typeArgs = Nil
  }
  case class ObjectRef(typeName: TypeName) extends TsModel {
    val typeArgs = Nil
  }
  case class Union(typeName: TypeName, typeArgs: List[TsModel], possibilities: List[Object | Interface]) extends TsModel
  case class UnionRef(typeName: TypeName, typeArgs: List[TsModel], allObjects: scala.Boolean) extends TsModel
  case class Unknown(typeName: TypeName, typeArgs: List[TsModel]) extends TsModel
}
