package com.mpc.scalats.core

import scala.collection.immutable.ListSet
import scala.reflect.runtime.universe.Type

// TODO: ValueClass

object ScalaModel {
  sealed trait TypeDef {
    def name: String
    def fullTypeName: String
    def scalaType: Type
  }

  case class CaseClass(
    name: String,
    fullTypeName: String,
    fields: ListSet[TypeMember],
    values: ListSet[TypeMember],
    typeArgs: ListSet[String],
    scalaType: Type
  ) extends TypeDef

  case class CaseObject(
    name: String,
    fullTypeName: String,
    values: ListSet[TypeMember],
    scalaType: Type
  ) extends TypeDef

  case class SealedUnion(
    name: String,
    fullTypeName: String,
    fields: ListSet[TypeMember],
    possibilities: ListSet[TypeDef],
    scalaType: Type
  ) extends TypeDef

  case class TypeAlias(
    name: String,
    fullTypeName: String,
    typeRef: TypeRef,
    typeArgs: ListSet[String],
    scalaType: Type
  ) extends TypeDef

  // ---

  sealed trait TypeRef
  sealed trait HasType {
    def name: String
    def tpe: Type
  }

  case class OptionRef(innerType: TypeRef) extends TypeRef

  case class UnionRef(name: String, possibilities: ListSet[TypeRef], tpe: Type) extends TypeRef with HasType

  case class EitherRef(leftType: TypeRef, rightType: TypeRef) extends TypeRef

  case class MapRef(keyType: TypeRef, valueType: TypeRef) extends TypeRef

  case class CaseClassRef(name: String, fullTypeName: String, typeArgs: ListSet[TypeRef], tpe: Type) extends TypeRef with HasType

  case class SeqRef(innerType: TypeRef) extends TypeRef

  case class TupleRef(typeArgs: ListSet[TypeRef]) extends TypeRef

  case class TheseRef(leftType: TypeRef, rightType: TypeRef) extends TypeRef

  case class NonEmptySeqRef(innerType: TypeRef) extends TypeRef

  case class TypeMember(name: String, typeRef: TypeRef, value: Option[Any] = None)

  case class UnknownTypeRef(name: String, tpe: Type) extends TypeRef with HasType

  case class TypeParamRef(name: String, tpe: Type) extends TypeRef with HasType

  case object IntRef extends TypeRef

  case object LongRef extends TypeRef

  case object DoubleRef extends TypeRef

  case object BooleanRef extends TypeRef

  case object StringRef extends TypeRef

  case object DateRef extends TypeRef

  case object DateTimeRef extends TypeRef
}
