package com.mpc.scalats.core

import scala.collection.immutable.ListSet
import scala.reflect.runtime.universe.Type

object TypeScriptModel {

  sealed trait Declaration {
    val name: String
    val superInterface: Option[InterfaceDeclaration]
  }

  sealed trait TypeRef

  case class CustomTypeRef(
    name: String,
    typeArgs: ListSet[TypeRef],
    scalaType: Type
  ) extends TypeRef

  case class ArrayRef(innerType: TypeRef) extends TypeRef

  case class NonEmptyArrayRef(innerType: TypeRef) extends TypeRef

  case class InterfaceDeclaration(name: String, fields: ListSet[Member], typeParams: ListSet[String], superInterface: Option[InterfaceDeclaration]) extends Declaration
  // TODO: Support mapping of typeParams with superInterface

  class Member(val name: String, val typeRef: TypeRef, val value: Option[Any])
  object Member {
    def apply(name: String, typeRef: TypeRef, value: Option[Any]): Member =
      new Member(name.trim, typeRef, value)

    def unapply(member: Member): Some[(String, TypeRef, Option[Any])] =
      Some((member.name, member.typeRef, member.value))
  }

  case class SingletonDeclaration(
    name: String,
    values: ListSet[Member],
    superInterface: Option[InterfaceDeclaration]
  ) extends Declaration

  case class UnionDeclaration(
    name: String,
    fields: ListSet[Member],
    possibilities: ListSet[CustomTypeRef],
    superInterface: Option[InterfaceDeclaration],
    emitAllConst: Boolean
  ) extends Declaration

  case class TypeDeclaration(
    name: String,
    typeRef: TypeRef,
    typeParams: ListSet[String]
  ) extends Declaration {
    val superInterface = None
  }

  case class UnknownTypeRef(name: String) extends TypeRef

  case object NumberRef extends TypeRef {
    override def toString = "number"
  }

  case object StringRef extends TypeRef {
    override def toString = "string"
  }

  case object BooleanRef extends TypeRef {
    override def toString = "boolean"
  }

  case object DateRef extends TypeRef {
    override def toString = "LocalDate"
  }

  case object DateTimeRef extends TypeRef {
    override def toString = "DateTime"
  }

  case object NullRef extends TypeRef {
    override def toString = "null"
  }

  case object UndefinedRef extends TypeRef {
    override def toString = "undefined"
  }

  case class SimpleTypeRef(name: String) extends TypeRef {
    override def toString = name
  }

  case class UnionType(possibilities: ListSet[TypeRef]) extends TypeRef

  case class EitherType(lT: TypeRef, rT: TypeRef) extends TypeRef

  case class TheseType(lT: TypeRef, rT: TypeRef) extends TypeRef

  case class MapType(keyType: TypeRef, valueType: TypeRef) extends TypeRef

  case class TupleType(types: ListSet[TypeRef]) extends TypeRef
}
