package org.scalajs.linker.backend.wasmemitter

import org.scalajs.ir.Names._
import org.scalajs.ir.Types._

import org.scalajs.linker.backend.webassembly.{Types => watpe}

import VarGen._

object TypeTransformer {

  /** This transformation should be used only for the result types of functions or blocks.
    *
    * `nothing` translates to an empty result type list, because Wasm does not have a bottom type
    * (at least not one that can expressed at the user level). A block or function call that returns
    * `nothing` should typically be followed by an extra `unreachable` statement to recover a
    * stack-polymorphic context.
    *
    * @see
    *   https://webassembly.github.io/spec/core/syntax/types.html#result-types
    */
  def transformResultType(t: Type)(implicit globalKnowledge: GlobalKnowledge): List[watpe.Type] = {
    t match {
      case NoType      => Nil
      case NothingType => Nil
      case _           => List(transformType(t))
    }
  }

  /** Transforms a value type to a unique Wasm type.
    *
    * This method cannot be used for `void` and `nothing`, since they have no corresponding Wasm
    * value type.
    */
  def transformType(t: Type)(implicit globalKnowledge: GlobalKnowledge): watpe.Type = {
    t match {
      case AnyType => watpe.RefType.anyref

      case tpe: ArrayType =>
        watpe.RefType.nullable(genTypeID.forArrayClass(tpe.arrayTypeRef))

      case ClassType(className)   => transformClassType(className)
      case RecordType(fields)     => ???
      case StringType | UndefType => watpe.RefType.any
      case p: PrimTypeWithRef     => transformPrimType(p)
    }
  }

  def transformClassType(
      className: ClassName
  )(implicit globalKnowledge: GlobalKnowledge): watpe.RefType = {
    if (globalKnowledge.isAncestorOfHijackedClass(className))
      watpe.RefType.anyref
    else if (globalKnowledge.isInterface(className))
      watpe.RefType.nullable(genTypeID.ObjectStruct)
    else
      watpe.RefType.nullable(genTypeID.forClass(className))
  }

  private def transformPrimType(t: PrimTypeWithRef): watpe.Type = {
    t match {
      case BooleanType => watpe.Int32
      case ByteType    => watpe.Int32
      case ShortType   => watpe.Int32
      case IntType     => watpe.Int32
      case CharType    => watpe.Int32
      case LongType    => watpe.Int64
      case FloatType   => watpe.Float32
      case DoubleType  => watpe.Float64
      case NullType    => watpe.RefType.nullref

      case NoType | NothingType =>
        throw new IllegalArgumentException(s"${t.show()} does not have a corresponding Wasm type")
    }
  }
}
