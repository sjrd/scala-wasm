package org.scalajs.linker.backend.wasmemitter

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees.{AnyFieldDef, MethodDef, JSNativeLoadSpec}
import org.scalajs.ir.Types.Type

import org.scalajs.linker.standard.ModuleSet.ModuleID

private[wasmemitter] trait GlobalKnowledge {

  /** Tests whether the specified class name refers to an `Interface`. */
  def isInterface(className: ClassName): Boolean

  /** All the `FieldDef`s, included inherited ones, of a Scala class.
    *
    * It is invalid to call this method with anything but a `Class` or `ModuleClass`.
    */
  def getAllScalaClassFieldDefs(className: ClassName): List[AnyFieldDef]

  /** Tests whether the specified class has any instances at all. */
  def hasInstances(className: ClassName): Boolean

  /** Gets the types of the `jsClassCaptures` of the given class. */
  def getJSClassCaptureTypes(className: ClassName): Option[List[Type]]

  /** `None` for non-native JS classes/objects; `Some(spec)` for native JS classes/objects.
    *
    * It is invalid to call this method with a class that is not a JS class or object (native or
    * not), or one that has JS class captures.
    */
  def getJSNativeLoadSpec(className: ClassName): Option[JSNativeLoadSpec]

  /** The JS native load spec of a native JS member. */
  def getJSNativeLoadSpec(className: ClassName, member: MethodName): JSNativeLoadSpec

  /** The global variables that mirror a given static field. */
  def getStaticFieldMirrors(field: FieldName): List[String]

  /** Equivalent to `hijackedDescendants(className).nonEmpty` but more efficient. */
  def isAncestorOfHijackedClass(className: ClassName): Boolean

  def resolveConcreteMethod(
      className: ClassName,
      methodName: MethodName
  ): Option[WasmContext.ConcreteMethodInfo]

  def getItableIdx(className: ClassName): Int
}
