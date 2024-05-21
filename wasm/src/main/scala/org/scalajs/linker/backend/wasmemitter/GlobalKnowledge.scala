package org.scalajs.linker.backend.wasmemitter

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees.{AnyFieldDef, MethodDef, JSNativeLoadSpec}
import org.scalajs.ir.Types.Type

import org.scalajs.linker.standard.ModuleSet.ModuleID

import VarGen._

private[wasmemitter] trait GlobalKnowledge {

  /** Tests whether the specified class name refers to an `Interface`. */
  def isInterface(className: ClassName): Boolean

  /** Tests whether the specified class name refers to a JS type. */
  def isJSType(className: ClassName): Boolean

  /** All the `FieldDef`s, included inherited ones, of a Scala class.
    *
    * It is invalid to call this method with anything but a `Class` or `ModuleClass`.
    */
  def getAllScalaClassFieldDefs(className: ClassName): List[AnyFieldDef]

  /** Tests whether the specified class has any instances at all. */
  def hasInstances(className: ClassName): Boolean

  /** Tests whether the specified class has run-time type information. */
  def hasRuntimeTypeInfo(className: ClassName): Boolean

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

  /** The first ancestor class of the given class that provides a concrete implementation of the given method. */
  def resolveConcreteMethod(className: ClassName, methodName: MethodName): Option[ClassName]

  /** Is the given concrete method effectively final, i.e., never overridden? */
  def isMethodEffectivelyFinal(className: ClassName, methodName: MethodName): Boolean

  /** The number of elements in itables arrays. */
  def getItablesLength: Int

  /** The itable index of the given interface. */
  def getItableIdx(className: ClassName): Int

  /** Retrieves a unique identifier for a reflective proxy with the given name.
    *
    * If no class defines a reflective proxy with the given name, returns `-1`.
    */
  def getReflectiveProxyId(name: MethodName): Int

  /** The method table entries in the vtable/itable of the given class/interface. */
  def getTableEntries(className: ClassName): List[MethodName]
}

private[wasmemitter] object GlobalKnowledge {

  final class ConcreteMethodInfo(val ownerClass: ClassName, val methodName: MethodName) {
    val tableEntryName = genFunctionID.forTableEntry(ownerClass, methodName)

    private var effectivelyFinal: Boolean = true

    private[wasmemitter] def markOverridden(): Unit =
      effectivelyFinal = false

    def isEffectivelyFinal: Boolean = effectivelyFinal
  }

}
