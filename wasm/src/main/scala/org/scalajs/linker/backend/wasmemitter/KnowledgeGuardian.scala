package org.scalajs.linker.backend.wasmemitter

import scala.collection.mutable

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import org.scalajs.ir.{ClassKind, Traversers}

import org.scalajs.linker.standard.{LinkedClass, LinkedTopLevelExport}

import EmbeddedConstants._
import GlobalKnowledge.ConcreteMethodInfo
import WasmContext._

final class KnowledgeGuardian(
    classInfo: Map[ClassName, KnowledgeGuardian.ClassInfo],
    reflectiveProxies: Map[MethodName, Int],
    itablesLength: Int
) extends GlobalKnowledge {
  import KnowledgeGuardian._

  def isInterface(className: ClassName): Boolean =
    classInfo(className).isInterface

  def isJSType(className: ClassName): Boolean =
    classInfo(className).kind.isJSType

  def getAllScalaClassFieldDefs(className: ClassName): List[FieldDef] =
    classInfo(className).allFieldDefs

  def hasInstances(className: ClassName): Boolean =
    classInfo(className).hasInstances

  def hasRuntimeTypeInfo(className: ClassName): Boolean =
    classInfo.get(className).exists(_.hasRuntimeTypeInfo)

  def getJSClassCaptureTypes(className: ClassName): Option[List[Type]] =
    classInfo(className).jsClassCaptures.map(_.map(_.ptpe))

  def getJSNativeLoadSpec(className: ClassName): Option[JSNativeLoadSpec] =
    classInfo(className).jsNativeLoadSpec

  def getJSNativeLoadSpec(className: ClassName, member: MethodName): JSNativeLoadSpec =
    classInfo(className).jsNativeMembers(member)

  def getStaticFieldMirrors(field: FieldName): List[String] =
    classInfo(field.className).staticFieldMirrors(field)

  def isAncestorOfHijackedClass(className: ClassName): Boolean =
    classInfo(className).isAncestorOfHijackedClass

  def resolveConcreteMethod(className: ClassName, methodName: MethodName): Option[ClassName] =
    classInfo(className).resolvedMethodInfos.get(methodName).map(_.ownerClass)

  /** Is the given concrete method effectively final, i.e., never overridden? */
  def isMethodEffectivelyFinal(className: ClassName, methodName: MethodName): Boolean =
    classInfo(className).resolvedMethodInfos(methodName).isEffectivelyFinal

  /** The number of elements in itables arrays. */
  def getItablesLength: Int =
    itablesLength

  def getItableIdx(className: ClassName): Int =
    classInfo(className).itableIdx

  def getReflectiveProxyId(name: MethodName): Int =
    reflectiveProxies.getOrElse(name, -1)

  def getTableEntries(className: ClassName): List[MethodName] =
    classInfo(className).tableEntries

  // Not part of GlobalKnowledge API

  def getClassInfo(className: ClassName): ClassInfo =
    classInfo(className)

  def getClassInfoOption(className: ClassName): Option[ClassInfo] =
    classInfo.get(className)
}

object KnowledgeGuardian {
  final class ClassInfo(
      val name: ClassName,
      val kind: ClassKind,
      val jsClassCaptures: Option[List[ParamDef]],
      classConcretePublicMethodNames: List[MethodName],
      val allFieldDefs: List[FieldDef],
      superClass: Option[ClassInfo],
      val classImplementsAnyInterface: Boolean,
      private var _hasInstances: Boolean,
      val isAbstract: Boolean,
      val hasRuntimeTypeInfo: Boolean,
      val jsNativeLoadSpec: Option[JSNativeLoadSpec],
      val jsNativeMembers: Map[MethodName, JSNativeLoadSpec],
      val staticFieldMirrors: Map[FieldName, List[String]],
      private var _itableIdx: Int
  ) {
    val resolvedMethodInfos: Map[MethodName, ConcreteMethodInfo] = {
      if (kind.isClass || kind == ClassKind.HijackedClass) {
        val inherited: Map[MethodName, ConcreteMethodInfo] = superClass match {
          case Some(superClass) => superClass.resolvedMethodInfos
          case None             => Map.empty
        }

        for (methodName <- classConcretePublicMethodNames)
          inherited.get(methodName).foreach(_.markOverridden())

        classConcretePublicMethodNames.foldLeft(inherited) { (prev, methodName) =>
          prev.updated(methodName, new ConcreteMethodInfo(name, methodName))
        }
      } else {
        Map.empty
      }
    }

    private val methodsCalledDynamically = mutable.HashSet.empty[MethodName]

    /** For a class or interface, its table entries in definition order. */
    private var _tableEntries: List[MethodName] = null

    // See caller in Preprocessor.preprocess
    def setHasInstances(): Unit =
      _hasInstances = true

    def hasInstances: Boolean = _hasInstances

    def setItableIdx(idx: Int): Unit = _itableIdx = idx

    /** Returns the index of this interface's itable in the classes' interface tables.
      */
    def itableIdx: Int = _itableIdx

    private var _specialInstanceTypes: Int = 0

    def addSpecialInstanceType(jsValueType: Int): Unit =
      _specialInstanceTypes |= (1 << jsValueType)

    /** A bitset of the `jsValueType`s corresponding to hijacked classes that extend this class.
      *
      * This value is used for instance tests against this class. A JS value `x` is an instance of
      * this type iff `jsValueType(x)` is a member of this bitset. Because of how a bitset works,
      * this means testing the following formula:
      *
      * {{{
      * ((1 << jsValueType(x)) & specialInstanceTypes) != 0
      * }}}
      *
      * For example, if this class is `Comparable`, we want the bitset to contain the values for
      * `boolean`, `string` and `number` (but not `undefined`), because `jl.Boolean`, `jl.String`
      * and `jl.Double` implement `Comparable`.
      *
      * This field is initialized with 0, and augmented during preprocessing by calls to
      * `addSpecialInstanceType`.
      *
      * This technique is used both for static `isInstanceOf` tests as well as reflective tests
      * through `Class.isInstance`. For the latter, this value is stored in
      * `typeData.specialInstanceTypes`. For the former, it is embedded as a constant in the
      * generated code.
      *
      * See the `isInstance` and `genInstanceTest` helpers.
      *
      * Special cases: this value remains 0 for all the numeric hijacked classes except `jl.Double`,
      * since `jsValueType(x) == JSValueTypeNumber` is not enough to deduce that
      * `x.isInstanceOf[Int]`, for example.
      */
    def specialInstanceTypes: Int = _specialInstanceTypes

    /** Is this class an ancestor of any hijacked class?
      *
      * This includes but is not limited to the hijacked classes themselves, as well as `jl.Object`.
      */
    def isAncestorOfHijackedClass: Boolean =
      specialInstanceTypes != 0 || kind == ClassKind.HijackedClass

    def isInterface = kind == ClassKind.Interface

    def registerDynamicCall(methodName: MethodName): Unit =
      methodsCalledDynamically += methodName

    def buildMethodTable(): Unit = {
      if (_tableEntries != null)
        throw new IllegalStateException(s"Duplicate call to buildMethodTable() for $name")

      kind match {
        case ClassKind.Class | ClassKind.ModuleClass | ClassKind.HijackedClass =>
          val superTableEntries = superClass.fold[List[MethodName]](Nil)(_.tableEntries)
          val superTableEntrySet = superTableEntries.toSet

          /* When computing the table entries to add for this class, exclude:
           * - methods that are already in the super class' table entries, and
           * - methods that are effectively final, since they will always be
           *   statically resolved instead of using the table dispatch.
           */
          val newTableEntries = methodsCalledDynamically.toList
            .filter(!superTableEntrySet.contains(_))
            .filterNot(m => resolvedMethodInfos.get(m).exists(_.isEffectivelyFinal))
            .sorted // for stability

          _tableEntries = superTableEntries ::: newTableEntries

        case ClassKind.Interface =>
          _tableEntries = methodsCalledDynamically.toList.sorted // for stability

        case _ =>
          _tableEntries = Nil
      }

      methodsCalledDynamically.clear() // gc
    }

    def tableEntries: List[MethodName] = {
      if (_tableEntries == null)
        throw new IllegalStateException(s"Table not yet built for $name")
      _tableEntries
    }
  }

  def preprocess(classes: List[LinkedClass], tles: List[LinkedTopLevelExport]): KnowledgeGuardian = {
    val staticFieldMirrors = computeStaticFieldMirrors(tles)

    val classInfosBuilder = mutable.HashMap.empty[ClassName, ClassInfo]
    val definedReflectiveProxyNames = mutable.HashSet.empty[MethodName]

    for (clazz <- classes) {
      val classInfo = preprocess(
        clazz,
        staticFieldMirrors.getOrElse(clazz.className, Map.empty),
        classInfosBuilder
      )
      classInfosBuilder += clazz.className -> classInfo

      // For Scala classes, collect the reflective proxy method names that it defines
      if (clazz.kind.isClass || clazz.kind == ClassKind.HijackedClass) {
        for (method <- clazz.methods if method.methodName.isReflectiveProxy)
          definedReflectiveProxyNames += method.methodName
      }
    }

    val classInfos = classInfosBuilder.toMap

    // sort for stability
    val reflectiveProxyIDs = definedReflectiveProxyNames.toList.sorted.zipWithIndex.toMap

    val collector = new AbstractMethodCallCollector(classInfos)
    for (clazz <- classes)
      collector.collectAbstractMethodCalls(clazz)
    for (tle <- tles)
      collector.collectAbstractMethodCalls(tle)

    for (clazz <- classes) {
      classInfos(clazz.className).buildMethodTable()
    }
    val itablesLength = assignBuckets(classes, classInfos)

    new KnowledgeGuardian(classInfos, reflectiveProxyIDs, itablesLength)
  }

  private def computeStaticFieldMirrors(
      tles: List[LinkedTopLevelExport]
  ): Map[ClassName, Map[FieldName, List[String]]] = {
    var result = Map.empty[ClassName, Map[FieldName, List[String]]]
    for (tle <- tles) {
      tle.tree match {
        case TopLevelFieldExportDef(_, exportName, FieldIdent(fieldName)) =>
          val className = tle.owningClass
          val mirrors = result.getOrElse(className, Map.empty)
          val newExportNames = exportName :: mirrors.getOrElse(fieldName, Nil)
          val newMirrors = mirrors.updated(fieldName, newExportNames)
          result = result.updated(className, newMirrors)

        case _ =>
      }
    }
    result
  }

  private def preprocess(
      clazz: LinkedClass,
      staticFieldMirrors: Map[FieldName, List[String]],
      previousClassInfos: collection.Map[ClassName, ClassInfo]
  ): ClassInfo = {
    val className = clazz.className
    val kind = clazz.kind

    val allFieldDefs: List[FieldDef] =
      if (kind.isClass) {
        val inheritedFields = clazz.superClass match {
          case None      => Nil
          case Some(sup) => previousClassInfos(sup.name).allFieldDefs
        }
        val myFieldDefs = clazz.fields.collect {
          case fd: FieldDef if !fd.flags.namespace.isStatic =>
            fd
          case fd: JSFieldDef =>
            throw new AssertionError(s"Illegal $fd in Scala class $className")
        }
        inheritedFields ::: myFieldDefs
      } else {
        Nil
      }

    val classConcretePublicMethodNames = {
      if (kind.isClass || kind == ClassKind.HijackedClass) {
        for {
          m <- clazz.methods
          if m.body.isDefined && m.flags.namespace == MemberNamespace.Public
        } yield {
          m.methodName
        }
      } else {
        Nil
      }
    }

    val superClass = clazz.superClass.map(sup => previousClassInfos(sup.name))

    val strictClassAncestors =
      if (kind.isClass || kind == ClassKind.HijackedClass) clazz.ancestors.tail
      else Nil

    // Does this Scala class implement any interface?
    val classImplementsAnyInterface =
      strictClassAncestors.exists(a => previousClassInfos(a).isInterface)

    /* Should we emit a vtable/typeData global for this class?
     *
     * There are essentially three reasons for which we need them:
     *
     * - Because there is a `classOf[C]` somewhere in the program; if that is
     *   true, then `clazz.hasRuntimeTypeInfo` is true.
     * - Because it is the vtable of a class with direct instances; in that
     *   case `clazz.hasRuntimeTypeInfo` is also true, as guaranteed by the
     *   Scala.js frontend analysis.
     * - Because we generate a test of the form `isInstanceOf[Array[C]]`. In
     *   that case, `clazz.hasInstanceTests` is true.
     *
     * `clazz.hasInstanceTests` is also true if there is only `isInstanceOf[C]`,
     * in the program, so that is not *optimal*, but it is correct.
     */
    val hasRuntimeTypeInfo = clazz.hasRuntimeTypeInfo || clazz.hasInstanceTests

    val classInfo = {
      new ClassInfo(
        className,
        kind,
        clazz.jsClassCaptures,
        classConcretePublicMethodNames,
        allFieldDefs,
        superClass,
        classImplementsAnyInterface,
        clazz.hasInstances,
        !clazz.hasDirectInstances,
        hasRuntimeTypeInfo,
        clazz.jsNativeLoadSpec,
        clazz.jsNativeMembers.map(m => m.name.name -> m.jsNativeLoadSpec).toMap,
        staticFieldMirrors,
        _itableIdx = -1
      )
    }

    // Update specialInstanceTypes for ancestors of hijacked classes
    if (clazz.kind == ClassKind.HijackedClass) {
      def addSpecialInstanceTypeOnAllAncestors(jsValueType: Int): Unit =
        strictClassAncestors.foreach(previousClassInfos(_).addSpecialInstanceType(jsValueType))

      clazz.className match {
        case BoxedBooleanClass =>
          addSpecialInstanceTypeOnAllAncestors(JSValueTypeFalse)
          addSpecialInstanceTypeOnAllAncestors(JSValueTypeTrue)
        case BoxedStringClass =>
          addSpecialInstanceTypeOnAllAncestors(JSValueTypeString)
        case BoxedDoubleClass =>
          addSpecialInstanceTypeOnAllAncestors(JSValueTypeNumber)
        case BoxedUnitClass =>
          addSpecialInstanceTypeOnAllAncestors(JSValueTypeUndefined)
        case _ =>
          ()
      }
    }

    /* Work around https://github.com/scala-js/scala-js/issues/4972
     * Manually mark all ancestors of instantiated classes as having instances.
     */
    if (clazz.hasDirectInstances && !kind.isJSType)
      strictClassAncestors.foreach(ancestor => previousClassInfos(ancestor).setHasInstances())

    classInfo
  }

  /** Collect FunctionInfo based on the abstract method call
    *
    * ```
    * class A extends B:
    *   def a = 1
    *
    * class B extends C:
    *   def b: Int = 1
    *   override def c: Int = 1
    *
    * abstract class C:
    *   def c: Int
    * ```
    *
    * why we need this? - The problem is that the frontend linker gets rid of abstract method
    * entirely.
    *
    * It keeps B.c because it's concrete and used. But because `C.c` isn't there at all anymore, if
    * we have val `x: C` and we call `x.c`, we don't find the method at all.
    */
  private class AbstractMethodCallCollector(classInfos: Map[ClassName, ClassInfo])
      extends Traversers.Traverser {
    def collectAbstractMethodCalls(clazz: LinkedClass): Unit = {
      for (method <- clazz.methods)
        traverseMethodDef(method)
      for (jsConstructor <- clazz.jsConstructorDef)
        traverseJSConstructorDef(jsConstructor)
      for (export <- clazz.exportedMembers)
        traverseJSMethodPropDef(export)
    }

    def collectAbstractMethodCalls(tle: LinkedTopLevelExport): Unit = {
      tle.tree match {
        case TopLevelMethodExportDef(_, jsMethodDef) =>
          traverseJSMethodPropDef(jsMethodDef)
        case _ =>
          ()
      }
    }

    override def traverse(tree: Tree): Unit = {
      super.traverse(tree)

      tree match {
        case Apply(flags, receiver, methodName, _) if !methodName.name.isReflectiveProxy =>
          receiver.tpe match {
            case ClassType(className) =>
              val classInfo = classInfos(className)
              if (classInfo.hasInstances)
                classInfo.registerDynamicCall(methodName.name)
            case AnyType =>
              classInfos(ObjectClass).registerDynamicCall(methodName.name)
            case _ =>
              // For all other cases, including arrays, we will always perform a static dispatch
              ()
          }

        case _ =>
          ()
      }
    }
  }

  /** Group interface types + types that implements any interfaces into buckets, where no two types
    * in the same bucket can have common subtypes.
    *
    * It allows compressing the itable by reusing itable's index (buckets) for unrelated types,
    * instead of having a 1-1 mapping from type to index. As a result, the itables' length will be
    * the same as the number of buckets).
    *
    * The algorithm separates the type hierarchy into three disjoint subsets,
    *
    *   - join types: types with multiple parents (direct supertypes) that have only single
    *     subtyping descendants: `join(T) = {x ∈ multis(T) | ∄ y ∈ multis(T) : y <: x}` where
    *     multis(T) means types with multiple direct supertypes.
    *   - spine types: all ancestors of join types: `spine(T) = {x ∈ T | ∃ y ∈ join(T) : x ∈
    *     ancestors(y)}`
    *   - plain types: types that are neither join nor spine types
    *
    * The bucket assignment process consists of two parts:
    *
    * **1. Assign buckets to spine types**
    *
    * Two spine types can share the same bucket only if they do not have any common join type
    * descendants.
    *
    * Visit spine types in reverse topological order because (from leaves to root) when assigning a
    * a spine type to bucket, the algorithm already has the complete information about the
    * join/spine type descendants of that spine type.
    *
    * Assign a bucket to a spine type if adding it doesn't violate the bucket assignment rule: two
    * spine types can share a bucket only if they don't have any common join type descendants. If no
    * existing bucket satisfies the rule, create a new bucket.
    *
    * **2. Assign buckets to non-spine types (plain and join types)**
    *
    * Visit these types in level order (from root to leaves) For each type, compute the set of
    * buckets already used by its ancestors. Assign the type to any available bucket not in this
    * set. If no available bucket exists, create a new one.
    *
    * To test if type A is a subtype of type B: load the bucket index of type B (we do this by
    * `getItableIdx`), load the itable at that index from A, and check if the itable is an itable
    * for B.
    *
    * @see
    *   This algorithm is based on the "packed encoding" presented in the paper "Efficient Type
    *   Inclusion Tests"
    *   [[https://www.researchgate.net/publication/2438441_Efficient_Type_Inclusion_Tests]]
    */
  private def assignBuckets(
      allClasses: List[LinkedClass],
      classInfos: Map[ClassName, ClassInfo]
  ): Int = {
    val classes = allClasses.filterNot(_.kind.isJSType)

    var nextIdx = 0
    def newBucket(): Bucket = {
      val idx = nextIdx
      nextIdx += 1
      new Bucket(idx)
    }
    def getAllInterfaces(clazz: LinkedClass): List[ClassName] =
      clazz.ancestors.filter(classInfos(_).isInterface)

    val buckets = new mutable.ListBuffer[Bucket]()

    /** All join type descendants of the class */
    val joinsOf =
      new mutable.HashMap[ClassName, mutable.HashSet[ClassName]]()

    /** the buckets that have been assigned to any of the ancestors of the class */
    val usedOf = new mutable.HashMap[ClassName, mutable.HashSet[Bucket]]()
    val spines = new mutable.HashSet[ClassName]()

    for (clazz <- classes.reverseIterator) {
      val info = classInfos(clazz.name.name)
      val ifaces = getAllInterfaces(clazz)
      if (ifaces.nonEmpty) {
        val joins = joinsOf.getOrElse(clazz.name.name, new mutable.HashSet())

        if (joins.nonEmpty) { // spine type
          var found = false
          val bs = buckets.iterator
          // look for an existing bucket to add the spine type to
          while (!found && bs.hasNext) {
            val b = bs.next()
            // two spine types can share a bucket only if they don't have any common join type descendants
            if (!b.joins.exists(joins)) {
              found = true
              b.add(info)
              b.joins ++= joins
            }
          }
          if (!found) { // there's no bucket to add, create new bucket
            val b = newBucket()
            b.add(info)
            buckets.append(b)
            b.joins ++= joins
          }
          for (iface <- ifaces) {
            joinsOf.getOrElseUpdate(iface, new mutable.HashSet()) ++= joins
          }
          spines.add(clazz.name.name)
        } else if (ifaces.length > 1) { // join type, add to joins map, bucket assignment is done later
          ifaces.foreach { iface =>
            joinsOf.getOrElseUpdate(iface, new mutable.HashSet()) += clazz.name.name
          }
        }
        // else: plain, do nothing
      }

    }

    for (clazz <- classes) {
      val info = classInfos(clazz.name.name)
      val ifaces = getAllInterfaces(clazz)
      if (ifaces.nonEmpty && !spines.contains(clazz.name.name)) {
        val used = usedOf.getOrElse(clazz.name.name, new mutable.HashSet())
        for {
          iface <- ifaces
          parentUsed <- usedOf.get(iface)
        } { used ++= parentUsed }

        var found = false
        val bs = buckets.iterator
        while (!found && bs.hasNext) {
          val b = bs.next()
          if (!used.contains(b)) {
            found = true
            b.add(info)
            used.add(b)
          }
        }
        if (!found) {
          val b = newBucket()
          buckets.append(b)
          b.add(info)
          used.add(b)
        }
      }
    }

    buckets.length
  }

  private final class Bucket(idx: Int) {
    def add(clazz: ClassInfo) = clazz.setItableIdx((idx))

    /** A set of join types that are descendants of the types assigned to that bucket */
    val joins = new mutable.HashSet[ClassName]()
  }

}
