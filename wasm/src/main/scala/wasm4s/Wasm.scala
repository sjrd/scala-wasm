package wasm.wasm4s

import scala.collection.mutable

import Types._
import Names._
import Names.WasmTypeName._

sealed case class WasmExpr(instr: List[WasmInstr])

sealed abstract class WasmExport {
  val exportName: String
}

object WasmExport {
  final case class Function(exportName: String, funcName: WasmFunctionName) extends WasmExport
  final case class Global(exportName: String, globalName: WasmGlobalName) extends WasmExport
}

final case class WasmImport(module: String, name: String, desc: WasmImportDesc)

sealed abstract class WasmImportDesc

object WasmImportDesc {
  final case class Func(id: WasmFunctionName, typeName: WasmTypeName) extends WasmImportDesc
  final case class Global(id: WasmGlobalName, typ: WasmType, isMutable: Boolean)
      extends WasmImportDesc
  final case class Tag(id: WasmTagName, typeName: WasmTypeName) extends WasmImportDesc
}

/** @see
  *   https://webassembly.github.io/spec/core/syntax/modules.html#functions
  */
case class WasmFunction(
    val name: WasmFunctionName,
    val typeName: WasmTypeName,
    val locals: List[WasmLocal],
    val results: List[WasmType],
    val body: WasmExpr
)

/** The index space for locals is only accessible inside a function and includes the parameters of
  * that function, which precede the local variables.
  */
case class WasmLocal(
    val name: WasmLocalName,
    val typ: WasmType,
    val isParameter: Boolean // for text
)

final case class WasmTag(val name: WasmTagName, val typ: WasmTypeName)

final case class WasmData(val name: WasmDataName, val bytes: Array[Byte], mode: WasmData.Mode)

object WasmData {
  sealed abstract class Mode
  object Mode {
    case object Passive extends Mode
    // final case class Active
  }
}

case class WasmGlobal(
    val name: WasmGlobalName,
    val typ: WasmType,
    val init: WasmExpr,
    val isMutable: Boolean
)

final class WasmRecType {
  private val _subTypes = mutable.ListBuffer.empty[WasmSubType]

  def addSubType(subType: WasmSubType): Unit =
    _subTypes += subType

  def addSubType(name: WasmTypeName, compositeType: WasmCompositeType): Unit =
    addSubType(WasmSubType(name, compositeType))

  def subTypes: List[WasmSubType] = _subTypes.toList
}

object WasmRecType {

  /** Builds a `rectype` with a single `subtype`. */
  def apply(singleSubType: WasmSubType): WasmRecType = {
    val recType = new WasmRecType
    recType.addSubType(singleSubType)
    recType
  }
}

final case class WasmSubType(
    name: WasmTypeName,
    isFinal: Boolean,
    superType: Option[WasmTypeName],
    compositeType: WasmCompositeType
)

object WasmSubType {

  /** Builds a `subtype` that is `final` and without any super type. */
  def apply(name: WasmTypeName, compositeType: WasmCompositeType): WasmSubType =
    WasmSubType(name, isFinal = true, superType = None, compositeType)
}

sealed abstract class WasmCompositeType

case class WasmFunctionSignature(
    params: List[WasmType],
    results: List[WasmType]
)
object WasmFunctionSignature {
  val NilToNil: WasmFunctionSignature = WasmFunctionSignature(Nil, Nil)
}

case class WasmFunctionType(
    params: List[WasmType],
    results: List[WasmType]
) extends WasmCompositeType
object WasmFunctionType {
  def apply(sig: WasmFunctionSignature): WasmFunctionType = {
    WasmFunctionType(sig.params, sig.results)
  }
}

case class WasmStructType(fields: List[WasmStructField]) extends WasmCompositeType
object WasmStructType {

  /** Run-time type data of a `TypeRef`. Support for `j.l.Class` methods and other reflective
    * operations.
    *
    * @see
    *   [[Names.WasmFieldName.typeData]], which contains documentation of what is in each field.
    */
  def typeData(implicit ctx: ReadOnlyWasmContext): WasmStructType = WasmStructType(
    List(
      WasmStructField(
        WasmFieldName.typeData.nameData,
        WasmRefType.nullable(WasmArrayTypeName.i16Array),
        isMutable = false
      ),
      WasmStructField(
        WasmFieldName.typeData.kind,
        WasmInt32,
        isMutable = false
      ),
      WasmStructField(
        WasmFieldName.typeData.specialInstanceTypes,
        WasmInt32,
        isMutable = false
      ),
      WasmStructField(
        WasmFieldName.typeData.strictAncestors,
        WasmRefType.nullable(WasmTypeName.WasmArrayTypeName.typeDataArray),
        isMutable = false
      ),
      WasmStructField(
        WasmFieldName.typeData.componentType,
        WasmRefType.nullable(WasmTypeName.WasmStructTypeName.typeData),
        isMutable = false
      ),
      WasmStructField(
        WasmFieldName.typeData.name,
        WasmRefType.anyref,
        isMutable = true
      ),
      WasmStructField(
        WasmFieldName.typeData.classOfValue,
        WasmRefType.nullable(WasmHeapType.ClassType),
        isMutable = true
      ),
      WasmStructField(
        WasmFieldName.typeData.arrayOf,
        WasmRefType.nullable(WasmTypeName.WasmStructTypeName.ObjectVTable),
        isMutable = true
      ),
      WasmStructField(
        WasmFieldName.typeData.cloneFunction,
        WasmRefType.nullable(ctx.cloneFunctionTypeName),
        isMutable = false
      ),
      WasmStructField(
        WasmFieldName.typeData.isJSClassInstance,
        WasmRefType.nullable(ctx.isJSClassInstanceFuncTypeName),
        isMutable = false
      ),
      WasmStructField(
        WasmFieldName.typeData.reflectiveProxies,
        WasmRefType(WasmHeapType(WasmArrayTypeName.reflectiveProxies)),
        isMutable = false
      )
    )
  )

  // The number of fields of typeData, after which we find the vtable entries
  def typeDataFieldCount(implicit ctx: ReadOnlyWasmContext) = typeData.fields.size

  val reflectiveProxy: WasmStructType = WasmStructType(
    List(
      WasmStructField(
        WasmFieldName.reflectiveProxy.func_name,
        WasmInt32,
        isMutable = false
      ),
      WasmStructField(
        WasmFieldName.reflectiveProxy.func_ref,
        WasmRefType(WasmHeapType.Func),
        isMutable = false
      )
    )
  )
}

case class WasmArrayType(field: WasmStructField) extends WasmCompositeType
object WasmArrayType {

  /** array (ref typeData) */
  val typeDataArray = WasmArrayType(
    WasmStructField(
      WasmFieldName.arrayItem,
      WasmRefType(WasmStructTypeName.typeData),
      isMutable = false
    )
  )

  /** array (ref struct) */
  val itables = WasmArrayType(
    WasmStructField(
      WasmFieldName.itable,
      WasmRefType.nullable(WasmHeapType.Struct),
      isMutable = true
    )
  )

  val reflectiveProxies = WasmArrayType(
    WasmStructField(
      WasmFieldName.reflectiveProxyField,
      WasmRefType(WasmStructTypeName.reflectiveProxy),
      isMutable = false
    )
  )

  /** array i8 */
  val i8Array = WasmArrayType(
    WasmStructField(WasmFieldName.arrayItem, WasmInt8, true)
  )

  /** array i16 */
  val i16Array = WasmArrayType(
    WasmStructField(WasmFieldName.arrayItem, WasmInt16, true)
  )

  /** array i32 */
  val i32Array = WasmArrayType(
    WasmStructField(WasmFieldName.arrayItem, WasmInt32, true)
  )

  /** array i64 */
  val i64Array = WasmArrayType(
    WasmStructField(WasmFieldName.arrayItem, WasmInt64, true)
  )

  /** array f32 */
  val f32Array = WasmArrayType(
    WasmStructField(WasmFieldName.arrayItem, WasmFloat32, true)
  )

  /** array f64 */
  val f64Array = WasmArrayType(
    WasmStructField(WasmFieldName.arrayItem, WasmFloat64, true)
  )

  /** array anyref */
  val anyArray = WasmArrayType(
    WasmStructField(WasmFieldName.arrayItem, WasmRefType.anyref, true)
  )
}

case class WasmStructField(
    name: WasmFieldName,
    typ: WasmStorageType,
    isMutable: Boolean
)
object WasmStructField {
  val itables = WasmStructField(
    WasmFieldName.itables,
    WasmRefType.nullable(WasmArrayTypeName.itables),
    isMutable = false
  )
}

final case class WasmElement(typ: WasmType, init: List[WasmExpr], mode: WasmElement.Mode)

object WasmElement {
  sealed abstract class Mode

  object Mode {
    case object Passive extends Mode
    // final case class Active(table: WasmImmediate.TableIdx, offset: WasmExpr) extends Mode
    case object Declarative extends Mode
  }
}

/** @see
  *   https://webassembly.github.io/spec/core/syntax/modules.html#modules
  */
class WasmModule {
  private val _recTypes: mutable.ListBuffer[WasmRecType] = new mutable.ListBuffer()
  private val _imports: mutable.ListBuffer[WasmImport] = new mutable.ListBuffer()
  private val _definedFunctions: mutable.ListBuffer[WasmFunction] = new mutable.ListBuffer()
  private val _tags: mutable.ListBuffer[WasmTag] = new mutable.ListBuffer()
  private val _data: mutable.ListBuffer[WasmData] = new mutable.ListBuffer()
  private val _globals: mutable.ListBuffer[WasmGlobal] = new mutable.ListBuffer()
  private val _exports: mutable.ListBuffer[WasmExport] = new mutable.ListBuffer()
  private var _startFunction: Option[WasmFunctionName] = None
  private val _elements: mutable.ListBuffer[WasmElement] = new mutable.ListBuffer()

  def addRecType(typ: WasmRecType): Unit = _recTypes += typ
  def addRecType(typ: WasmSubType): Unit = addRecType(WasmRecType(typ))

  def addRecType(name: WasmTypeName, compositeType: WasmCompositeType): Unit =
    addRecType(WasmSubType(name, compositeType))

  def addImport(imprt: WasmImport): Unit = _imports += imprt
  def addFunction(function: WasmFunction): Unit = _definedFunctions += function
  def addTag(tag: WasmTag): Unit = _tags += tag
  def addData(data: WasmData): Unit = _data += data
  def addGlobal(typ: WasmGlobal): Unit = _globals += typ
  def addExport(exprt: WasmExport): Unit = _exports += exprt
  def setStartFunction(startFunction: WasmFunctionName): Unit = _startFunction = Some(startFunction)
  def addElement(element: WasmElement): Unit = _elements += element

  def recTypes = _recTypes.toList
  def imports = _imports.toList
  def definedFunctions = _definedFunctions.toList
  def tags: List[WasmTag] = _tags.toList
  def data: List[WasmData] = _data.toList
  def globals = _globals.toList
  def exports = _exports.toList
  def startFunction: Option[WasmFunctionName] = _startFunction
  def elements: List[WasmElement] = _elements.toList
}
