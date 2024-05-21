package org.scalajs.linker.backend.wasmemitter

import scala.annotation.tailrec

import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees.{FieldDef, ParamDef, JSNativeLoadSpec}
import org.scalajs.ir.Types._

import org.scalajs.linker.interface.ModuleInitializer
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl
import org.scalajs.linker.standard.LinkedTopLevelExport
import org.scalajs.linker.standard.LinkedClass

import org.scalajs.linker.backend.webassembly.ModuleBuilder
import org.scalajs.linker.backend.webassembly.{Instructions => wa}
import org.scalajs.linker.backend.webassembly.{Modules => wamod}
import org.scalajs.linker.backend.webassembly.{Identitities => wanme}
import org.scalajs.linker.backend.webassembly.{Types => watpe}

import VarGen._

final class WasmContext {
  import WasmContext._

  private val functionTypes = LinkedHashMap.empty[watpe.FunctionType, wanme.TypeID]
  private val tableFunctionTypes = mutable.HashMap.empty[MethodName, wanme.TypeID]
  private val constantStringGlobals = LinkedHashMap.empty[String, StringData]
  private val closureDataTypes = LinkedHashMap.empty[List[Type], wanme.TypeID]

  val moduleBuilder: ModuleBuilder = {
    new ModuleBuilder(new ModuleBuilder.FunctionTypeProvider {
      def functionTypeToTypeName(sig: watpe.FunctionType): wanme.TypeID = {
        functionTypes.getOrElseUpdate(
          sig, {
            val typeName = genTypeID.forFunction(functionTypes.size)
            moduleBuilder.addRecType(typeName, NoOriginalName, sig)
            typeName
          }
        )
      }
    })
  }

  private var stringPool = new mutable.ArrayBuffer[Byte]()
  private var nextConstantStringIndex: Int = 0
  private var nextClosureDataTypeIndex: Int = 1

  private val _funcDeclarations: mutable.LinkedHashSet[wanme.FunctionID] =
    new mutable.LinkedHashSet()

  /** The main `rectype` containing the object model types. */
  val mainRecType: ModuleBuilder.RecTypeBuilder = new ModuleBuilder.RecTypeBuilder

  def inferTypeFromTypeRef(typeRef: TypeRef)(implicit globalKnowledge: GlobalKnowledge): Type = typeRef match {
    case PrimRef(tpe) =>
      tpe
    case ClassRef(className) =>
      if (className == ObjectClass || globalKnowledge.isJSType(className))
        AnyType
      else
        ClassType(className)
    case typeRef: ArrayTypeRef =>
      ArrayType(typeRef)
  }

  /** Adds or reuses a function type for a table function.
    *
    * Table function types are part of the main `rectype`, and have names derived from the
    * `methodName`.
    */
  def tableFunctionType(methodName: MethodName)(implicit globalKnowledge: GlobalKnowledge): wanme.TypeID = {
    // Project all the names with the same *signatures* onto a normalized `MethodName`
    val normalizedName = MethodName(
      SpecialNames.normalizedSimpleMethodName,
      methodName.paramTypeRefs,
      methodName.resultTypeRef,
      methodName.isReflectiveProxy
    )

    tableFunctionTypes.getOrElseUpdate(
      normalizedName, {
        val typeName = genTypeID.forTableFunctionType(normalizedName)
        val regularParamTyps = normalizedName.paramTypeRefs.map { typeRef =>
          TypeTransformer.transformType(inferTypeFromTypeRef(typeRef))
        }
        val resultTyp =
          TypeTransformer.transformResultType(inferTypeFromTypeRef(normalizedName.resultTypeRef))
        mainRecType.addSubType(
          typeName,
          NoOriginalName,
          watpe.FunctionType(watpe.RefType.any :: regularParamTyps, resultTyp)
        )
        typeName
      }
    )
  }

  def addConstantStringGlobal(str: String): StringData = {
    constantStringGlobals.get(str) match {
      case Some(data) =>
        data

      case None =>
        val bytes = str.toCharArray.flatMap { char =>
          Array((char & 0xFF).toByte, (char >> 8).toByte)
        }
        val offset = stringPool.size
        val data = StringData(nextConstantStringIndex, offset)
        constantStringGlobals(str) = data

        stringPool ++= bytes
        nextConstantStringIndex += 1
        data
    }
  }

  def getConstantStringInstr(str: String): List[wa.Instr] =
    getConstantStringDataInstr(str) :+ wa.Call(genFunctionID.stringLiteral)

  def getConstantStringDataInstr(str: String): List[wa.I32Const] = {
    val data = addConstantStringGlobal(str)
    List(
      wa.I32Const(data.offset),
      // Assuming that the stringLiteral method will instantiate the
      // constant string from the data section using "array.newData $i16Array ..."
      // The length of the array should be equal to the length of the WTF-16 encoded string
      wa.I32Const(str.length()),
      wa.I32Const(data.constantStringIndex)
    )
  }

  def getClosureDataStructType(captureParamTypes: List[Type])(implicit globalKnowledge: GlobalKnowledge): wanme.TypeID = {
    closureDataTypes.getOrElseUpdate(
      captureParamTypes, {
        val fields: List[watpe.StructField] =
          for ((tpe, i) <- captureParamTypes.zipWithIndex)
            yield watpe.StructField(
              genFieldID.captureParam(i),
              NoOriginalName,
              TypeTransformer.transformType(tpe),
              isMutable = false
            )
        val structTypeName = genTypeID.captureData(nextClosureDataTypeIndex)
        nextClosureDataTypeIndex += 1
        val structType = watpe.StructType(fields)
        moduleBuilder.addRecType(structTypeName, NoOriginalName, structType)
        structTypeName
      }
    )
  }

  def refFuncWithDeclaration(name: wanme.FunctionID): wa.RefFunc = {
    _funcDeclarations += name
    wa.RefFunc(name)
  }

  def addGlobal(g: wamod.Global): Unit =
    moduleBuilder.addGlobal(g)

  def getFinalStringPool(): (Array[Byte], Int) =
    (stringPool.toArray, nextConstantStringIndex)

  def getAllFuncDeclarations(): List[wanme.FunctionID] =
    _funcDeclarations.toList
}

object WasmContext {
  final case class StringData(constantStringIndex: Int, offset: Int)
}
