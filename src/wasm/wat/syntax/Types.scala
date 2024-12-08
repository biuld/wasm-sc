package wasm.wat.syntax.types

/** Numeric types in WebAssembly */
enum NumType:
  case I32, I64, F32, F64

/** Reference types in WebAssembly */
enum RefType:
  case FuncRef, ExternRef

enum HeapType:
  case Func, Extern

/** Value types: either numeric or reference types */
enum ValType:
  case Num(numType: NumType)       // Numeric type
  case Ref(refType: RefType)       // Reference type

/** Function type: parameter types and result types */
case class FuncType(
  params: List[ValType],           // List of parameter types
  results: List[ValType]           // List of result types
)

/** Limits: specifies minimum and optionally maximum size */
case class Limits(
  min: Int,                        // Minimum size
  max: Option[Int] = None          // Optional maximum size
)

/** Memory type: defined by its limits */
case class MemType(
  limits: Limits                   // Memory size limits
)

/** Table type: reference type and limits */
case class TableType(
  refType: RefType,                // Reference type of the table
  limits: Limits                   // Table size limits
)

/** Global type: value type and mutability */
enum Mutability:
  case Const                       // Immutable global
  case Var                         // Mutable global

case class GlobalType(
  valType: ValType,                // Type of the global variable
  mutability: Mutability           // Mutability of the global variable
)