package wasm.wat.syntax.mod

import wasm.wat.syntax.types.*
import wasm.wat.syntax.instr.*

enum Idx:
  case U32(value: Int)
  case Id(value: String)

type TableIdx = Idx
type ElemIdx = Idx
type LocalIdx = Idx
type GlobalIdx = Idx
type DataIdx = Idx
type FuncIdx = Idx
type MemIdx = Idx

/** Represents a WAT module */
case class Module(
    types: List[Named[FuncType]], // Function types
    imports: List[Import], // Import declarations
    funcs: List[Func], // Function definitions
    tables: List[Table], // Table definitions
    mems: List[Mem], // Memory definitions
    globals: List[Global], // Global definitions
    exports: List[Export], // Export declarations
    start: Option[Start], // Start function (optional)
    elems: List[Elem], // Element segments
    datas: List[Data] // Data segments
)

/** Represents an import */
case class Import(
    module: String, // Module name
    name: String, // Import name
    desc: ImportDesc // Import description
)

/** Import description */
enum ImportDesc:
  case Func(typeIdx: FuncIdx, typeUse: TypeUse) // Function type index
  case Table(tableType: TableType) // Table type
  case Mem(memType: MemType) // Memory type
  case Global(globalType: GlobalType) // Global type

case class Named[T](name: Option[Idx], value: T)

case class TypeUse(
    id: Option[Idx], // Type index
    params: List[ValType], // Parameter types
    results: List[ValType] // Result types
)

def EMPTY_TYPE_USE: TypeUse = TypeUse(None, Nil, Nil)

/** Represents a function */
case class Func(
    id: Option[String],
    imports: Option[(String, String)],
    exports: List[String],
    typeUse: TypeUse,
    locals: List[Named[ValType]],
    body: List[Instruction]
)

/** Represents a table */
case class Table(
    idx: Option[TableIdx],
    tableType: TableType // Table type
)

/** Represents memory */
case class Mem(
    memType: MemType // Memory type
)

/** Represents a global */
case class Global(
    globalType: GlobalType, // Global type
    init: Expr // Initialization expression
)

/** Represents an export */
case class Export(
    name: String, // Export name
    desc: ExportDesc // Export description
)

/** Export description */
enum ExportDesc:
  case Func(funcIdx: FuncIdx) // Function index
  case Table(tableIdx: TableIdx) // Table index
  case Mem(memIdx: MemIdx) // Memory index
  case Global(globalIdx: GlobalIdx) // Global index

/** Represents the start function */
case class Start(
    funcIdx: FuncIdx // Index of the start function
)

/** Represents an element segment */
case class Elem(
    tableIdx: ElemIdx, // Table index
    offset: Expr, // Offset expression
    init: List[FuncIdx] // List of function indices to initialize the table
)

/** Represents a data segment */
case class Data(
    memIdx: MemIdx, // Memory index
    offset: Expr, // Offset expression
    init: List[Byte] // Data to initialize memory
)

/** Represents an expression (a list of instructions) */
type Expr = List[Instruction]
