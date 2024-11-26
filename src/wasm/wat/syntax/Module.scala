package wasm.wat.syntax

/** Represents a WAT module */
case class Module(
    types: List[FuncType], // Function types
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
  case Func(typeIdx: Int) // Function type index
  case Table(tableType: TableType) // Table type
  case Mem(memType: MemType) // Memory type
  case Global(globalType: GlobalType) // Global type

/** Represents a function */
case class Func(
    typeIdx: Int, // Function type index
    locals: List[ValType], // Local variable types
    body: List[Instruction] // Function body as a list of instructions
)

/** Represents a table */
case class Table(
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
  case Func(funcIdx: Int) // Function index
  case Table(tableIdx: Int) // Table index
  case Mem(memIdx: Int) // Memory index
  case Global(globalIdx: Int) // Global index

/** Represents the start function */
case class Start(
    funcIdx: Int // Index of the start function
)

/** Represents an element segment */
case class Elem(
    tableIdx: Int, // Table index
    offset: Expr, // Offset expression
    init: List[Int] // List of function indices to initialize the table
)

/** Represents a data segment */
case class Data(
    memIdx: Int, // Memory index
    offset: Expr, // Offset expression
    init: List[Byte] // Data to initialize memory
)

/** Represents an expression (a list of instructions) */
type Expr = List[Instruction]
