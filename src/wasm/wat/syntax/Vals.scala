package wasm.wat.syntax

/** Represents values and identifiers in WebAssembly */
enum Val:
  // Basic WebAssembly value types
  case I32(value: Int)            // 32-bit integer
  case I64(value: Long)           // 64-bit integer
  case F32(value: Float)          // 32-bit floating-point number
  case F64(value: Double)         // 64-bit floating-point number

  // Extended types
  case Str(value: String)         // String value (non-standard, for extended use cases)
  case Name(value: String)        // Named identifier (e.g., for labels or symbols)
  case Id(value: String)          // Programmatic identifier (e.g., function or variable index)