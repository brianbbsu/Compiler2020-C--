#ifndef __HEADER_HH__
#define __HEADER_HH__

#include <iostream>
#include <string>
#include <variant>
#include <vector>

template <typename T>
void printHelper(std::ostream &os, const T &arg) {
  os << arg << std::endl;
}

template <typename T, typename... Args>
void printHelper(std::ostream &os, const T &arg, const Args &... args) {
  os << arg;
  printHelper(os, args...);
}

#define raiseError(...)                                                               \
  printHelper(std::cerr, "Internal Error: ", __FILE__, ": ", "Line ", __LINE__, ": ", \
              __PRETTY_FUNCTION__, ": ", __VA_ARGS__),                                \
      exit(1)

#define EMPTY_DIM (-1)

/* Enum declaration */
enum DATA_TYPE {
  INT_TYPE,
  FLOAT_TYPE,
  VOID_TYPE,
  ARR_TYPE,
  CONST_STRING_TYPE,     // for "const string"
  WRITE_PARAMETER_TYPE,  // for write()
  NONE_TYPE,
  ERROR_TYPE
};

enum IDENTIFIER_KIND {
  NORMAL_ID,     // function Name, uninitialized scalar variable
  ARRAY_ID,      // ID_NODE->child = dim
  WITH_INIT_ID,  // ID_NODE->child = initial value
};

enum BINARY_OPERATOR {
  BINARY_OP_ADD,
  BINARY_OP_SUB,
  BINARY_OP_MUL,
  BINARY_OP_DIV,
  BINARY_OP_EQ,
  BINARY_OP_GE,
  BINARY_OP_LE,
  BINARY_OP_NE,
  BINARY_OP_GT,
  BINARY_OP_LT,
  BINARY_OP_AND,
  BINARY_OP_OR
};

enum UNARY_OPERATOR { UNARY_OP_POSITIVE, UNARY_OP_NEGATIVE, UNARY_OP_LOGICAL_NEGATION };

// C_type= type of constant ex: 1, 3.3, "const string"
// do not modify, or lexer might break
enum C_type { INTEGERC, FLOATC, STRINGC };


struct Const {
  C_type const_type;
  std::variant<int, float, std::string> value;
  Const() : const_type(INTEGERC), value(0) {}
  Const(int value_) : const_type(INTEGERC), value(value_) {}
  Const(float value_) : const_type(FLOATC), value(value_) {}
};


enum STMT_KIND {
  WHILE_STMT,
  FOR_STMT,
  ASSIGN_STMT,  // TODO:for simpler implementation, assign_expr also uses this
  IF_STMT,
  FUNCTION_CALL_STMT,
  RETURN_STMT,
};

enum EXPR_KIND { BINARY_OPERATION, UNARY_OPERATION };

enum DECL_KIND { VARIABLE_DECL, TYPE_DECL, ENUM_DECL, FUNCTION_DECL, FUNCTION_PARAMETER_DECL };

enum AST_TYPE {
  PROGRAM_NODE,
  DECLARATION_NODE,
  IDENTIFIER_NODE,
  ENUM_NODE,
  PARAM_LIST_NODE,
  NUL_NODE,
  BLOCK_NODE,
  VARIABLE_DECL_LIST_NODE,
  STMT_LIST_NODE,
  STMT_NODE,
  EXPR_NODE,
  CONST_VALUE_NODE,  // ex:1, 2, "constant string"
  NONEMPTY_ASSIGN_EXPR_LIST_NODE,
  NONEMPTY_RELOP_EXPR_LIST_NODE
};

/* Structs for Semantic Value */
struct STMTSemanticValue {
  STMT_KIND kind;
};

struct EXPRSemanticValue {
  EXPR_KIND kind;

  bool isConstEval;
  Const constEvalValue;
  std::variant<BINARY_OPERATOR, UNARY_OPERATOR> op;
};

struct DECLSemanticValue {
  DECL_KIND kind;
};

struct SymbolAttribute;

struct IdentifierSemanticValue {
  std::string identifierName;
  IDENTIFIER_KIND kind;

  bool isEnumerator;
  int enumeratorValue;
  /* struct SymbolTableEntry *symbolTableEntry; */
};

struct ArrayProperties {
  DATA_TYPE elementType;
  std::vector<int> dimensions;
  ArrayProperties() : elementType(NONE_TYPE) {}
  ArrayProperties(DATA_TYPE elementType_, std::vector<int> dimensions_) : elementType{elementType_}, dimensions{dimensions_} {}
  bool operator==(const ArrayProperties &rhs) const {
    return elementType == rhs.elementType && dimensions == rhs.dimensions;
  }
};

struct TypeDescriptor {
  DATA_TYPE type;
  ArrayProperties arrayProperties;
  TypeDescriptor() : type(NONE_TYPE) {}
  TypeDescriptor(DATA_TYPE _type) : type(_type) {}
  TypeDescriptor(DATA_TYPE type_, ArrayProperties arrProp_) : type{type_}, arrayProperties{arrProp_} {}
  bool operator==(const TypeDescriptor &rhs) const {
    return type == rhs.type && arrayProperties == rhs.arrayProperties;
  }
};

/* Parser Types */
extern int lineno;


/* Types for Resource Allocation */
#define SIZEOF_REGISTER (8)
typedef std::string Register;
const Register
  REG_X0{"x0"},   REG_ZERO{"zero"}, REG_F0{"f0"},   REG_FT0{"ft0"},
  REG_X1{"x1"},   REG_RA{"ra"},     REG_F1{"f1"},   REG_FT1{"ft1"},
  REG_X2{"x2"},   REG_SP{"sp"},     REG_F2{"f2"},   REG_FT2{"ft2"},
  REG_X3{"x3"},   REG_GP{"gp"},     REG_F3{"f3"},   REG_FT3{"ft3"},
  REG_X4{"x4"},   REG_TP{"tp"},     REG_F4{"f4"},   REG_FT4{"ft4"},
  REG_X5{"x5"},   REG_T0{"t0"},     REG_F5{"f5"},   REG_FT5{"ft5"},
  REG_X6{"x6"},   REG_T1{"t1"},     REG_F6{"f6"},   REG_FT6{"ft6"},
  REG_X7{"x7"},   REG_T2{"t2"},     REG_F7{"f7"},   REG_FT7{"ft7"},
  REG_X8{"x8"},   REG_FP{"fp"},     REG_F8{"f8"},   REG_FS0{"fs0"},
  REG_X9{"x9"},   REG_S1{"s1"},     REG_F9{"f9"},   REG_FS1{"fs1"},
  REG_X10{"x10"}, REG_A0{"a0"},     REG_F10{"f10"}, REG_FA0{"fa0"},
  REG_X11{"x11"}, REG_A1{"a1"},     REG_F11{"f11"}, REG_FA1{"fa1"},
  REG_X12{"x12"}, REG_A2{"a2"},     REG_F12{"f12"}, REG_FA2{"fa2"},
  REG_X13{"x13"}, REG_A3{"a3"},     REG_F13{"f13"}, REG_FA3{"fa3"},
  REG_X14{"x14"}, REG_A4{"a4"},     REG_F14{"f14"}, REG_FA4{"fa4"},
  REG_X15{"x15"}, REG_A5{"a5"},     REG_F15{"f15"}, REG_FA5{"fa5"},
  REG_X16{"x16"}, REG_A6{"a6"},     REG_F16{"f16"}, REG_FA6{"fa6"},
  REG_X17{"x17"}, REG_A7{"a7"},     REG_F17{"f17"}, REG_FA7{"fa7"},
  REG_X18{"x18"}, REG_S2{"s2"},     REG_F18{"f18"}, REG_FS2{"fs2"},
  REG_X19{"x19"}, REG_S3{"s3"},     REG_F19{"f19"}, REG_FS3{"fs3"},
  REG_X20{"x20"}, REG_S4{"s4"},     REG_F20{"f20"}, REG_FS4{"fs4"},
  REG_X21{"x21"}, REG_S5{"s5"},     REG_F21{"f21"}, REG_FS5{"fs5"},
  REG_X22{"x22"}, REG_S6{"s6"},     REG_F22{"f22"}, REG_FS6{"fs6"},
  REG_X23{"x23"}, REG_S7{"s7"},     REG_F23{"f23"}, REG_FS7{"fs7"},
  REG_X24{"x24"}, REG_S8{"s8"},     REG_F24{"f24"}, REG_FS8{"fs8"},
  REG_X25{"x25"}, REG_S9{"s9"},     REG_F25{"f25"}, REG_FS9{"fs9"},
  REG_X26{"x26"}, REG_S10{"s10"},   REG_F26{"f26"}, REG_FS10{"fs10"},
  REG_X27{"x27"}, REG_S11{"s11"},   REG_F27{"f27"}, REG_FS11{"fs11"},
  REG_X28{"x28"}, REG_T3{"t3"},     REG_F28{"f28"}, REG_FT8{"ft8"},
  REG_X29{"x29"}, REG_T4{"t4"},     REG_F29{"f29"}, REG_FT9{"ft9"},
  REG_X30{"x30"}, REG_T5{"t5"},     REG_F30{"f30"}, REG_FT10{"ft10"},
  REG_X31{"x31"}, REG_T6{"t6"},     REG_F31{"f31"}, REG_FT11{"ft11"}
;


typedef int StackMemoryOffset;
typedef std::string LabelInAssembly;
/**
 * AST node and SymbolTableEntry which represent a value or var_ref should contain a MemoryLocation
 * For AST node
 *  - LabelInAssembly
 *      : the AST node is a ID of global variable
 *  - StackMemoryOffset && !(isAbsoluteMemoryAddress)
 *      : the AST node is a local variable or temporary value
 *  - StackMemoryOffset && isAbsoluteMemoryAddress
 *      : the AST node is an array dereference, $(fp+offset) stores the address to the array element
 * For SymbolTabelEntry
 *  - LabelInAssembly
 *      : the symbol is a global variable
 *  - StackMemoryOffset && !(isAbsoluteMemoryAddress)
 *      : the symbol is a local variable
 *  - StackMemoryOffset && isAbsoluteMemoryAddress
 *      : impossible
 */
struct MemoryLocation {
  bool isAbsoluteMemoryAddress;
  std::variant<StackMemoryOffset, LabelInAssembly> value;
  MemoryLocation () : isAbsoluteMemoryAddress{false}, value{0} {}
  MemoryLocation (bool absolute, const StackMemoryOffset &offset) : isAbsoluteMemoryAddress{absolute}, value{offset} {}
  MemoryLocation (bool absolute, const LabelInAssembly &label) : isAbsoluteMemoryAddress{absolute}, value{label} {}
  MemoryLocation (const StackMemoryOffset &offset) : isAbsoluteMemoryAddress{false}, value{offset} {}
  MemoryLocation (const LabelInAssembly &label) : isAbsoluteMemoryAddress{false}, value{label} {}
};



typedef std::string AssemblySection;
const AssemblySection
  ASSEMBLY_DATA_SECTION{".data"},
  ASSEMBLY_TEXT_SECTION{".text"}
;


struct AST {
  AST_TYPE nodeType;
  TypeDescriptor dataType;
  int linenumber;
  std::vector<AST *> children;
  std::variant<IdentifierSemanticValue, STMTSemanticValue, DECLSemanticValue, EXPRSemanticValue,
               Const>
      semanticValue;
  MemoryLocation place;
  AST(AST_TYPE _nodeType = NUL_NODE)
      : nodeType(_nodeType), dataType(NONE_TYPE), linenumber(lineno) {}
};

#endif  // __HEADER_HH__
