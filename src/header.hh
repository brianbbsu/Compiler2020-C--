#ifndef __HEADER_HH__
#define __HEADER_HH__

#include <string>
#include <variant>
#include <vector>

#define MAX_ARRAY_DIMENSION 10

/* Enum declaration */
enum DATA_TYPE {
  INT_TYPE,
  FLOAT_TYPE,
  VOID_TYPE,
  INT_PTR_TYPE,       // for parameter passing
  FLOAT_PTR_TYPE,     // for parameter passing
  CONST_STRING_TYPE,  // for "const string"
  NONE_TYPE,          // for nodes like PROGRAM_NODE which has no type
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

enum UNARY_OPERATOR {
  UNARY_OP_POSITIVE,
  UNARY_OP_NEGATIVE,
  UNARY_OP_LOGICAL_NEGATION
};

// C_type= type of constant ex: 1, 3.3, "const string"
// do not modify, or lexer might break
enum C_type { INTEGERC, FLOATC, STRINGC };

enum STMT_KIND {
  WHILE_STMT,
  FOR_STMT,
  ASSIGN_STMT,  // TODO:for simpler implementation, assign_expr also uses this
  IF_STMT,
  FUNCTION_CALL_STMT,
  RETURN_STMT,
};

enum EXPR_KIND { BINARY_OPERATION, UNARY_OPERATION };

enum DECL_KIND {
  VARIABLE_DECL,
  TYPE_DECL,
  FUNCTION_DECL,
  FUNCTION_PARAMETER_DECL
};

enum AST_TYPE {
  PROGRAM_NODE,
  DECLARATION_NODE,
  IDENTIFIER_NODE,
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

  int isConstEval;
  std::variant<int, double> constEvalValue;
  std::variant<BINARY_OPERATOR, UNARY_OPERATOR> op;
};

struct DECLSemanticValue {
  DECL_KIND kind;
};

struct SymbolAttribute;

struct IdentifierSemanticValue {
  std::string identifierName;
  IDENTIFIER_KIND kind;
  /* struct SymbolTableEntry *symbolTableEntry; */
};

struct TypeSpecSemanticValue {
  std::string typeName;
};

/* Parser Types */
extern int lineno;

struct Const {
  C_type const_type;
  std::variant<int, double, std::string> value;
};

struct AST {
  AST_TYPE nodeType;
  DATA_TYPE dataType;
  int linenumber;
  AST *parent;
  std::vector<AST *> children;
  std::variant<IdentifierSemanticValue, STMTSemanticValue, DECLSemanticValue,
               EXPRSemanticValue, Const>
      semanticValue;
  AST(AST_TYPE _nodeType = NUL_NODE)
      : nodeType(_nodeType),
        dataType(NONE_TYPE),
        linenumber(lineno),
        parent(nullptr) {}
};

#endif  // __HEADER_HH__