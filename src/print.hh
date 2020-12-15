#ifndef __PRINT_HH__
#define __PRINT_HH__

#include <fstream>
#include <iostream>
#include <string>

#include "header.hh"
#include "symbolTable.hh"

#define ANSI_RED "\x1b[31m"
#define ANSI_BOLD "\x1b[1m"
#define ANSI_RESET "\x1b[0m"

const std::string DATA_TYPE_str[] = {"int",       "float",        "void",
                                     "ARR_TYPE",  "const string", "WRITE_PARAMETER_TYPE",
                                     "NONE_TYPE", "ERROR_TYPE"};

const std::string IDENTIFIER_KIND_str[] = {"NORMAL_ID", "ARRAY_ID", "WITH_INIT_ID"};

const std::string BINARY_OPERATOR_str[] = {
    "+", "-", "*", "/", "==", ">=", "<=", "!=", ">", "<", "&&", "||"};

const std::string UNARY_OPERATOR_str[] = {"plus", "minus", "not"};

const std::string STMT_KIND_str[] = {"WHILE_STMT", "FOR_STMT",           "ASSIGN_STMT",
                                     "IF_STMT",    "FUNCTION_CALL_STMT", "RETURN_STMT"};

const std::string DECL_KIND_str[] = {"VARIABLE_DECL", "TYPE_DECL", "ENUM_DECL", "FUNCTION_DECL",
                                     "FUNCTION_PARAMETER_DECL"};

const std::string AST_TYPE_str[] = {"PROGRAM_NODE",
                                    "DECLARATION_NODE",
                                    "IDENTIFIER_NODE",
                                    "ENUM_NODE",
                                    "PARAM_LIST_NODE",
                                    "NUL_NODE",
                                    "BLOCK_NODE",
                                    "VARIABLE_DECL_LIST_NODE",
                                    "STMT_LIST_NODE",
                                    "STMT_NODE",
                                    "EXPR_NODE",
                                    "CONST_VALUE_NODE",
                                    "NONEMPTY_ASSIGN_EXPR_LIST_NODE",
                                    "NONEMPTY_RELOP_EXPR_LIST_NODE"};

const std::string SymbolKind_str[] = {"variable", "type", "function", "enumerator"};

class ASTPrinter {
  AST *root;
  std::ofstream os;
  int nextNodeID;
  void printSubtree(AST *node, int nodeID);
  void printGVNode(AST *node, int nodeID);
  void printGVEdge(int fromNodeID, int toNodeID);

 public:
  ASTPrinter(AST *_rt) : root(_rt) {}
  void print(const std::string filename = "AST_Graph.gv");
};

std::ostream &operator<<(std::ostream &os, UNARY_OPERATOR op);
std::ostream &operator<<(std::ostream &os, BINARY_OPERATOR op);
std::ostream &operator<<(std::ostream &os, SymbolKind op);
std::ostream &operator<<(std::ostream &os, const TypeDescriptor &typeDesc);
std::ostream &operator<<(std::ostream &os, const Const &c);

#endif  // ! __PRINT_HH__