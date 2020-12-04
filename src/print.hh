#ifndef __PRINT_HH__
#define __PRINT_HH__

#include <fstream>
#include <string>

#include "header.hh"

const std::string AST_TYPE_str[] = {"PROGRAM_NODE",
                                    "DECLARATION_NODE",
                                    "IDENTIFIER_NODE",
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

#endif  // ! __PRINT_HH__