#ifndef __PRINT_HH__
#define __PRINT_HH__

#include <fstream>
#include <string>

#include "header.hh"

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