#include "print.hh"

#include <cassert>
#include <cstdio>
#include <iostream>
#include <string>

#include "header.hh"

void ASTPrinter::print(const std::string filename /* = "AST_Graph.gv" */) {
  os.open(filename);
  os << "Digraph AST {\n";
  os << "label = \"" << filename << "\"\n";

  nextNodeID = 1;
  printGVNode(root, 0);
  printSubtree(root, 0);

  os << "}\n";
  os.close();
}

void ASTPrinter::printSubtree(AST *node, int nodeID) {
  assert(node != nullptr);
  int childrenIDStart = nextNodeID;
  nextNodeID += node->children.size();
  for (int i = 0; i < (int)node->children.size(); ++i)
    printGVNode(node->children[i], childrenIDStart + i);
  for (int i = 0; i < (int)node->children.size(); ++i) printGVEdge(nodeID, childrenIDStart + i);
  for (int i = 0; i < (int)node->children.size(); ++i)
    printSubtree(node->children[i], childrenIDStart + i);
}

void ASTPrinter::printGVEdge(int fromNodeID, int toNodeID) {
  os << "node" << fromNodeID << " -> node" << toNodeID << " [style = bold]\n";
}

void ASTPrinter::printGVNode(AST *node, int nodeID) {
  os << "node" << nodeID << " [label =\"";
  os << AST_TYPE_str[(size_t)node->nodeType];
  switch (node->nodeType) {
    case DECLARATION_NODE:
      os << " " << DECL_KIND_str[(size_t)std::get<DECLSemanticValue>(node->semanticValue).kind];
      break;
    case STMT_NODE:
      os << " " << STMT_KIND_str[(size_t)std::get<STMTSemanticValue>(node->semanticValue).kind];
      break;
    case IDENTIFIER_NODE:
      os << " '" << std::get<IdentifierSemanticValue>(node->semanticValue).identifierName << "' "
         << IDENTIFIER_KIND_str[(size_t)std::get<IdentifierSemanticValue>(node->semanticValue).kind];
      break;
    default:
      break;
  }
  os << "\"]\n";
}