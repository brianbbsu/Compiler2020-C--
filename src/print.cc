#include "print.hh"

#include <cassert>
#include <cstdio>
#include <iostream>
#include <string>

#include "header.hh"
#include "symbolTable.hh"

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
         << IDENTIFIER_KIND_str[(size_t)std::get<IdentifierSemanticValue>(node->semanticValue)
                                    .kind];
      break;
    case CONST_VALUE_NODE:
      os << " " << std::get<Const>(node->semanticValue);
      break;
    case EXPR_NODE:
      {
        const EXPRSemanticValue &exprSemanticValue = std::get<EXPRSemanticValue>(node->semanticValue);
        if (exprSemanticValue.kind == UNARY_OPERATION)
          os << " " << UNARY_OPERATOR_str[(size_t)std::get<UNARY_OPERATOR>(exprSemanticValue.op)];
        else
          os << " " << BINARY_OPERATOR_str[(size_t)std::get<BINARY_OPERATOR>(exprSemanticValue.op)];
      }
    default:
      break;
  }
  os << "\"]\n";
}

std::ostream &operator<<(std::ostream &os, UNARY_OPERATOR op) {
  return os << UNARY_OPERATOR_str[(size_t)op];
}

std::ostream &operator<<(std::ostream &os, BINARY_OPERATOR op) {
  return os << BINARY_OPERATOR_str[(size_t)op];
}

std::ostream &operator<<(std::ostream &os, SymbolKind op) {
  return os << SymbolKind_str[(size_t)op];
}

std::ostream &operator<<(std::ostream &os, const TypeDescriptor &typeDesc) {
  if (typeDesc.type != ARR_TYPE) {
    return os << DATA_TYPE_str[(size_t)typeDesc.type];
  } else {
    const ArrayProperties &arrayProperties = typeDesc.arrayProperties;
    assert(arrayProperties.elementType == INT_TYPE || arrayProperties.elementType == FLOAT_TYPE);
    os << DATA_TYPE_str[arrayProperties.elementType] << "[]";
    for (size_t i = 1; i < arrayProperties.dimensions.size(); ++i)
      os << "[" << arrayProperties.dimensions[i] << "]";
    return os;
  }
}

std::ostream &operator<<(std::ostream &os, const Const &c) {
  switch (c.const_type) {
    case INTEGERC:
      os << std::get<int>(c.value);
      break;
    case FLOATC:
      os << std::get<float>(c.value);
      break;
    case STRINGC:
      os << '"' << std::get<std::string>(c.value) << '"';
      break;
    default:
      raiseError("Unknown const type");
  }
  return os;
}
