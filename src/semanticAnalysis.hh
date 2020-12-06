#ifndef __SEMANTIC_ANALYSIS_HH__
#define __SEMANTIC_ANALYSIS_HH__

#include <iostream>

#include "header.hh"
#include "symbolTable.hh"

class SemanticAnalysis {
  AST *prog;
  SymbolTable symbolTable;

  template <typename... Args>
  void semanticError(int lineno, const Args &... args) {
    printHelper(std::cerr, "Error found in line ", lineno, "\n", args...);
    anyerror = true;
  }

  void processProgramNode(AST *programNode);
  void processVariableDeclListNode(AST *variableDeclListNode);
  void processVariableDeclaration(AST *declarationNode);
  void processTypeDeclaration(AST *declarationNode);
  void processFunctionDeclaration(AST *declarationNode);  // only declare
  void processFunctionDefinition(AST *declarationNode);   // with definition

 public:
  SemanticAnalysis(AST *_prog);
  void runAnalysis();
  bool anyerror;
};

#endif  // ! __SEMANTIC_ANALYSIS_HH__