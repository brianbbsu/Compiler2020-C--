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
    anySemanticError = true;
  }

  bool isConst(AST *node);
  template <typename T>
  T getConstValue(AST *node);
  template <typename T, typename R>
  void tryBinaryConstEval(AST *exprNode, AST *lOperand, AST *rOperand, BINARY_OPERATOR op);
  template <typename T, typename R>
  void tryUnaryConstEval(AST *exprNode, AST *operand, UNARY_OPERATOR op);

  DATA_TYPE getLargerType(DATA_TYPE type1, DATA_TYPE type2);

  void processProgramNode(AST *programNode);
  void processVariableDeclListNode(AST *variableDeclListNode);
  void processVariableDeclaration(AST *declarationNode);
  void processTypeDeclaration(AST *declarationNode);
  void processFunctionDeclaration(AST *declarationNode);  // only declare
  void processFunctionDefinition(AST *declarationNode);   // with definition

  void processExpressionComponent(AST *expressionComponent);  // every thing with a data type
  void processExpressionNode(AST *expressionNode);
  void processFunctionCallStatement(AST *statementNode);
  void processConstNode(AST *constNode);

 public:
  SemanticAnalysis(AST *_prog);
  void runAnalysis();
  bool anySemanticError;
};

#endif  // ! __SEMANTIC_ANALYSIS_HH__