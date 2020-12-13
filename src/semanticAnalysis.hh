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
  bool isTypeCompatible(const TypeDescriptor &lhsTypeDesc, const TypeDescriptor &rhsTypeDesc);
  void processProgramNode(AST *programNode);
  void processVariableDeclListNode(AST *variableDeclListNode);
  void processVariableDeclaration(AST *declarationNode);
  void processTypeDeclaration(AST *declarationNode);

  void processFunctionDeclaration(AST *declarationNode);  // only declare
  void processFunctionDefinition(AST *declarationNode);   // with definition
  std::vector<TypeDescriptor> processParameterDeclList(AST *paramListNode);

  void processBlockNode(AST *blockNode);
  void processStatementListNode(AST *statementListNode);
  void processFunctionCallStatement(AST *statementNode);

  void processTypeSpecifier(AST *typeSpecifier);
  TypeDescriptor getDeclaratorType(const TypeDescriptor &typeSpecifierTypeDesc, AST *declarator);
  void processEnumNode(AST *enumNode);

  void processExpressionComponent(AST *expressionComponent);  // every thing with a data type
  void processExpressionNode(AST *expressionNode);
  void processConstNode(AST *constNode);
  void processIdentifierLValue(AST *identifierNode);

 public:
  SemanticAnalysis(AST *_prog);
  void runAnalysis();
  bool anySemanticError;
};

#endif  // ! __SEMANTIC_ANALYSIS_HH__