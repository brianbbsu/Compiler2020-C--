#ifndef __SEMANTIC_ANALYSIS_HH__
#define __SEMANTIC_ANALYSIS_HH__

#include <iostream>
#include <string>

#include "header.hh"
#include "print.hh"
#include "symbolTable.hh"

class SemanticAnalysis {
  AST *prog;
  std::string inputFilename;
  SymbolTable symbolTable;

  template <typename... Args>
  void semanticError(AST *errorNode, const Args &... args) {
    printHelper(std::cerr, "Error found in line ",
                errorNode->linenumber, ": ", args...);
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
  std::vector<TypeDescriptor> processParameterDeclList(AST *paramListNode, bool isDefinition);

  void processBlockNode(AST *blockNode);
  void processStatement(AST *statementNode);
  void processFunctionCallStatement(AST *statementNode);
  void processIfStatement(AST *statementNode);
  void processForStatement(AST *statementNode);
  void processWhileStatement(AST *statementNode);
  void processAssignmentStatement(AST *statementNode);
  void processReturnStatement(AST *statementNode);

  void processTypeSpecifier(AST *typeSpecifier);
  TypeDescriptor getDeclaratorType(const TypeDescriptor &typeSpecifierTypeDesc, AST *declarator);
  void processEnumNode(AST *enumNode);

  void processExpressionComponent(AST *expressionComponent);  // every thing with a data type
  void processExpressionNode(AST *expressionNode);
  void processConstNode(AST *constNode);
  void processIdentifierRValue(AST *identifierNode);
  void processIdentifierLValue(AST *identifierNode);

 public:
  SemanticAnalysis(AST *_prog, const std::string &inputFilename);
  void runAnalysis();
  bool anySemanticError;
};

#endif  // ! __SEMANTIC_ANALYSIS_HH__