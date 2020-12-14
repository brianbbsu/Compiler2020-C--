#include "semanticAnalysis.hh"

#include <cassert>
#include <cstdlib>
#include <iostream>
#include <limits>

#include "print.hh"

SemanticAnalysis::SemanticAnalysis(AST *_prog) : prog(_prog), symbolTable() {}

void SemanticAnalysis::runAnalysis() {
  symbolTable.resetSymbolTable();
  anySemanticError = false;
  processProgramNode(prog);
}

bool SemanticAnalysis::isConst(AST *node) {
  if (node->nodeType == CONST_VALUE_NODE) {
    C_type &type = std::get<Const>(node->semanticValue).const_type;
    return type == INTEGERC || type == FLOATC;
  } else if (node->nodeType == EXPR_NODE)
    return std::get<EXPRSemanticValue>(node->semanticValue).isConstEval;
  else if (node->nodeType == IDENTIFIER_NODE)
    return std::get<IdentifierSemanticValue>(node->semanticValue).isEnumerator;
  return false;
}

template <typename T>
T SemanticAnalysis::getConstValue(AST *node) {
  assert(isConst(node));
  auto visitor = [&](auto &&arg) -> T {
    using V = std::decay_t<decltype(arg)>;
    if constexpr (std::is_same_v<V, int> || std::is_same_v<V, float>)
      return static_cast<T>(arg);
    else
      raiseError("Unknown variant value type in getConstValue");
  };
  if (node->nodeType == CONST_VALUE_NODE)
    return std::visit(visitor, std::get<Const>(node->semanticValue).value);
  else if (node->nodeType == EXPR_NODE)
    return std::visit(visitor, std::get<EXPRSemanticValue>(node->semanticValue).constEvalValue);
  else if (node->nodeType == IDENTIFIER_NODE)
    return static_cast<T>(std::get<IdentifierSemanticValue>(node->semanticValue).enumeratorValue);
  raiseError("Unknown node type in getConstValue");
}

template <typename T, typename R>
void SemanticAnalysis::tryBinaryConstEval(AST *exprNode, AST *lOperand, AST *rOperand,
                                          BINARY_OPERATOR op) {
  assert(exprNode->nodeType == EXPR_NODE);
  if (!isConst(lOperand) || !isConst(rOperand)) return;
  T lVal = getConstValue<T>(lOperand);
  T rVal = getConstValue<T>(rOperand);
  R nVal;
  if (op == BINARY_OP_ADD)
    nVal = static_cast<R>(lVal + rVal);
  else if (op == BINARY_OP_SUB)
    nVal = static_cast<R>(lVal - rVal);
  else if (op == BINARY_OP_MUL)
    nVal = static_cast<R>(lVal * rVal);
  else if (op == BINARY_OP_DIV) {
    if (rVal == static_cast<T>(0)) return;
    nVal = static_cast<R>(lVal / rVal);
  } else if (op == BINARY_OP_EQ)
    nVal = static_cast<R>(lVal == rVal);
  else if (op == BINARY_OP_GE)
    nVal = static_cast<R>(lVal >= rVal);
  else if (op == BINARY_OP_LE)
    nVal = static_cast<R>(lVal <= rVal);
  else if (op == BINARY_OP_NE)
    nVal = static_cast<R>(lVal != rVal);
  else if (op == BINARY_OP_GT)
    nVal = static_cast<R>(lVal > rVal);
  else if (op == BINARY_OP_LT)
    nVal = static_cast<R>(lVal < rVal);
  else if (op == BINARY_OP_AND)
    nVal = static_cast<R>(lVal && rVal);
  else if (op == BINARY_OP_OR)
    nVal = static_cast<R>(lVal || rVal);
  else {
    raiseError("Unknown binary operator to do constant evaluation");
  }
  EXPRSemanticValue &exprSemanticValue = std::get<EXPRSemanticValue>(exprNode->semanticValue);
  exprSemanticValue.isConstEval = true;
  exprSemanticValue.constEvalValue.emplace<R>(nVal);
}

template <typename T, typename R>
void SemanticAnalysis::tryUnaryConstEval(AST *exprNode, AST *operand, UNARY_OPERATOR op) {
  assert(exprNode->nodeType == EXPR_NODE);
  if (!isConst(operand)) return;
  T val = getConstValue<T>(operand);
  R nVal;
  if (op == UNARY_OP_POSITIVE)
    nVal = static_cast<R>(+val);
  else if (op == UNARY_OP_NEGATIVE)
    nVal = static_cast<R>(-val);
  else if (op == UNARY_OP_LOGICAL_NEGATION)
    nVal = static_cast<R>(!val);
  else {
    raiseError("Unknown unary operator to do constant evaluation");
  }
  EXPRSemanticValue &exprSemanticValue = std::get<EXPRSemanticValue>(exprNode->semanticValue);
  exprSemanticValue.isConstEval = true;
  exprSemanticValue.constEvalValue.emplace<R>(nVal);
}

DATA_TYPE SemanticAnalysis::getLargerType(DATA_TYPE type1, DATA_TYPE type2) {
  assert(type1 == INT_TYPE || type1 == FLOAT_TYPE);
  assert(type2 == INT_TYPE || type2 == FLOAT_TYPE);
  if (type1 == FLOAT_TYPE || type2 == FLOAT_TYPE)
    return FLOAT_TYPE;
  else
    return INT_TYPE;
}

bool SemanticAnalysis::isTypeCompatible(const TypeDescriptor &lhsTypeDesc,
                                        const TypeDescriptor &rhsTypeDesc) {
  if ((lhsTypeDesc.type == ARR_TYPE) ^ (rhsTypeDesc.type == ARR_TYPE)) return false;
  if (lhsTypeDesc.type == ARR_TYPE) {
    const ArrayProperties &lhsArrayProperties = lhsTypeDesc.arrayProperties;
    const ArrayProperties &rhsArrayProperties = rhsTypeDesc.arrayProperties;
    if (lhsArrayProperties.elementType != rhsArrayProperties.elementType ||
        lhsArrayProperties.dimensions.size() != rhsArrayProperties.dimensions.size())
      return false;
    for (size_t idx = 1; idx < lhsArrayProperties.dimensions.size(); ++idx)
      if (lhsArrayProperties.dimensions[idx] != rhsArrayProperties.dimensions[idx]) return false;
    return true;
  }
  if (lhsTypeDesc.type == WRITE_PARAMETER_TYPE)
    return rhsTypeDesc.type == INT_TYPE || rhsTypeDesc.type == FLOAT_TYPE ||
           rhsTypeDesc.type == CONST_STRING_TYPE;
  if ((lhsTypeDesc.type == INT_TYPE || lhsTypeDesc.type == FLOAT_TYPE) &&
      (rhsTypeDesc.type == INT_TYPE || rhsTypeDesc.type == FLOAT_TYPE))
    return true;
  return false;
}

void SemanticAnalysis::processProgramNode(AST *programNode) {
  assert(programNode->nodeType == PROGRAM_NODE);
  for (AST *child : programNode->children) {
    switch (child->nodeType) {
      case VARIABLE_DECL_LIST_NODE:
        processVariableDeclListNode(child);
        break;
      case DECLARATION_NODE:
        processFunctionDefinition(child);
        break;
      default:
        raiseError("Unknown child node type");
    }
  }
}

void SemanticAnalysis::processVariableDeclListNode(AST *variableDeclListNode) {
  assert(variableDeclListNode->nodeType == VARIABLE_DECL_LIST_NODE);

  for (AST *child : variableDeclListNode->children) {
    assert(child->nodeType == DECLARATION_NODE);
    DECL_KIND kind = std::get<DECLSemanticValue>(child->semanticValue).kind;
    switch (kind) {
      case VARIABLE_DECL:
        processVariableDeclaration(child);
        break;
      case TYPE_DECL:
        processTypeDeclaration(child);
        break;
      case ENUM_DECL:
        assert(child->children.size() == 1);
        processEnumNode(child->children[0]);
        break;
      case FUNCTION_DECL:
        processFunctionDeclaration(child);
        break;
      default:
        raiseError("Unknown child DECL_KIND");
    }
  }
}

void SemanticAnalysis::processVariableDeclaration(AST *declarationNode) {
  assert(declarationNode->nodeType == DECLARATION_NODE);
  assert(std::get<DECLSemanticValue>(declarationNode->semanticValue).kind == VARIABLE_DECL);
  assert(declarationNode->children.size() >= 2);

  AST *typeSpecifierNode = declarationNode->children[0];
  processTypeSpecifier(typeSpecifierNode);
  if (typeSpecifierNode->dataType.type == ERROR_TYPE) return;
  const TypeDescriptor &typeDesc = typeSpecifierNode->dataType;
  for (size_t idx = 1; idx < declarationNode->children.size(); ++idx) {
    /*
      The order should be:
      1. Get extra array dimensions if it's ARRAY_ID.
      2. Deduce actual type and full dimensions from the type
         description and the extra array dimensions from step 1.
      3. Parse initial value if exists. Raise error when assigning
         initial value to array variable or when the type is incompatible.
      4. Declare the variable.
    */
    // Step 1 & 2
    AST *variableIDNode = declarationNode->children[idx];
    const TypeDescriptor variableTypeDesc = getDeclaratorType(typeDesc, variableIDNode);
    if (variableTypeDesc.type == ERROR_TYPE) continue;
    // Step 3
    IdentifierSemanticValue variableSemanticValue =
        std::get<IdentifierSemanticValue>(variableIDNode->semanticValue);
    if (variableSemanticValue.kind == WITH_INIT_ID) {
      // This should not affect symbol table (right?)
      assert(variableIDNode->children.size() == 1);
      AST *initValueNode = variableIDNode->children[0];
      processExpressionComponent(initValueNode);
      if (initValueNode->dataType.type == ERROR_TYPE) break;
      if (variableTypeDesc.type == ARR_TYPE) {
        semanticError(variableIDNode->linenumber, "cannot declare array variable with initializer");
        continue;
      }
      if (!isTypeCompatible(variableTypeDesc, initValueNode->dataType)) {
        semanticError(initValueNode->linenumber, "invalid initializer");
        continue;
      }
      if (symbolTable.isGlobalScope() && !isConst(initValueNode)) {
        semanticError(initValueNode->linenumber, "initializer element is not constant");
        continue;
      }
    }
    // Step 4
    if (variableTypeDesc.type == VOID_TYPE) {
      semanticError(variableIDNode->linenumber, "variable or field '",
                    variableSemanticValue.identifierName, "' declared void");
      continue;
    }
    if (variableTypeDesc.type == ARR_TYPE &&
        variableTypeDesc.arrayProperties.elementType == VOID_TYPE) {
      semanticError(variableIDNode->linenumber, "declaration of ‘",
                    variableSemanticValue.identifierName, "’ as array of voids");
      continue;
    }
    if (symbolTable.declaredLocally(variableSemanticValue.identifierName)) {
      SymbolTableEntry *origEntry = symbolTable.getSymbol(variableSemanticValue.identifierName);
      if (origEntry->symbolKind != VARIABLE_SYMBOL) {
        semanticError(variableIDNode->linenumber, "'", variableSemanticValue.identifierName,
                      "' redeclared as different kind of symbol");
        continue;
      }
      TypeDescriptor &origTypeDesc = std::get<TypeDescriptor>(origEntry->attribute);
      if (origTypeDesc == variableTypeDesc) {
        semanticError(variableIDNode->linenumber, "redeclaration of '",
                      variableSemanticValue.identifierName, "'");
        continue;
      } else {
        semanticError(variableIDNode->linenumber, "conflicting types for '",
                      variableSemanticValue.identifierName, "'");
        continue;
      }
    }
    symbolTable.addVariableSymbol(variableSemanticValue.identifierName,
                                  std::move(variableTypeDesc));
  }
}

void SemanticAnalysis::processTypeDeclaration(AST *declarationNode) {
  assert(declarationNode->nodeType == DECLARATION_NODE);
  assert(std::get<DECLSemanticValue>(declarationNode->semanticValue).kind == TYPE_DECL);
  assert(declarationNode->children.size() >= 2);

  AST *typeSpecifierNode = declarationNode->children[0];
  processTypeSpecifier(typeSpecifierNode);
  if (typeSpecifierNode->dataType.type == ERROR_TYPE) return;
  const TypeDescriptor &typeSpecifierTypeDesc = typeSpecifierNode->dataType;

  for (size_t idx = 1; idx < declarationNode->children.size(); ++idx) {
    AST *declaratorIDNode = declarationNode->children[idx];
    const TypeDescriptor declaratorTypeDesc =
        getDeclaratorType(typeSpecifierTypeDesc, declaratorIDNode);
    if (declaratorTypeDesc.type == ERROR_TYPE) continue;
    const IdentifierSemanticValue &declaratorSemanticValue =
        std::get<IdentifierSemanticValue>(declaratorIDNode->semanticValue);
    assert(declaratorSemanticValue.kind != WITH_INIT_ID);
    if (symbolTable.declaredLocally(declaratorSemanticValue.identifierName)) {
      SymbolTableEntry *origEntry = symbolTable.getSymbol(declaratorSemanticValue.identifierName);
      if (origEntry->symbolKind != TYPE_SYMBOL) {
        semanticError(declaratorIDNode->linenumber, "'", declaratorSemanticValue.identifierName,
                      "' redeclared as different kind of symbol");
        continue;
      }
      TypeDescriptor &origTypeDesc = std::get<TypeDescriptor>(origEntry->attribute);
      if (!(origTypeDesc == declaratorTypeDesc)) {
        semanticError(declaratorIDNode->linenumber, "conflicting types for '",
                      declaratorSemanticValue.identifierName, "'");
      }
      continue;  // allow redeclaration of the same type
    }
    symbolTable.addTypeSymbol(declaratorSemanticValue.identifierName,
                              std::move(declaratorTypeDesc));
  }
}

void SemanticAnalysis::processFunctionDeclaration(AST *declarationNode) {
  assert(declarationNode->nodeType == DECLARATION_NODE);
  assert(std::get<DECLSemanticValue>(declarationNode->semanticValue).kind == FUNCTION_DECL);
  assert(declarationNode->children.size() == 3);
  // Children : 0 -> Type, 1 -> Name, 2 -> Argument

  AST *typeSpecifierNode = declarationNode->children[0];
  AST *functionNameIDNode = declarationNode->children[1];
  AST *parameterListNode = declarationNode->children[2];

  assert(functionNameIDNode->nodeType == IDENTIFIER_NODE);
  const std::string functionName =
      std::get<IdentifierSemanticValue>(functionNameIDNode->semanticValue).identifierName;

  processTypeSpecifier(typeSpecifierNode);
  const TypeDescriptor &returnTypeDesc = typeSpecifierNode->dataType;
  if (returnTypeDesc.type == ERROR_TYPE) return;
  if (returnTypeDesc.type == ARR_TYPE) {
    semanticError(functionNameIDNode->linenumber, "'", functionName,
                  "' declared as function returning an array");
    return;
  }

  symbolTable.openScope();
  std::vector<TypeDescriptor> parameters = processParameterDeclList(parameterListNode, false);
  symbolTable.closeScope();
  if (parameterListNode->dataType.type == ERROR_TYPE) return;
  if (symbolTable.declaredLocally(functionName)) {
    SymbolTableEntry *origEntry = symbolTable.getSymbol(functionName);
    if (origEntry->symbolKind != FUNCTION_SYMBOL) {
      semanticError(functionNameIDNode->linenumber, "'", functionName,
                    "' redeclared as different kind of symbol");
      return;
    }
    FunctionSignature &origSignature = std::get<FunctionSignature>(origEntry->attribute);
    if (!(origSignature.returnType == returnTypeDesc.type &&
          origSignature.parameters == parameters)) {
      semanticError(functionNameIDNode->linenumber, "conflicting types for '", functionName, "'");
      return;
    }
    // Nothing to do, leave it as it is
  } else {
    symbolTable.addFunctionSymbol(functionName,
                                  {returnTypeDesc.type, std::move(parameters), false});
  }
}

void SemanticAnalysis::processFunctionDefinition(AST *declarationNode) {
  assert(declarationNode->nodeType == DECLARATION_NODE);
  assert(std::get<DECLSemanticValue>(declarationNode->semanticValue).kind == FUNCTION_DECL);
  assert(declarationNode->children.size() == 4);
  // Children : 0 -> Type, 1 -> Name, 2 -> Argument, 3 -> block

  AST *typeSpecifierNode = declarationNode->children[0];
  AST *functionNameIDNode = declarationNode->children[1];
  AST *parameterListNode = declarationNode->children[2];
  AST *bodyBlockNode = declarationNode->children[3];

  assert(functionNameIDNode->nodeType == IDENTIFIER_NODE);
  const std::string functionName =
      std::get<IdentifierSemanticValue>(functionNameIDNode->semanticValue).identifierName;

  processTypeSpecifier(typeSpecifierNode);
  const TypeDescriptor &returnTypeDesc = typeSpecifierNode->dataType;
  if (returnTypeDesc.type == ERROR_TYPE) return;
  if (returnTypeDesc.type == ARR_TYPE) {
    semanticError(functionNameIDNode->linenumber, "'", functionName,
                  "' declared as function returning an array");
    return;
  }

  symbolTable.openScope();
  std::vector<TypeDescriptor> parameters = processParameterDeclList(parameterListNode, true);
  if (parameterListNode->dataType.type == ERROR_TYPE) {
    symbolTable.closeScope();  // skip this function
    return;
  }
  symbolTable.stashScope();  // stash to process function declaration
  SymbolTableEntry *functionEntry = nullptr;
  if (symbolTable.declaredLocally(functionName)) {
    SymbolTableEntry *origEntry = symbolTable.getSymbol(functionName);
    if (origEntry->symbolKind != FUNCTION_SYMBOL) {
      semanticError(functionNameIDNode->linenumber, "'", functionName,
                    "' redeclared as different kind of symbol");
      symbolTable.dropStash();
      return;
    }
    FunctionSignature &origSignature = std::get<FunctionSignature>(origEntry->attribute);
    if (!(origSignature.returnType == returnTypeDesc.type &&
          origSignature.parameters == parameters)) {
      semanticError(functionNameIDNode->linenumber, "conflicting types for '", functionName, "'");
      symbolTable.dropStash();
      return;
    }
    if (origSignature.hasDefinition) {
      semanticError(functionNameIDNode->linenumber, "redefinition of '", functionName, "'");
      symbolTable.dropStash();
      return;
    }
    origSignature.hasDefinition = true;
    functionEntry = origEntry;
  } else {
    functionEntry = symbolTable.addFunctionSymbol(
        functionName, {returnTypeDesc.type, std::move(parameters), true});
  }
  assert(functionEntry != nullptr);
  symbolTable.popStash();
  symbolTable.enterFunction(functionEntry);
  processBlockNode(bodyBlockNode);
  symbolTable.leaveFunction();
  symbolTable.closeScope();
}

std::vector<TypeDescriptor> SemanticAnalysis::processParameterDeclList(AST *paramListNode,
                                                                       bool isDefinition) {
  assert(paramListNode->nodeType == PARAM_LIST_NODE);
  std::vector<TypeDescriptor> parameterDeclList;
  for (size_t idx = 0; idx < paramListNode->children.size(); ++idx) {
    AST *child = paramListNode->children[idx];
    assert(std::get<DECLSemanticValue>(child->semanticValue).kind == FUNCTION_PARAMETER_DECL);
    assert(child->children.size() == 2);
    AST *typeSpecifierNode = child->children[0];
    AST *parameterNameIDNode = child->children[1];
    processTypeSpecifier(typeSpecifierNode);
    if (typeSpecifierNode->dataType.type == ERROR_TYPE) {
      paramListNode->dataType = ERROR_TYPE;
      parameterDeclList.push_back(ERROR_TYPE);
      continue;
    }
    const TypeDescriptor &typeSpecifierTypeDesc = typeSpecifierNode->dataType;
    TypeDescriptor parameterTypeDesc =
        getDeclaratorType(typeSpecifierTypeDesc, parameterNameIDNode);
    if (parameterTypeDesc.type == ERROR_TYPE) {
      paramListNode->dataType = ERROR_TYPE;
      parameterDeclList.push_back(ERROR_TYPE);
      continue;
    }
    if (parameterTypeDesc.type == ARR_TYPE)
      parameterTypeDesc.arrayProperties.dimensions[0] = EMPTY_DIM;  // ignore first dimension
    parameterDeclList.push_back(parameterTypeDesc);

    const IdentifierSemanticValue &parameterNameSemanticValue =
        std::get<IdentifierSemanticValue>(parameterNameIDNode->semanticValue);
    assert(parameterNameSemanticValue.kind == NORMAL_ID ||
           parameterNameSemanticValue.kind == ARRAY_ID);

    const std::string parameterName = parameterNameSemanticValue.identifierName;

    if (parameterName == "") {
      if (isDefinition)
        semanticError(child->linenumber, "parameter name omitted");
      else if (parameterTypeDesc.type == VOID_TYPE)
        semanticError(child->linenumber, "parameter ", idx + 1, " has void type");
      continue;
    }

    if (parameterTypeDesc.type == VOID_TYPE) {
      semanticError(child->linenumber, "parameter ", idx + 1, " ('", parameterName,
                    "') has void type");
      continue;
    }

    if (symbolTable.declaredLocally(parameterName)) {
      SymbolTableEntry *origEntry = symbolTable.getSymbol(parameterName);
      if (origEntry->symbolKind != VARIABLE_SYMBOL) {
        semanticError(parameterNameIDNode->linenumber, "'", parameterName,
                      "' redeclared as different kind of symbol");
        continue;
      }
      const TypeDescriptor &origTypeDesc = std::get<TypeDescriptor>(origEntry->attribute);
      if (!(origTypeDesc == parameterTypeDesc)) {
        semanticError(parameterNameIDNode->linenumber, "conflicting types for '", parameterName,
                      "'");
        continue;
      }
      semanticError(parameterNameIDNode->linenumber, "redefinition of parameter '", parameterName,
                    "'");
      continue;
    }
    symbolTable.addVariableSymbol(parameterName, parameterTypeDesc);
  }
  return parameterDeclList;
}

void SemanticAnalysis::processBlockNode(AST *blockNode) {
  assert(blockNode->nodeType == BLOCK_NODE);
  for (AST *child : blockNode->children) {
    switch (child->nodeType) {
      case VARIABLE_DECL_LIST_NODE:
        processVariableDeclListNode(child);
        break;
      case STMT_LIST_NODE:
        for (AST *stmtNode : child->children) processStatement(stmtNode);
        break;
      default:
        raiseError("Unkonwn blockNode child node type");
    }
  }
}

void SemanticAnalysis::processStatement(AST *statementNode) {
  if (statementNode->nodeType == NUL_NODE) return;
  if (statementNode->nodeType == BLOCK_NODE) {
    symbolTable.openScope();
    processBlockNode(statementNode);
    symbolTable.closeScope();
    return;
  }
  assert(statementNode->nodeType == STMT_NODE);
  const STMTSemanticValue &statementSemanticValue =
      std::get<STMTSemanticValue>(statementNode->semanticValue);
  switch (statementSemanticValue.kind) {
    case IF_STMT:
      processIfStatement(statementNode);
      break;
    case FOR_STMT:
      processForStatement(statementNode);
      break;
    case WHILE_STMT:
      processWhileStatement(statementNode);
      break;
    case ASSIGN_STMT:
      processAssignmentStatement(statementNode);
      break;
    case FUNCTION_CALL_STMT:
      processFunctionCallStatement(statementNode);
      break;
    case RETURN_STMT:
      processReturnStatement(statementNode);
      break;
    default:
      raiseError("Unknown statementNode kind");
  }
}

void SemanticAnalysis::processFunctionCallStatement(AST *statementNode) {
  assert(statementNode->nodeType == STMT_NODE);
  assert(std::get<STMTSemanticValue>(statementNode->semanticValue).kind == FUNCTION_CALL_STMT);
  assert(statementNode->children.size() == 2);

  AST *functionNameIDNode = statementNode->children[0];
  AST *parameterListNode = statementNode->children[1];

  assert(functionNameIDNode->nodeType == IDENTIFIER_NODE);
  const std::string functionName =
      std::get<IdentifierSemanticValue>(functionNameIDNode->semanticValue).identifierName;
  SymbolTableEntry *funcEntry = symbolTable.getSymbol(functionName);
  if (funcEntry == nullptr) {
    semanticError(functionNameIDNode->linenumber, "'", functionName,
                  "' was not declared in this scope");
    statementNode->dataType = ERROR_TYPE;
    return;
  }
  if (funcEntry->symbolKind != FUNCTION_SYMBOL) {
    semanticError(functionNameIDNode->linenumber, "called object '", functionName,
                  "' is not a function (but ",
                  (funcEntry->symbolKind == ENUMERATOR_SYMBOL ? "an" : "a"), " ",
                  funcEntry->symbolKind, ")");
    statementNode->dataType = ERROR_TYPE;
    return;
  }
  assert(parameterListNode->nodeType == NONEMPTY_RELOP_EXPR_LIST_NODE ||
         parameterListNode->nodeType == NUL_NODE);
  const FunctionSignature &funcSignature = std::get<FunctionSignature>(funcEntry->attribute);
  statementNode->dataType = funcSignature.returnType;
  if (parameterListNode->children.size() != funcSignature.parameters.size()) {
    if (parameterListNode->children.size() < funcSignature.parameters.size())
      semanticError(functionNameIDNode->linenumber, "too few arguments to function '", functionName,
                    "'");
    else
      semanticError(functionNameIDNode->linenumber, "too many arguments to function '",
                    functionName, "'");
    return;
  }
  for (size_t idx = 0; idx < parameterListNode->children.size(); ++idx) {
    AST *parameterNode = parameterListNode->children[idx];
    processExpressionComponent(parameterNode);
    if (parameterNode->dataType.type == ERROR_TYPE) continue;
    const TypeDescriptor &callTypeDesc = parameterNode->dataType;
    const TypeDescriptor &funcTypeDesc = funcSignature.parameters[idx];
    if (callTypeDesc.type == VOID_TYPE) {
      semanticError(parameterNode->linenumber, "invalid use of void expression");
      continue;
    }
    if (!isTypeCompatible(funcTypeDesc, callTypeDesc)) {
      semanticError(parameterNode->linenumber, "incompatible type for argument ", idx + 1, " of '",
                    functionName, "'");
      continue;
    }
  }
}

void SemanticAnalysis::processIfStatement(AST *statementNode) {
  assert(statementNode->nodeType == STMT_NODE);
  assert(std::get<STMTSemanticValue>(statementNode->semanticValue).kind == IF_STMT);
  assert(statementNode->children.size() == 3);

  AST *testNode = statementNode->children[0];
  AST *trueStatement = statementNode->children[1];
  AST *falseStatement = statementNode->children[2];

  processExpressionComponent(testNode);
  if (testNode->dataType.type != ERROR_TYPE && testNode->dataType.type != INT_TYPE &&
      testNode->dataType.type != FLOAT_TYPE) {
    if (testNode->dataType.type == VOID_TYPE) {
      semanticError(testNode->linenumber, "void value not ignored as it ought to be");
    } else {
      semanticError(testNode->linenumber, "expect scalar type but found type '", testNode->dataType,
                    "'");
    }
  }
  processStatement(trueStatement);
  processStatement(falseStatement);
}

void SemanticAnalysis::processForStatement(AST *statementNode) {
  assert(statementNode->nodeType == STMT_NODE);
  assert(std::get<STMTSemanticValue>(statementNode->semanticValue).kind == FOR_STMT);
  assert(statementNode->children.size() == 4);

  AST *initList = statementNode->children[0];
  AST *condList = statementNode->children[1];
  AST *iterList = statementNode->children[2];
  AST *bodyStatement = statementNode->children[3];

  if (initList->nodeType != NUL_NODE)
    for (AST *child : initList->children) processExpressionComponent(child);
  if (condList->nodeType != NUL_NODE) {
    for (size_t idx = 0; idx < condList->children.size(); ++idx) {
      AST *child = condList->children[idx];
      processExpressionComponent(child);
      if (idx + 1 == condList->children.size()) {
        if (child->dataType.type != ERROR_TYPE && child->dataType.type != INT_TYPE &&
            child->dataType.type != FLOAT_TYPE) {
          if (child->dataType.type == VOID_TYPE) {
            semanticError(child->linenumber, "void value not ignored as it ought to be");
          } else {
            semanticError(child->linenumber, "expect scalar type but found type '", child->dataType,
                          "'");
          }
        }
      }
    }
  }
  if (iterList->nodeType != NUL_NODE)
    for (AST *child : iterList->children) processExpressionComponent(child);
  processStatement(bodyStatement);
}

void SemanticAnalysis::processWhileStatement(AST *statementNode) {
  assert(statementNode->nodeType == STMT_NODE);
  assert(std::get<STMTSemanticValue>(statementNode->semanticValue).kind == WHILE_STMT);
  assert(statementNode->children.size() == 2);

  AST *testNode = statementNode->children[0];
  AST *bodyStatement = statementNode->children[1];

  processExpressionComponent(testNode);
  if (testNode->dataType.type != ERROR_TYPE && testNode->dataType.type != INT_TYPE &&
      testNode->dataType.type != FLOAT_TYPE) {
    if (testNode->dataType.type == VOID_TYPE) {
      semanticError(testNode->linenumber, "void value not ignored as it ought to be");
    } else {
      semanticError(testNode->linenumber, "expect scalar type but found type '", testNode->dataType,
                    "'");
    }
  }
  processStatement(bodyStatement);
}

void SemanticAnalysis::processAssignmentStatement(AST *statementNode) {
  assert(statementNode->nodeType == STMT_NODE);
  assert(std::get<STMTSemanticValue>(statementNode->semanticValue).kind == ASSIGN_STMT);
  assert(statementNode->children.size() == 2);

  AST *lhsIDNode = statementNode->children[0];
  AST *rhsExprNode = statementNode->children[1];

  processIdentifierLValue(lhsIDNode);
  processExpressionComponent(rhsExprNode);
  if (lhsIDNode->dataType.type == ERROR_TYPE || rhsExprNode->dataType.type == ERROR_TYPE) {
    statementNode->dataType = ERROR_TYPE;
    return;
  }
  if (rhsExprNode->dataType.type == VOID_TYPE) {
    semanticError(rhsExprNode->linenumber, "void value not ignored as it ought to be");
    statementNode->dataType = ERROR_TYPE;
    return;
  }
  if (!isTypeCompatible(lhsIDNode->dataType, rhsExprNode->dataType)) {
    semanticError(statementNode->linenumber, "incompatible types when assigning to type '",
                  lhsIDNode->dataType, "' from type '", rhsExprNode->dataType, "'");
    statementNode->dataType = ERROR_TYPE;
    return;
  }
  statementNode->dataType = lhsIDNode->dataType;
}

void SemanticAnalysis::processReturnStatement(AST *statementNode) {
  assert(statementNode->nodeType == STMT_NODE);
  assert(std::get<STMTSemanticValue>(statementNode->semanticValue).kind == RETURN_STMT);
  assert(statementNode->children.size() == 1);

  AST *exprNode = statementNode->children[0];
  SymbolTableEntry *funcEntry = symbolTable.getCurrentFunction();
  const TypeDescriptor &funcReturnTypeDesc =
      std::get<FunctionSignature>(funcEntry->attribute).returnType;
  if (exprNode->nodeType == NUL_NODE) {
    if (funcReturnTypeDesc.type != VOID_TYPE) {
      semanticError(statementNode->linenumber,
                    "'return' with no value, in function returning non-void");
      return;
    }
    // no error
    return;
  }
  processExpressionComponent(exprNode);
  const TypeDescriptor &exprTypeDesc = exprNode->dataType;
  if (exprTypeDesc.type == ERROR_TYPE) return;
  if (exprTypeDesc.type == VOID_TYPE) {
    if (funcReturnTypeDesc.type != VOID_TYPE) {
      semanticError(exprNode->linenumber, "void value not ignored as it ought to be");
      return;
    }
    // no error
    return;
  }
  if (funcReturnTypeDesc.type == VOID_TYPE) {
    semanticError(exprNode->linenumber, "'return' with a value, in function returning void");
    return;
  }
  if (!isTypeCompatible(funcReturnTypeDesc, exprTypeDesc)) {
    semanticError(exprNode->linenumber, "incompatible types when returning type '", exprTypeDesc,
                  "' but '", funcReturnTypeDesc, "' was expected");
    return;
  }
}

void SemanticAnalysis::processTypeSpecifier(AST *typeSpecifier) {
  if (typeSpecifier->nodeType == IDENTIFIER_NODE) {
    assert(std::get<IdentifierSemanticValue>(typeSpecifier->semanticValue).kind == NORMAL_ID);
    const std::string &typeName =
        std::get<IdentifierSemanticValue>(typeSpecifier->semanticValue).identifierName;
    SymbolTableEntry *typeEntry = symbolTable.getSymbol(typeName);
    if (typeEntry == nullptr) {
      semanticError(typeSpecifier->linenumber, "unknown type name '", typeName, "'");
      typeSpecifier->dataType = ERROR_TYPE;
    } else if (typeEntry->symbolKind != TYPE_SYMBOL) {
      semanticError(typeSpecifier->linenumber, "'", typeName, "' is not a type (but ",
                    (typeEntry->symbolKind == ENUMERATOR_SYMBOL ? "an" : "a"), " ",
                    typeEntry->symbolKind, ")");
      typeSpecifier->dataType = ERROR_TYPE;
    } else
      typeSpecifier->dataType = std::get<TypeDescriptor>(typeEntry->attribute);
  } else if (typeSpecifier->nodeType == ENUM_NODE) {
    processEnumNode(typeSpecifier);
  } else
    raiseError("Unknown type specifier node type");
  assert(typeSpecifier->dataType.type != NONE_TYPE);
}

TypeDescriptor SemanticAnalysis::getDeclaratorType(const TypeDescriptor &typeSpecifierTypeDesc,
                                                   AST *declarator) {
  assert(declarator->nodeType == IDENTIFIER_NODE);
  IdentifierSemanticValue declaratorSemanticValue =
      std::get<IdentifierSemanticValue>(declarator->semanticValue);
  const std::string &declaratorName = declaratorSemanticValue.identifierName;
  if (declaratorSemanticValue.kind == NORMAL_ID || declaratorSemanticValue.kind == WITH_INIT_ID)
    return typeSpecifierTypeDesc;
  assert(declaratorSemanticValue.kind == ARRAY_ID);
  TypeDescriptor declaratorTypeDesc;
  declaratorTypeDesc.type = ARR_TYPE;
  ArrayProperties &arrayProperties = declaratorTypeDesc.arrayProperties;
  if (typeSpecifierTypeDesc.type == ARR_TYPE)
    arrayProperties.elementType = typeSpecifierTypeDesc.arrayProperties.elementType;
  else
    arrayProperties.elementType = typeSpecifierTypeDesc.type;
  bool anyError = false;
  for (AST *dimComponent : declarator->children) {
    if (dimComponent->nodeType == NUL_NODE) {
      arrayProperties.dimensions.push_back(EMPTY_DIM);
      continue;
    }
    processExpressionComponent(dimComponent);
    if (dimComponent->dataType.type == ERROR_TYPE) {
      anyError = true;
      continue;
    } else if (dimComponent->dataType.type != INT_TYPE) {
      if (declaratorName == "")
        semanticError(declarator->linenumber, "size of unnamed array has non-integer type");
      else
        semanticError(declarator->linenumber, "size of array '", declaratorName,
                      "' has non-integer type");
      anyError = true;
      break;
    } else if (!isConst(dimComponent)) {
      if (declaratorName == "")
        semanticError(declarator->linenumber, "size of unnamed array is not a constant");
      else
        semanticError(declarator->linenumber, "size of array '", declaratorName,
                      "' is not a constant");
      anyError = true;
      break;
    }
    int dimVal = getConstValue<int>(dimComponent);
    if (dimVal <= 0) {
      if (declaratorName == "")
        semanticError(declarator->linenumber, "size of unnamed array is not positive");
      else
        semanticError(declarator->linenumber, "size of array '", declaratorName,
                      "' is not positive");
      anyError = true;
      break;
    }
    arrayProperties.dimensions.push_back(dimVal);
  }
  if (anyError) return ERROR_TYPE;
  if (typeSpecifierTypeDesc.type == ARR_TYPE)
    arrayProperties.dimensions.insert(arrayProperties.dimensions.end(),
                                      typeSpecifierTypeDesc.arrayProperties.dimensions.begin(),
                                      typeSpecifierTypeDesc.arrayProperties.dimensions.end());

  return declaratorTypeDesc;
}

void SemanticAnalysis::processEnumNode(AST *enumNode) {
  assert(enumNode->nodeType == ENUM_NODE);
  if (enumNode->children.size() == 1) {  // enum ref
    AST *enumNameIDNode = enumNode->children[0];
    assert(enumNameIDNode->nodeType == IDENTIFIER_NODE);
    const IdentifierSemanticValue &enumNameSemanticValue =
        std::get<IdentifierSemanticValue>(enumNameIDNode->semanticValue);
    assert(enumNameSemanticValue.kind == NORMAL_ID);
    const std::string entryName = "enum " + enumNameSemanticValue.identifierName;
    SymbolTableEntry *enumTypeEntry = symbolTable.getSymbol(entryName);
    if (enumTypeEntry == nullptr) {
      semanticError(enumNameIDNode->linenumber, "unknown type name '", entryName, "'");
      enumNode->dataType = ERROR_TYPE;
      return;
    }
    assert(enumTypeEntry->symbolKind == TYPE_SYMBOL);
    enumNode->dataType = std::get<TypeDescriptor>(enumTypeEntry->attribute);
    return;
  }
  // enum def
  AST *enumNameIDNode = enumNode->children[0];
  if (enumNameIDNode->nodeType == IDENTIFIER_NODE) {  // named enum
    const IdentifierSemanticValue &enumNameSemanticValue =
        std::get<IdentifierSemanticValue>(enumNameIDNode->semanticValue);
    assert(enumNameSemanticValue.kind == NORMAL_ID);
    const std::string entryName = "enum " + enumNameSemanticValue.identifierName;
    if (symbolTable.declaredLocally(entryName)) {
      semanticError(enumNameIDNode->linenumber, "redefinition of '", entryName, "'");
      enumNode->dataType = ERROR_TYPE;
      return;
    }
    symbolTable.addTypeSymbol(entryName, INT_TYPE);
  } else
    assert(enumNameIDNode->nodeType == NUL_NODE);
  enumNode->dataType = INT_TYPE;
  int lastEnumeratorValue = -1;
  for (size_t idx = 1; idx < enumNode->children.size(); ++idx) {
    AST *enumeratorIDNode = enumNode->children[idx];
    assert(enumeratorIDNode->nodeType == IDENTIFIER_NODE);
    const IdentifierSemanticValue &enumeratorSemanticValue =
        std::get<IdentifierSemanticValue>(enumeratorIDNode->semanticValue);
    int enumeratorValue;
    bool haveExplicitValue = false;
    if (enumeratorSemanticValue.kind == WITH_INIT_ID) {
      assert(enumeratorIDNode->children.size() == 1);
      AST *valueNode = enumeratorIDNode->children[0];
      processExpressionComponent(valueNode);
      if (valueNode->dataType.type == ERROR_TYPE)  // do nothing
        ;
      else if (valueNode->dataType.type != INT_TYPE || !isConst(valueNode)) {
        semanticError(valueNode->linenumber, "enumerator value for '",
                      enumeratorSemanticValue.identifierName, "' is not an integer constant");
      } else {
        enumeratorValue = getConstValue<int>(valueNode);
        haveExplicitValue = true;
      }
    }
    if (!haveExplicitValue) {
      if (lastEnumeratorValue == std::numeric_limits<int>::max()) {
        semanticError(enumeratorIDNode->linenumber, "overflow in enumeration values");
      }
      enumeratorValue = lastEnumeratorValue + 1;
    }
    lastEnumeratorValue = enumeratorValue;
    if (symbolTable.declaredLocally(enumeratorSemanticValue.identifierName)) {
      SymbolTableEntry *origEntry = symbolTable.getSymbol(enumeratorSemanticValue.identifierName);
      if (origEntry->symbolKind != ENUMERATOR_SYMBOL)
        semanticError(enumeratorIDNode->linenumber, "'", enumeratorSemanticValue.identifierName,
                      "' redeclared as different kind of symbol");
      else
        semanticError(enumeratorIDNode->linenumber, "redeclaration of enumerator '",
                      enumeratorSemanticValue.identifierName, "'");
      continue;
    }
    symbolTable.addEnumeratorSymbol(enumeratorSemanticValue.identifierName, enumeratorValue);
  }
}

void SemanticAnalysis::processExpressionComponent(AST *expressionComponent) {
  switch (expressionComponent->nodeType) {
    case EXPR_NODE:
      processExpressionNode(expressionComponent);
      break;
    case STMT_NODE:
      switch (std::get<STMTSemanticValue>(expressionComponent->semanticValue).kind) {
        case FUNCTION_CALL_STMT:
          processFunctionCallStatement(expressionComponent);
          break;
        case ASSIGN_STMT:
          processAssignmentStatement(expressionComponent);
          break;
        default:
          raiseError("Unknown statement kind in expression");
      }
      break;
    case CONST_VALUE_NODE:
      processConstNode(expressionComponent);
      break;
    case IDENTIFIER_NODE:
      processIdentifierRValue(expressionComponent);
      break;
    default:
      raiseError("Unknown expression component node type");
  }
  assert(expressionComponent->dataType.type != NONE_TYPE);
}

void SemanticAnalysis::processExpressionNode(AST *expressionNode) {
  assert(expressionNode->nodeType == EXPR_NODE);
  EXPRSemanticValue &exprSemanticValue = std::get<EXPRSemanticValue>(expressionNode->semanticValue);

  bool anyError = false;
  for (AST *operandNode : expressionNode->children) {
    processExpressionComponent(operandNode);
    if (operandNode->dataType.type == ERROR_TYPE)
      anyError = true;
    else if (operandNode->dataType.type == VOID_TYPE) {
      semanticError(operandNode->linenumber, "void value not ignored as it ought to be");
      anyError = true;
    }
  }
  if (anyError) {
    expressionNode->dataType = ERROR_TYPE;
    return;
  }
  if (exprSemanticValue.kind == UNARY_OPERATION) {
    assert(expressionNode->children.size() == (size_t)1);
    UNARY_OPERATOR op = std::get<UNARY_OPERATOR>(exprSemanticValue.op);
    AST *operand = expressionNode->children[0];
    const TypeDescriptor &opTypeDesc = operand->dataType;
    if (!(opTypeDesc.type == INT_TYPE || opTypeDesc.type == FLOAT_TYPE)) {
      semanticError(expressionNode->linenumber, "invalid operand to unary ", op, " (have '",
                    opTypeDesc, "')");
      expressionNode->dataType = ERROR_TYPE;
      return;
    }
    const DATA_TYPE opValueType = opTypeDesc.type;
    switch (op) {
      case UNARY_OP_LOGICAL_NEGATION:
        expressionNode->dataType = INT_TYPE;
        if (opValueType == INT_TYPE)
          tryUnaryConstEval<int, int>(expressionNode, operand, op);
        else if (opValueType == FLOAT_TYPE)
          tryUnaryConstEval<float, int>(expressionNode, operand, op);
        break;
      case UNARY_OP_POSITIVE:
      case UNARY_OP_NEGATIVE:
        expressionNode->dataType = opValueType;
        if (opValueType == INT_TYPE)
          tryUnaryConstEval<int, int>(expressionNode, operand, op);
        else if (opValueType == FLOAT_TYPE)
          tryUnaryConstEval<float, float>(expressionNode, operand, op);
        break;
    }
  } else {  // BINARY_OPERATION
    assert(expressionNode->children.size() == (size_t)2);
    BINARY_OPERATOR op = std::get<BINARY_OPERATOR>(exprSemanticValue.op);
    AST *lOperand = expressionNode->children[0];
    AST *rOperand = expressionNode->children[1];
    const TypeDescriptor &lOpTypeDesc = lOperand->dataType;
    const TypeDescriptor &rOpTypeDesc = rOperand->dataType;
    if (!(lOpTypeDesc.type == INT_TYPE || lOpTypeDesc.type == FLOAT_TYPE) ||
        !(rOpTypeDesc.type == INT_TYPE || rOpTypeDesc.type == FLOAT_TYPE)) {
      semanticError(expressionNode->linenumber, "invalid operand to binary ", op, " (have '",
                    lOpTypeDesc, "' and '", rOpTypeDesc, "')");
      expressionNode->dataType = ERROR_TYPE;
      return;
    }
    const DATA_TYPE opValueType = getLargerType(lOpTypeDesc.type, rOpTypeDesc.type);
    switch (op) {
      case BINARY_OP_ADD:
      case BINARY_OP_SUB:
      case BINARY_OP_MUL:
      case BINARY_OP_DIV:
        expressionNode->dataType = opValueType;
        if (opValueType == INT_TYPE)
          tryBinaryConstEval<int, int>(expressionNode, lOperand, rOperand, op);
        else if (opValueType == FLOAT_TYPE)
          tryBinaryConstEval<float, float>(expressionNode, lOperand, rOperand, op);
        break;
      case BINARY_OP_EQ:
      case BINARY_OP_GE:
      case BINARY_OP_LE:
      case BINARY_OP_NE:
      case BINARY_OP_GT:
      case BINARY_OP_LT:
      case BINARY_OP_AND:
      case BINARY_OP_OR:
        expressionNode->dataType = INT_TYPE;
        if (opValueType == INT_TYPE)
          tryBinaryConstEval<int, int>(expressionNode, lOperand, rOperand, op);
        else if (opValueType == FLOAT_TYPE)
          tryBinaryConstEval<float, int>(expressionNode, lOperand, rOperand, op);
        break;
    }
  }
}

void SemanticAnalysis::processConstNode(AST *constNode) {
  assert(constNode->nodeType == CONST_VALUE_NODE);

  switch (std::get<Const>(constNode->semanticValue).const_type) {
    case INTEGERC:
      constNode->dataType = INT_TYPE;
      break;
    case FLOATC:
      constNode->dataType = FLOAT_TYPE;
      break;
    case STRINGC:
      constNode->dataType = CONST_STRING_TYPE;
      break;
  }
}

void SemanticAnalysis::processIdentifierRValue(AST *identifierNode) {
  assert(identifierNode->nodeType == IDENTIFIER_NODE);
  IdentifierSemanticValue &idSemanticValue =
      std::get<IdentifierSemanticValue>(identifierNode->semanticValue);
  assert(idSemanticValue.kind == NORMAL_ID || idSemanticValue.kind == ARRAY_ID);

  SymbolTableEntry *idEntry = symbolTable.getSymbol(idSemanticValue.identifierName);
  if (idEntry == nullptr) {
    semanticError(identifierNode->linenumber, "'", idSemanticValue.identifierName,
                  "' was not declared in this scope");
    identifierNode->dataType = ERROR_TYPE;
    return;
  } else if (!(idEntry->symbolKind == VARIABLE_SYMBOL ||
               idEntry->symbolKind == ENUMERATOR_SYMBOL)) {
    semanticError(identifierNode->linenumber, "'", idSemanticValue.identifierName,
                  "' is not a variable nor an enumerator (but a ", idEntry->symbolKind, ")");
    identifierNode->dataType = ERROR_TYPE;
    return;
  }
  if (idEntry->symbolKind == ENUMERATOR_SYMBOL) {
    if (idSemanticValue.kind == ARRAY_ID) {
      semanticError(identifierNode->linenumber,
                    "subscripted value is neither array nor pointer nor vector");
      identifierNode->dataType = ERROR_TYPE;
      return;
    }
    idSemanticValue.isEnumerator = true;
    idSemanticValue.enumeratorValue = std::get<int>(idEntry->attribute);
    identifierNode->dataType = INT_TYPE;
    return;
  }
  assert(idEntry->symbolKind == VARIABLE_SYMBOL);
  const TypeDescriptor &variableTypeDesc = std::get<TypeDescriptor>(idEntry->attribute);
  if (idSemanticValue.kind == NORMAL_ID) {
    identifierNode->dataType = variableTypeDesc;
  } else {  // ARRAY_ID
    bool anyError = false;
    size_t index_dim = 0;
    for (AST *dimComponent : identifierNode->children) {
      processExpressionComponent(dimComponent);
      if (dimComponent->dataType.type == ERROR_TYPE) {
        anyError = true;
        break;
      } else if (dimComponent->dataType.type != INT_TYPE) {
        semanticError(identifierNode->linenumber, "array subscript is not an integer");
        anyError = true;
        break;
      }
      ++index_dim;
    }
    if (anyError) {
      identifierNode->dataType = ERROR_TYPE;
      return;
    }
    assert(index_dim > 0);
    if (variableTypeDesc.type != ARR_TYPE ||
        index_dim > variableTypeDesc.arrayProperties.dimensions.size()) {
      semanticError(identifierNode->linenumber,
                    "subscripted value is neither array nor pointer nor vector");
      identifierNode->dataType = ERROR_TYPE;
      return;
    }
    TypeDescriptor resultTypeDesc;
    if (index_dim == variableTypeDesc.arrayProperties.dimensions.size()) {
      resultTypeDesc.type = variableTypeDesc.arrayProperties.elementType;
    } else {
      resultTypeDesc = variableTypeDesc;
      std::vector<int> &dimVec = resultTypeDesc.arrayProperties.dimensions;
      dimVec.erase(dimVec.begin(), dimVec.begin() + index_dim);
    }
    identifierNode->dataType = resultTypeDesc;
  }
}

void SemanticAnalysis::processIdentifierLValue(AST *identifierNode) {
  assert(identifierNode->nodeType == IDENTIFIER_NODE);
  const IdentifierSemanticValue &idSemanticValue =
      std::get<IdentifierSemanticValue>(identifierNode->semanticValue);
  assert(idSemanticValue.kind == NORMAL_ID || idSemanticValue.kind == ARRAY_ID);
  SymbolTableEntry *idEntry = symbolTable.getSymbol(idSemanticValue.identifierName);
  if (idEntry == nullptr) {
    semanticError(identifierNode->linenumber, "'", idSemanticValue.identifierName,
                  "' was not declared in this scope");
    identifierNode->dataType = ERROR_TYPE;
    return;
  }
  if (idEntry->symbolKind != VARIABLE_SYMBOL) {
    semanticError(identifierNode->linenumber, "'", idSemanticValue.identifierName,
                  "' is not a variable (but ",
                  (idEntry->symbolKind == ENUMERATOR_SYMBOL ? "an" : "a"), " ", idEntry->symbolKind,
                  ")");
    identifierNode->dataType = ERROR_TYPE;
    return;
  }
  const TypeDescriptor &variableTypeDesc = std::get<TypeDescriptor>(idEntry->attribute);
  if (idSemanticValue.kind == ARRAY_ID) {
    bool anyError = false;
    size_t index_dim = 0;
    for (AST *dimComponent : identifierNode->children) {
      processExpressionComponent(dimComponent);
      if (dimComponent->dataType.type == ERROR_TYPE) {
        anyError = true;
        break;
      } else if (dimComponent->dataType.type != INT_TYPE) {
        semanticError(identifierNode->linenumber, "array subscript is not an integer");
        anyError = true;
        break;
      }
      ++index_dim;
    }
    if (anyError) {
      identifierNode->dataType = ERROR_TYPE;
      return;
    }
    assert(index_dim > 0);
    if (variableTypeDesc.type != ARR_TYPE ||
        index_dim > variableTypeDesc.arrayProperties.dimensions.size()) {
      semanticError(identifierNode->linenumber,
                    "subscripted value is neither array nor pointer nor vector");
      identifierNode->dataType = ERROR_TYPE;
      return;
    }
    if (index_dim < variableTypeDesc.arrayProperties.dimensions.size()) {
      semanticError(identifierNode->linenumber, "assignment to expression with array type");
      identifierNode->dataType = ERROR_TYPE;
      return;
    }
    identifierNode->dataType = variableTypeDesc.arrayProperties.elementType;
  } else {  // NORMAL ID
    if (variableTypeDesc.type == ARR_TYPE) {
      semanticError(identifierNode->linenumber, "assignment to expression with array type");
      identifierNode->dataType = ERROR_TYPE;
      return;
    }
    identifierNode->dataType = variableTypeDesc.type;
  }
}