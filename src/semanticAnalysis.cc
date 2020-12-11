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
        // TODO:
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
      if (!(initValueNode->dataType.type == INT_TYPE ||
            initValueNode->dataType.type == FLOAT_TYPE)) {
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

void SemanticAnalysis::processFunctionDefinition(AST *declarationNode) {
  assert(declarationNode->nodeType == DECLARATION_NODE);
  assert(std::get<DECLSemanticValue>(declarationNode->semanticValue).kind == FUNCTION_DECL);
  // TODO: Process function definition

  // Children : 0 -> Type, 1 -> Name, 2 -> Argument, 3 -> block

  // Parse type and name before opening scope

  symbolTable.openScope();

  symbolTable.closeScope();
}

void SemanticAnalysis::processTypeSpecifier(AST *typeSpecifier) {
  if (typeSpecifier->nodeType == IDENTIFIER_NODE) {
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
  TypeDescriptor declaratorTypeDesc = typeSpecifierTypeDesc;
  if (declaratorSemanticValue.kind == NORMAL_ID) return declaratorTypeDesc;
  assert(declaratorSemanticValue.kind == ARRAY_ID);
  if (declaratorTypeDesc.type != ARR_TYPE) {
    // Turn scalar type into array type
    declaratorTypeDesc.arrayProperties.elementType = declaratorTypeDesc.type;
    declaratorTypeDesc.arrayProperties.dimensions.clear();  // ensure the array dimension is empty
    declaratorTypeDesc.type = ARR_TYPE;
  }
  ArrayProperties &arrayProperties = declaratorTypeDesc.arrayProperties;
  bool anyError = false;
  for (AST *dimComponent : declarator->children) {
    processExpressionComponent(dimComponent);
    if (dimComponent->dataType.type == ERROR_TYPE) {
      anyError = true;
      continue;
    } else if (dimComponent->dataType.type != INT_TYPE) {
      semanticError(declarator->linenumber, "size of array '",
                    declaratorSemanticValue.identifierName, "' has non-integer type");
      anyError = true;
      break;
    } else if (!isConst(dimComponent)) {
      semanticError(declarator->linenumber, "size of array '",
                    declaratorSemanticValue.identifierName, "' is not a constant");
      anyError = true;
      break;
    }
    int dimVal = getConstValue<int>(dimComponent);
    if (dimVal <= 0) {
      semanticError(declarator->linenumber, "size of array '",
                    declaratorSemanticValue.identifierName, "' is not positive");
      anyError = true;
      break;
    }
    arrayProperties.dimensions.push_back(dimVal);
  }
  if (anyError) return ERROR_TYPE;

  // TODO: remove debug code below
  std::cerr << "New array: ";
  for (size_t _i = 0; _i < arrayProperties.dimensions.size(); ++_i)
    std::cerr << arrayProperties.dimensions[_i]
              << " \n"[_i + 1 == arrayProperties.dimensions.size()];

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
      processFunctionCallStatement(expressionComponent);
      break;
    case CONST_VALUE_NODE:
      processConstNode(expressionComponent);
      break;
    case IDENTIFIER_NODE:
      processIdentifierLValue(expressionComponent);
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

void SemanticAnalysis::processIdentifierLValue(AST *identifierNode) {
  assert(identifierNode->nodeType == IDENTIFIER_NODE);
  IdentifierSemanticValue &idSemnticValue =
      std::get<IdentifierSemanticValue>(identifierNode->semanticValue);
  assert(idSemnticValue.kind == NORMAL_ID || idSemnticValue.kind == ARRAY_ID);

  SymbolTableEntry *idEntry = symbolTable.getSymbol(idSemnticValue.identifierName);
  if (idEntry == nullptr) {
    semanticError(identifierNode->linenumber, "'", idSemnticValue.identifierName,
                  "' was not declared in this scope");
    identifierNode->dataType = ERROR_TYPE;
    return;
  } else if (!(idEntry->symbolKind == VARIABLE_SYMBOL ||
               idEntry->symbolKind == ENUMERATOR_SYMBOL)) {
    semanticError(identifierNode->linenumber, "'", idSemnticValue.identifierName,
                  "' is not a variable nor an enumerator (but a ", idEntry->symbolKind, ")");
    identifierNode->dataType = ERROR_TYPE;
    return;
  }
  if (idEntry->symbolKind == ENUMERATOR_SYMBOL) {
    if (idSemnticValue.kind == ARRAY_ID) {
      semanticError(identifierNode->linenumber,
                    "subscripted value is neither array nor pointer nor vector");
      identifierNode->dataType = ERROR_TYPE;
      return;
    }
    idSemnticValue.isEnumerator = true;
    idSemnticValue.enumeratorValue = std::get<int>(idEntry->attribute);
    identifierNode->dataType = INT_TYPE;
    return;
  }
  assert(idEntry->symbolKind == VARIABLE_SYMBOL);
  const TypeDescriptor &variableTypeDesc = std::get<TypeDescriptor>(idEntry->attribute);
  if (idSemnticValue.kind == NORMAL_ID) {
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

void SemanticAnalysis::processFunctionCallStatement(AST *statementNode) {
  assert(statementNode->nodeType == STMT_NODE);
  assert(std::get<STMTSemanticValue>(statementNode->semanticValue).kind == FUNCTION_CALL_STMT);

  // TODO: process function call
}
