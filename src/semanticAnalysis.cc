#include "semanticAnalysis.hh"

#include <cassert>
#include <cstdlib>
#include <iostream>

SemanticAnalysis::SemanticAnalysis(AST *_prog) : prog(_prog), symbolTable() {}

void SemanticAnalysis::runAnalysis() {
  symbolTable.resetSymbolTable();
  anySemanticError = false;
  processProgramNode(prog);
}

bool SemanticAnalysis::isConstInt(AST *node) {
  if (node->nodeType == CONST_VALUE_NODE &&
      std::get<Const>(node->semanticValue).const_type == INTEGERC)
    return true;
  else if (node->nodeType == EXPR_NODE &&
           std::get<EXPRSemanticValue>(node->semanticValue).isConstEval)
    return true;
  return false;
}

int SemanticAnalysis::getConstValue(AST *node) {
  assert(isConstInt(node));
  if (node->nodeType == CONST_VALUE_NODE)
    return std::get<int>(std::get<Const>(node->semanticValue).value);
  else
    return std::get<EXPRSemanticValue>(node->semanticValue).constEvalValue;
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
        // TODO:
        break;
      case ENUM_DECL:
        // TODO:
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

  AST *typeIDNode = declarationNode->children[0];
  const std::string &typeName =
      std::get<IdentifierSemanticValue>(typeIDNode->semanticValue).identifierName;
  SymbolTableEntry *typeEntry = symbolTable.getSymbol(typeName);
  if (typeEntry == nullptr) {
    semanticError(typeIDNode->linenumber, "'", typeName, "' was not declared in this scope");
    return;
  } else if (typeEntry->symbolKind != TYPE_SYMBOL) {
    // TODO: type ID is not a type (may be a variable or function)
    raiseError("Unhandled semantic error");
  }

  const TypeDescriptor &typeDesc = std::get<TypeDescriptor>(typeEntry->attribute);
  for (size_t idx = 1; idx < declarationNode->children.size(); ++idx) {
    AST *variableIDNode = declarationNode->children[idx];
    IdentifierSemanticValue variableSemanticValue =
        std::get<IdentifierSemanticValue>(variableIDNode->semanticValue);
    if (symbolTable.declaredLocally(variableSemanticValue.identifierName)) {
      semanticError(variableIDNode->linenumber, "redeclaration of '", typeName, " ",
                    variableSemanticValue.identifierName, "'");
      continue;  // skip current variable, process the next one
    }
    /*
      TODO: declare the variable
      The order should be:
      1. Get extra array dimensions if it's ARRAY_ID.
      2. Deduce actual type and full dimensions from the type
         description and the extra array dimensions from step 1.
      3. Parse initial value if exists. Raise error when assigning
         initial value to array variable or when the type is incompatible.
      4. Declare the variable.
    */
    TypeDescriptor variableTypeDesc = typeDesc;
    // Step 1, 2
    if (variableSemanticValue.kind == ARRAY_ID) {
      if (variableTypeDesc.kind == SCALAR_TYPE_DESCRIPTOR) {
        // Turn scalar type into array type
        variableTypeDesc.kind = ARRAY_TYPE_DESCRIPTOR;
        DATA_TYPE origType = std::get<DATA_TYPE>(variableTypeDesc.description);
        variableTypeDesc.description.emplace<ArrayProperties>(ArrayProperties{origType, {}});
      }
      ArrayProperties &arrayProperties = std::get<ArrayProperties>(variableTypeDesc.description);
      bool anyError = false;
      for (AST *dimComponent : variableIDNode->children) {
        processExpressionComponent(dimComponent);
        if (dimComponent->dataType == ERROR_TYPE) {
          anyError = true;
          continue;
        } else if (dimComponent->dataType != INT_TYPE) {
          semanticError(variableIDNode->linenumber, "size of array '",
                        variableSemanticValue.identifierName, "' has non-integer type");
          anyError = true;
          break;
        } else if (!isConstInt(dimComponent)) {
          semanticError(variableIDNode->linenumber, "size of array '",
                        variableSemanticValue.identifierName, "' is not a compile time constant");
          anyError = true;
          break;
        }
        int dimVal = getConstValue(dimComponent);
        if (dimVal < 0) {
          semanticError(variableIDNode->linenumber, "size of array '",
                        variableSemanticValue.identifierName, "' is negative");
          anyError = true;
          break;
        }
        arrayProperties.dimensions.push_back(dimVal);
      }
      if (anyError) continue;  // next variable
      // TODO: remove debug code below
      std::cerr << "New array: ";
      for (size_t _i = 0; _i < arrayProperties.dimensions.size(); ++_i)
        std::cerr << arrayProperties.dimensions[_i]
                  << " \n"[_i + 1 == arrayProperties.dimensions.size()];
    }
    // Step 3
    if (variableSemanticValue.kind == WITH_INIT_ID) {
      // TODO: parse initial value and do type check
      // This should not affect symbol table (right?)
      raiseError("ID with initial value not implemented");
    }
    // Step 4
    if (variableTypeDesc.kind == SCALAR_TYPE_DESCRIPTOR &&
        std::get<DATA_TYPE>(variableTypeDesc.description) == VOID_TYPE) {
      semanticError(variableIDNode->linenumber, "variable or field '",
                    variableSemanticValue.identifierName, "' declared void");
      continue;
    }
    if (variableTypeDesc.kind == ARRAY_TYPE_DESCRIPTOR &&
        std::get<ArrayProperties>(variableTypeDesc.description).elementType == VOID_TYPE) {
      semanticError(variableIDNode->linenumber, "declaration of ‘",
                    variableSemanticValue.identifierName, "’ as array of voids");
      continue;
    }
    symbolTable.addVariableSymbol(variableSemanticValue.identifierName,
                                  std::move(variableTypeDesc));
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

void SemanticAnalysis::tryConstEval(AST *exprNode, AST *lOperand, AST *rOperand,
                                    BINARY_OPERATOR op) {
  assert(exprNode->nodeType == EXPR_NODE);
  int lVal = getConstValue(lOperand);
  int rVal = getConstValue(rOperand);
  int nVal;
  if (op == BINARY_OP_ADD)
    nVal = lVal + rVal;
  else if (op == BINARY_OP_SUB)
    nVal = lVal - rVal;
  else if (op == BINARY_OP_MUL)
    nVal = lVal * rVal;
  else if (op == BINARY_OP_DIV) {
    if (rVal == 0) return;
    nVal = lVal / rVal;
  } else
    raiseError("Unknown binary operator to do constant evaluation");
  EXPRSemanticValue &exprSemanticValue = std::get<EXPRSemanticValue>(exprNode->semanticValue);
  exprSemanticValue.isConstEval = true;
  exprSemanticValue.constEvalValue = nVal;
}

void SemanticAnalysis::processExpressionComponent(AST *expressionComponent) {
  // TODO:
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
    default:
      raiseError("Unknown expression component node type");
  }
  assert(expressionComponent->dataType != NONE_TYPE);
}

void SemanticAnalysis::processExpressionNode(AST *expressionNode) {
  assert(expressionNode->nodeType == EXPR_NODE);
  EXPRSemanticValue &exprSemanticValue = std::get<EXPRSemanticValue>(expressionNode->semanticValue);

  bool anyError = false;
  for (AST *operandNode : expressionNode->children) {
    processExpressionComponent(operandNode);
    if (operandNode->dataType == ERROR_TYPE)
      anyError = true;
    else if (operandNode->dataType == VOID_TYPE) {
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
    if (!(operand->dataType == INT_TYPE || operand->dataType == FLOAT_TYPE)) {
      const std::string &unaryOpStr = UNARY_OPERATOR_str[(size_t)op];
      const std::string &typeStr = DATA_TYPE_str[(size_t)operand->dataType];
      semanticError(expressionNode->linenumber, "invalid operand to unary ", unaryOpStr, " (have '",
                    typeStr, "')");
      expressionNode->dataType = ERROR_TYPE;
      return;
    }
    switch (op) {
      case UNARY_OP_LOGICAL_NEGATION:
        expressionNode->dataType = INT_TYPE;
        break;
      case UNARY_OP_POSITIVE:
      case UNARY_OP_NEGATIVE:
        expressionNode->dataType = operand->dataType;
        break;
    }
  } else {  // BINARY_OPERATION
    assert(expressionNode->children.size() == (size_t)2);
    BINARY_OPERATOR op = std::get<BINARY_OPERATOR>(exprSemanticValue.op);
    AST *lOperand = expressionNode->children[0];
    AST *rOperand = expressionNode->children[1];
    if (!(lOperand->dataType == INT_TYPE || lOperand->dataType == FLOAT_TYPE) ||
        !(rOperand->dataType == INT_TYPE || rOperand->dataType == FLOAT_TYPE)) {
      const std::string &binaryOpStr = BINARY_OPERATOR_str[(size_t)op];
      const std::string &lTypeStr = DATA_TYPE_str[(size_t)lOperand->dataType];
      const std::string &rTypeStr = DATA_TYPE_str[(size_t)rOperand->dataType];
      semanticError(expressionNode->linenumber, "invalid operand to binary ", binaryOpStr,
                    " (have '", lTypeStr, "' and '", rTypeStr, "')");
      expressionNode->dataType = ERROR_TYPE;
      return;
    }
    switch (op) {
      case BINARY_OP_ADD:
      case BINARY_OP_SUB:
      case BINARY_OP_MUL:
      case BINARY_OP_DIV:
        expressionNode->dataType = getLargerType(lOperand->dataType, rOperand->dataType);
        if (isConstInt(lOperand) && isConstInt(rOperand))
          tryConstEval(expressionNode, lOperand, rOperand, op);
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

void SemanticAnalysis::processFunctionCallStatement(AST *statementNode) {
  assert(statementNode->nodeType == STMT_NODE);
  assert(std::get<STMTSemanticValue>(statementNode->semanticValue).kind == FUNCTION_CALL_STMT);

  // TODO: process function call
}
