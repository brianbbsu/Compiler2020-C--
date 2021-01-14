#include <fstream>
#include <cassert>
#include <cstring>
#include "codeGen.hh"
#include "print.hh" // for output operator of Const


CodeGeneration::CodeGeneration (AST *_prog, const std::string &_filename)
: filename{_filename}, prog{_prog}, ofs{_filename, std::ofstream::out}, hasSetRoundingMode{false}, currentSection{ASSEMBLY_DATA_SECTION} {
}


CodeGeneration::~CodeGeneration () {
  ofs.close();
}


void CodeGeneration::run () {
  visitProgramNode(prog);
}


inline TypeDescriptor CodeGeneration::combineTypeAndDecl (const TypeDescriptor &declTypeDesc, AST *varIDNode) {
  auto &varIDSemanticValue {std::get<IdentifierSemanticValue>(varIDNode->semanticValue)};
  if (varIDSemanticValue.kind == NORMAL_ID || varIDSemanticValue.kind == WITH_INIT_ID) {
    return declTypeDesc;
  }

  // handle array type
  DATA_TYPE elementType {(declTypeDesc.type == ARR_TYPE) ? declTypeDesc.arrayProperties.elementType : declTypeDesc.type};
  std::vector<int> dimension;

  for (const auto &dimComponentNode : varIDNode->children) {
    if (dimComponentNode->nodeType == NUL_NODE) {
      dimension.push_back(EMPTY_DIM);
      continue;
    }

    int dimVal {std::get<int>(getConstValue(dimComponentNode).value)};
    dimension.push_back(dimVal);
  }

  return TypeDescriptor{ARR_TYPE, ArrayProperties{elementType, std::move(dimension)}};
}


inline size_t CodeGeneration::getTypeSize (const TypeDescriptor &typeDesc) {
  switch (typeDesc.type) {
    case VOID_TYPE: // void type cannot be on LHS (by CFG) or on RHS (by semantic check), so the type size here is just to handle function call with void type
    case CONST_STRING_TYPE: // string type can only appear in function call of "write", so the type size does not matter at all, too
      return 0;

    case INT_TYPE:
    case FLOAT_TYPE:
      return 4;

    case ARR_TYPE: {
      size_t retval = getTypeSize(typeDesc.arrayProperties.elementType);
      for (const auto &dim : typeDesc.arrayProperties.dimensions) {
        retval *= dim;
      }
      return retval;
    }

    default:
      assert(false);
  }
}


inline LabelInAssembly CodeGeneration::makeGlobalVarLabel (const std::string &varName) {
  return "g_" + varName;
}


inline LabelInAssembly CodeGeneration::makeFuncStartLabel (const std::string &funcName) {
  if (funcName == "main") return "_start_MAIN";
  return "f_start_" + funcName;
}


inline LabelInAssembly CodeGeneration::makeFuncPrologueStoreRegistersLabelBefore (const std::string &funcName) {
  return "prologue_before_" + funcName;
}


inline LabelInAssembly CodeGeneration::makeFuncPrologueStoreRegistersLabelAfter (const std::string &funcName) {
  return "prologue_after_" + funcName;
}


inline LabelInAssembly CodeGeneration::makeFuncEndLabel (const std::string &funcName) {
  return "f_end_" + funcName;
}


inline LabelInAssembly CodeGeneration::makeFrameSizeLabel (const LabelInAssembly &funcLabel) {
  return "frame_size_" + funcLabel;
}


inline LabelInAssembly CodeGeneration::makeConstStringLabel () {
  static int counter {0};
  return "s_" + std::to_string(counter++);
}


inline LabelInAssembly CodeGeneration::makeBranchLabel () {
  static int counter {0};
  return "br_" + std::to_string(counter++);
}


inline Const CodeGeneration::getConstValue (AST *node) {
  switch (node->nodeType) {
    case CONST_VALUE_NODE:
      return std::get<Const>(node->semanticValue);
    case IDENTIFIER_NODE:
      return Const{std::get<IdentifierSemanticValue>(node->semanticValue).enumeratorValue};
    case EXPR_NODE:
      return std::get<EXPRSemanticValue>(node->semanticValue).constEvalValue;
    default:
      assert(false);
  }
}


static_assert(sizeof(float) == sizeof(int32_t), "cannot memcpy float to int32_t");
inline int32_t CodeGeneration::float2intMemoryRepresent (float num) {
  int32_t retval;
  memcpy(&retval, &num, sizeof(float));
  return retval;
}


inline bool CodeGeneration::isFloatRegister (const Register &reg) {
  if (reg == REG_FP) return false;
  return reg[0] == 'f';
}


void CodeGeneration::visitProgramNode (AST *programNode) {
  // TODO: generate some information like file name

  for (AST *child : programNode->children) {
    switch (child->nodeType) {
      case VARIABLE_DECL_LIST_NODE:
        visitVariableDeclarationList(child);
        break;
      case DECLARATION_NODE:
        visitFunctionDefinition(child);
        break;
      default:
        assert(false);
        break;
    }
  }
}


void CodeGeneration::visitVariableDeclarationList (AST *varDeclListNode) {
  for (AST *child : varDeclListNode->children) {
    DECL_KIND kind = std::get<DECLSemanticValue>(child->semanticValue).kind;
    switch (kind) {
      case VARIABLE_DECL:
        visitVariableDeclaration(child);
        break;
      case TYPE_DECL:
        visitTypeDeclaration(child);
        break;
      case ENUM_DECL:
        visitEnumNode(child->children[0]);
        break;
      case FUNCTION_DECL:
        visitFunctionDeclaration(child);
        break;
      default:
        assert(false);
        break;
    }
  }
}


void CodeGeneration::visitTypeDeclaration (AST *typeDeclNode) {
  const auto &typeSpecifierNode {typeDeclNode->children[0]};
  visitTypeSpecifier(typeSpecifierNode);

  const TypeDescriptor baseTypeDesc {typeSpecifierNode->dataType};

  for (size_t idx {1}; idx < typeDeclNode->children.size(); ++idx) {
    AST *newTypeIDNode {typeDeclNode->children[idx]};
    const TypeDescriptor newTypeTypeDesc {combineTypeAndDecl(baseTypeDesc, newTypeIDNode)};
    const std::string newTypeName {std::get<IdentifierSemanticValue>(newTypeIDNode->semanticValue).identifierName};
    if (symtab.declaredLocally(newTypeName)) {
      continue; // allow redeclaration of the same type
    }
    symtab.addTypeSymbol(std::move(newTypeName), std::move(newTypeTypeDesc));
  }
}


void CodeGeneration::visitTypeSpecifier (AST *typeIDNode) {
  switch (typeIDNode->nodeType) {
    case IDENTIFIER_NODE:
      return;
    case ENUM_NODE:
      visitEnumNode(typeIDNode);
      return;
    default:
      assert(false);
  }
}


void CodeGeneration::visitEnumNode ([[maybe_unused]] AST *enumNode) {
  // all information needed should be generated in semantic check
}


void CodeGeneration::visitVariableDeclaration (AST *varDeclNode) {
  visitTypeSpecifier(varDeclNode->children[0]);
  const TypeDescriptor &declTypeDesc {varDeclNode->children[0]->dataType};

  for (size_t idx = 1; idx < varDeclNode->children.size(); ++idx) {
    AST *varIDNode {varDeclNode->children[idx]};
    const TypeDescriptor varTypeDesc {combineTypeAndDecl(declTypeDesc, varIDNode)};
    size_t varSize {getTypeSize(varTypeDesc)};
    MemoryLocation varPlace;

    const IdentifierSemanticValue &varSemanticValue {std::get<IdentifierSemanticValue>(varIDNode->semanticValue)};
    if (symtab.isGlobalScope()) {
      LabelInAssembly label {makeGlobalVarLabel(varSemanticValue.identifierName)};
      varPlace.value = label;

      if (varTypeDesc.type == ARR_TYPE) {
        genInitGlobalVarArray(label, varSize);
      }
      else { // global variable (not array)
        Const RHS {}; // initialized to (type=int, value=0)
        if (varSemanticValue.kind == WITH_INIT_ID) {
          RHS = getConstValue(varIDNode->children[0]);
        }

        genInitGlobalVarScalar(label, RHS);
      }
    }
    else { // local variable declaration
      StackMemoryOffset stackOffset {stackMemManager.getVariableMemory(varSize)};
      varPlace.value = stackOffset;

      if (varSemanticValue.kind == WITH_INIT_ID) {
        const auto &initValueNode {varIDNode->children[0]};
        visitExpressionComponent(initValueNode);
        genAssignExpr(stackOffset, initValueNode->place, varTypeDesc.type, initValueNode->dataType.type);
        // TODO: release the stack memory allocated to initValueNode
      }
    }

    symtab.addVariableSymbol(varSemanticValue.identifierName, varTypeDesc, varPlace);
  }
}


void CodeGeneration::visitExpressionComponent (AST *expressionComponent) {
  switch (expressionComponent->nodeType) {
    case EXPR_NODE:
      visitExpression(expressionComponent);
      break;
    case STMT_NODE:
      switch (std::get<STMTSemanticValue>(expressionComponent->semanticValue).kind) {
        case FUNCTION_CALL_STMT:
          visitFunctionCallStatement(expressionComponent);
          break;
        case ASSIGN_STMT:
          visitAssignmentStatement(expressionComponent);
          break;
        default:
          assert(false);
          break;
      }
      break;
    case CONST_VALUE_NODE:
      visitConstNode(expressionComponent);
      break;
    case IDENTIFIER_NODE:
      visitVarRefRValue(expressionComponent);
      break;
    default:
      assert(false);
      break;
  }
}


void CodeGeneration::visitExpression (AST *exprNode) {
  size_t dataSize {getTypeSize(exprNode->dataType)};
  exprNode->place = stackMemManager.getVariableMemory(dataSize);

  const EXPRSemanticValue &exprSemanticValue = std::get<EXPRSemanticValue>(exprNode->semanticValue);
  if (exprSemanticValue.isConstEval) {
    genAssignConst(exprNode->place, exprSemanticValue.constEvalValue, exprNode->dataType.type);
  }
  else {
    switch (exprSemanticValue.kind) {
      case UNARY_OPERATION: {
        UNARY_OPERATOR op {std::get<UNARY_OPERATOR>(exprSemanticValue.op)};
        AST *operand {exprNode->children[0]};
        visitExpressionComponent(operand);
        switch (op) {
          case UNARY_OP_LOGICAL_NEGATION:
            genLogicalNegation(exprNode->place, operand->place, operand->dataType.type);
            break;
          case UNARY_OP_POSITIVE:
            break;
          case UNARY_OP_NEGATIVE:
            genUnaryNegative(exprNode->place, operand->place, operand->dataType.type);
            break;
        }
        break;
      } // end of case UNARY_OPERATION

      case BINARY_OPERATION: {
        BINARY_OPERATOR op {std::get<BINARY_OPERATOR>(exprSemanticValue.op)};
        AST *lOperand {exprNode->children[0]};
        AST *rOperand {exprNode->children[1]};
        switch (op) {
          case BINARY_OP_ADD:
          case BINARY_OP_SUB:
          case BINARY_OP_MUL:
          case BINARY_OP_DIV:
            visitExpressionComponent(lOperand);
            visitExpressionComponent(rOperand);
            genArithmeticOperation(op, exprNode->place, lOperand->place, rOperand->place, exprNode->dataType.type, lOperand->dataType.type, rOperand->dataType.type);
            break;
          case BINARY_OP_EQ:
          case BINARY_OP_GE:
          case BINARY_OP_LE:
          case BINARY_OP_NE:
          case BINARY_OP_GT:
          case BINARY_OP_LT:
            visitExpressionComponent(lOperand);
            visitExpressionComponent(rOperand);
            genLogicalOperation(op, exprNode->place, lOperand->place, rOperand->place, exprNode->dataType.type, lOperand->dataType.type, rOperand->dataType.type);
            break;
          case BINARY_OP_AND:
          case BINARY_OP_OR:
            LabelInAssembly shortCircuitLabel {makeBranchLabel()};
            visitExpressionComponent(lOperand);
            genShortCircuitEvaluation(lOperand->place, lOperand->dataType.type, op, exprNode->place, shortCircuitLabel);
            visitExpressionComponent(rOperand);
            genLogicalOperation(op, exprNode->place, lOperand->place, rOperand->place, exprNode->dataType.type, lOperand->dataType.type, rOperand->dataType.type);
            ofs << shortCircuitLabel << ":" << std::endl;
            break;
        }
        break;
      } // end of case BINARY_OPERATION
    }

    // TODO: release stack memory allocated to child nodes
  }
}


void CodeGeneration::visitFunctionCallStatement (AST *stmtNode) {
  size_t dataSize {getTypeSize(stmtNode->dataType)};
  stmtNode->place = stackMemManager.getVariableMemory(dataSize);

  AST *funcNameIDNode {stmtNode->children[0]};
  AST *paramListNode {stmtNode->children[1]};

  const std::string &funcName {std::get<IdentifierSemanticValue>(funcNameIDNode->semanticValue).identifierName};
  SymbolTableEntry *funcEntry {symtab.getSymbol(funcName)};
  LabelInAssembly funcLabel;
  if (funcName == "write") {
    // function "write" is so evil that many bugs stem from here
    switch (paramListNode->children[0]->dataType.type) {
      case INT_TYPE:
        funcLabel = "_write_int";
        break;
      case FLOAT_TYPE:
        funcLabel = "_write_float";
        break;
      case CONST_STRING_TYPE:
        funcLabel = "_write_str";
        break;
      default:
        assert(false);
    }
  }
  else {
    funcLabel = std::get<LabelInAssembly>(funcEntry->place.value);
  }

  // prepare all parameters
  for (const auto &paramNode : paramListNode->children) {
    visitExpressionComponent(paramNode);
  }

  const FunctionSignature &funcSig {std::get<FunctionSignature>(funcEntry->attribute)};

  genCallerSaveRegisters();
  genPassParametersBeforeFunctionCall((funcName == "write"), paramListNode, funcSig.parameters, funcSig.parameterMemoryConsumption);
  genCallFunction(funcLabel, funcSig.parameterMemoryConsumption);
  if (!(stmtNode->dataType.type == VOID_TYPE))
    genSaveReturnValue(stmtNode->place, stmtNode->dataType.type);
  genCallerRestoreRegisters();
}


void CodeGeneration::visitAssignmentStatement (AST *stmtNode) {
  AST *LHSIDNode {stmtNode->children[0]};
  AST *RHSExprNode {stmtNode->children[1]};

  visitVarRefLValue(LHSIDNode);
  visitExpressionComponent(RHSExprNode);

  size_t dataSize {getTypeSize(stmtNode->dataType)};
  stmtNode->place = stackMemManager.getVariableMemory(dataSize);
  genAssignExpr(LHSIDNode->place, RHSExprNode->place, LHSIDNode->dataType.type, RHSExprNode->dataType.type);
  genAssignExpr(stmtNode->place, LHSIDNode->place, stmtNode->dataType.type, LHSIDNode->dataType.type);
  // TODO: release stack memory allocated to child nodes;
}


void CodeGeneration::visitConstNode (AST *constNode) {
  if (constNode->dataType.type == CONST_STRING_TYPE) {
    LabelInAssembly strLabel {makeConstStringLabel()};
    constNode->place = strLabel;
    const Const &constValue {std::get<Const>(constNode->semanticValue)};
    genConstString(strLabel, std::get<std::string>(constValue.value));
    return;
  }

  size_t dataSize {getTypeSize(constNode->dataType)};
  constNode->place = stackMemManager.getVariableMemory(dataSize);
  genAssignConst(constNode->place, getConstValue(constNode), constNode->dataType.type);
}


void CodeGeneration::visitVarRefRValue (AST *idNode) {
  visitVarRef(idNode);
}


void CodeGeneration::visitVarRefLValue (AST *idNode) {
  visitVarRef(idNode);
}


void CodeGeneration::visitVarRef (AST *idNode) {
  const auto &idSemanticValue {std::get<IdentifierSemanticValue>(idNode->semanticValue)};
  if (idSemanticValue.isEnumerator) {
    visitConstNode(idNode);
    return;
  }

  SymbolTableEntry *idEntry {symtab.getSymbol(idSemanticValue.identifierName)};

  if (idSemanticValue.kind == NORMAL_ID) {
    idNode->place = idEntry->place;
    return;
  }

  // array dereference
  const ArrayProperties &arrProp {std::get<TypeDescriptor>(idEntry->attribute).arrayProperties};
  StackMemoryOffset valueOffset {stackMemManager.getVariableMemory(SIZEOF_REGISTER)};
  _genSWorFSW(REG_ZERO, valueOffset, REG_FP);
  for (size_t idx {0}; idx < idNode->children.size(); ++idx) {
    // multiplied by the size of current dimension
    {
      AllocatedRegister currValueReg {regManager.getTempIntRegister("current_array_offset")};
      AllocatedRegister dimSizeReg {regManager.getTempIntRegister("next_dimension_size")};
      _genLWorFLW(currValueReg, valueOffset, REG_FP);
      _genLI(dimSizeReg, arrProp.dimensions[idx]);
      _genMUL(currValueReg, currValueReg, dimSizeReg);
      _genSWorFSW(currValueReg, valueOffset, REG_FP);
    }
    // added by the value of current index
    {
      const auto &dimComponentNode {idNode->children[idx]};
      visitExpressionComponent(dimComponentNode);
      AllocatedRegister currValueReg {regManager.getTempIntRegister("current_array_offset")};
      AllocatedRegister dimComponReg {regManager.getTempIntRegister("curr_dimension_component")};
      _genLWorFLW(currValueReg, valueOffset, REG_FP);
      _genLoadFromMemoryLocation(dimComponReg, dimComponentNode->place);
      _genADD(currValueReg, currValueReg, dimComponReg);
      _genSWorFSW(currValueReg, valueOffset, REG_FP);
    }
  }
  // multiplied by the size of array element type
  AllocatedRegister offsetReg {regManager.getTempIntRegister("array_offset")};
  {
    AllocatedRegister sizeReg {regManager.getTempIntRegister("sizeof_array_element")};
    _genLWorFLW(offsetReg, valueOffset, REG_FP);
    _genLI(sizeReg, getTypeSize(arrProp.elementType));
    _genMUL(offsetReg, offsetReg, sizeReg);
  }
  // added by base address
  {
    AllocatedRegister baseAddrReg {regManager.getTempIntRegister("base_address")};
    if (std::holds_alternative<LabelInAssembly>(idEntry->place.value))
      _genLA(baseAddrReg, std::get<LabelInAssembly>(idEntry->place.value));
    else if (idEntry->place.isAbsoluteMemoryAddress) {
      _genLD(baseAddrReg, std::get<StackMemoryOffset>(idEntry->place.value), REG_FP);
    } else {
      _genMV(baseAddrReg, REG_FP);
      _genADDI(baseAddrReg, baseAddrReg, std::get<StackMemoryOffset>(idEntry->place.value));
    }
    _genADD(offsetReg, offsetReg, baseAddrReg);
    _genSD(offsetReg, valueOffset, REG_FP);
  }
  idNode->place.value = valueOffset;
  idNode->place.isAbsoluteMemoryAddress = true;
}


void CodeGeneration::visitFunctionDeclaration ([[maybe_unused]] AST *declNode) {
  // Although the function definition will not conflict with declaration (by semantic check),
  // the reason why we cannot ignore this AST node is that some information is needed to be generated.
  // For example, function call statement may appear before the function definition.
  // Another example is that `struct` in the return type has to be declared, although it is not a required feature in this project.
  // We have to do something on symbol table as in semantic check.
  const auto &returnTypeNode {declNode->children[0]};
  const auto &funcNameIDNode {declNode->children[1]};
  const auto &paramListNode {declNode->children[2]};

  const std::string &funcName {std::get<IdentifierSemanticValue>(funcNameIDNode->semanticValue).identifierName};
  LabelInAssembly funcLabel {makeFuncStartLabel(funcName)};

  visitTypeSpecifier(returnTypeNode);
  symtab.openScope();
  const auto &[params, parameterTotalSize] {visitParameterDeclarationList(paramListNode)};
  symtab.closeScope();

  symtab.addFunctionSymbol(
    funcName,
    FunctionSignature{returnTypeNode->dataType.type, std::move(params), false, parameterTotalSize},
    funcLabel
  );
}


void CodeGeneration::visitFunctionDefinition (AST *defiNode) {
  AST *returnTypeNode {defiNode->children[0]};
  AST *funcNameIDNode {defiNode->children[1]};
  AST *paramListNode {defiNode->children[2]};
  AST *bodyBlockNode {defiNode->children[3]};

  visitTypeSpecifier(returnTypeNode);
  const TypeDescriptor &returnTypeDesc {returnTypeNode->dataType};
  const std::string &funcName {std::get<IdentifierSemanticValue>(funcNameIDNode->semanticValue).identifierName};
  LabelInAssembly funcLabel {makeFuncStartLabel(funcName)};

  symtab.openScope();
  const auto &[params, paramTotalSize] {visitParameterDeclarationList(paramListNode)};
  symtab.stashScope();
  SymbolTableEntry *funcEntry;
  if (symtab.declaredLocally(funcName)) {
    funcEntry = symtab.getSymbol(funcName);
    std::get<FunctionSignature>(funcEntry->attribute).hasDefinition = true;
  }
  else {
    funcEntry = symtab.addFunctionSymbol(
      funcName,
      FunctionSignature{returnTypeDesc.type, std::move(params), true, paramTotalSize},
      funcLabel
    );
  }
  symtab.popStash();
  symtab.enterFunction(funcEntry);
  stackMemManager.enterProcedure();
  genFunctionPrologue(funcLabel);
  visitBlock(bodyBlockNode);
  genFunctionEpilogue(funcLabel);
  stackMemManager.leaveProcedure();
  symtab.leaveFunction();
  symtab.closeScope();
}


std::tuple<std::vector<FunctionParameter>, size_t> CodeGeneration::visitParameterDeclarationList (AST *paramListNode) {
  stackMemManager.enterParameterDeclaration();
  std::vector<FunctionParameter> paramList;
  for (const auto &childNode : paramListNode->children) {
    AST *typeNode {childNode->children[0]};
    AST *IDNode {childNode->children[1]};

    visitTypeSpecifier(typeNode);
    TypeDescriptor paramTypeDesc {combineTypeAndDecl(typeNode->dataType, IDNode)};
    const std::string &paramName {std::get<IdentifierSemanticValue>(IDNode->semanticValue).identifierName};
    size_t paramSize;
    MemoryLocation symtabPlace; // specially handle isAbsoluteMemoryAddress of array type
    StackMemoryOffset paramPlace;
    if (paramTypeDesc.type == ARR_TYPE) {
      paramTypeDesc.arrayProperties.dimensions[0] = EMPTY_DIM;
      paramSize = SIZEOF_REGISTER;
      symtabPlace.isAbsoluteMemoryAddress = true;
    }
    else {
      paramSize = getTypeSize(paramTypeDesc);
      symtabPlace.isAbsoluteMemoryAddress = false;
    }
    paramPlace = stackMemManager.getParameterMemory(paramSize);
    symtabPlace.value = paramPlace;

    if (paramName != "")
      symtab.addVariableSymbol(paramName, paramTypeDesc, symtabPlace);

    paramList.push_back(FunctionParameter{paramTypeDesc, paramPlace});
  }
  size_t paramTotalSize {stackMemManager.getParameterMemoryConsumption()};
  stackMemManager.leaveParameterDeclaration();
  return {paramList, paramTotalSize};
}


void CodeGeneration::visitBlock (AST *blockNode) {
  for (const auto &childNode : blockNode->children) {
    switch (childNode->nodeType) {
      case VARIABLE_DECL_LIST_NODE:
        visitVariableDeclarationList(childNode);
        break;
      case STMT_LIST_NODE:
        for (const auto &stmtNode : childNode->children)
          visitStatement(stmtNode);
        break;
      default:
        assert(false);
    }
  }
}


void CodeGeneration::visitStatement (AST *stmtNode) {
  if (stmtNode->nodeType == NUL_NODE) return;
  if (stmtNode->nodeType == BLOCK_NODE) {
    symtab.openScope();
    visitBlock(stmtNode);
    symtab.closeScope();
    return;
  }

  const STMTSemanticValue &stmtSemanticValue {std::get<STMTSemanticValue>(stmtNode->semanticValue)};
  switch (stmtSemanticValue.kind) {
    case IF_STMT:
      visitIfStatement(stmtNode);
      break;
    case FOR_STMT:
      visitForStatement(stmtNode);
      break;
    case WHILE_STMT:
      visitWhileStatement(stmtNode);
      break;
    case ASSIGN_STMT:
      visitAssignmentStatement(stmtNode);
      break;
    case FUNCTION_CALL_STMT:
      visitFunctionCallStatement(stmtNode);
      break;
    case RETURN_STMT:
      visitReturnStatement(stmtNode);
      break;
    default:
      assert(false);
  }
}


void CodeGeneration::visitIfStatement (AST *stmtNode) {
  const auto &testNode {stmtNode->children[0]};
  const auto &trueStmtNode {stmtNode->children[1]};
  const auto &falseStmtNode {stmtNode->children[2]};

  visitExpressionComponent(testNode);
  LabelInAssembly branchElseLabel {makeBranchLabel()};
  LabelInAssembly branchFinalLabel {makeBranchLabel()};
  genBranchTest(testNode, branchElseLabel);
  visitStatement(trueStmtNode);
  _genJ(branchFinalLabel);
  ofs << branchElseLabel << ":" << std::endl;
  visitStatement(falseStmtNode);
  ofs << branchFinalLabel << ":" << std::endl;
}


void CodeGeneration::visitForStatement (AST *stmtNode) {
  const auto &initListNode {stmtNode->children[0]};
  const auto &condListNode {stmtNode->children[1]};
  const auto &iterListNode {stmtNode->children[2]};
  const auto &bodyStmtNode {stmtNode->children[3]};

  if (initListNode->nodeType != NUL_NODE) {
    for (const auto &childNode : initListNode->children)
      visitExpressionComponent(childNode);
  }

  LabelInAssembly beforeTestLabel {makeBranchLabel()};
  LabelInAssembly finalLabel {makeBranchLabel()};

  ofs << beforeTestLabel << ":" << std::endl;
  if (condListNode->nodeType != NUL_NODE) {
    for (const auto &childNode : condListNode->children)
      visitExpressionComponent(childNode);
    genBranchTest(condListNode->children[condListNode->children.size() - 1], finalLabel);
    visitStatement(bodyStmtNode);
  }
  if (iterListNode->nodeType != NUL_NODE) {
    for (const auto &childNode : iterListNode->children)
      visitExpressionComponent(childNode);
  }
  _genJ(beforeTestLabel);
  ofs << finalLabel << ":" << std::endl;
}


void CodeGeneration::visitWhileStatement (AST *stmtNode) {
  const auto &testNode {stmtNode->children[0]};
  const auto &bodyStmtNode {stmtNode->children[1]};

  LabelInAssembly beforeTestLabel {makeBranchLabel()};
  LabelInAssembly finalLabel {makeBranchLabel()};

  ofs << beforeTestLabel << ":" << std::endl;
  visitExpressionComponent(testNode);
  genBranchTest(testNode, finalLabel);
  visitStatement(bodyStmtNode);
  _genJ(beforeTestLabel);
  ofs << finalLabel << ":" << std::endl;
}


void CodeGeneration::visitReturnStatement (AST *retStmtNode) {
  AST *exprNode {retStmtNode->children[0]};
  if (exprNode->nodeType != NUL_NODE) {
    visitExpressionComponent(exprNode);
    genReturnValue(exprNode);
  }

  const std::string &currFuncName {symtab.getCurrentFunction()->name};
  _genJ(makeFuncEndLabel(currFuncName));
}


void CodeGeneration::genCallerSaveRegisters () {
  std::set<Register> saveRegisters {regManager.getCallerSaveRegistersOfCurrentProcedure()};
  _genADDI(REG_SP, REG_SP, -8 * saveRegisters.size());
  size_t idx {0};
  for (const auto &reg : saveRegisters) {
    _genSWorFSW(reg, -8 * idx, REG_SP);
    ++idx;
  }
}


void CodeGeneration::genCallerRestoreRegisters () {
  std::set<Register> saveRegisters {regManager.getCallerSaveRegistersOfCurrentProcedure()};
  size_t idx {0};
  for (const auto &reg : saveRegisters) {
    _genLWorFLW(reg, -8 * idx, REG_SP);
    ++idx;
  }
  _genADDI(REG_SP, REG_SP, 8 * saveRegisters.size());
}


void CodeGeneration::genPassParametersBeforeFunctionCall (bool isWriteFunction, const AST *paramListNode, const std::vector<FunctionParameter> &paramDeclList, size_t paramTotalSize) {
  if (paramListNode->children.size() == 0)
    return;

  for (size_t idx {0}; idx < paramListNode->children.size(); ++idx) {
    const auto &childNode {paramListNode->children[idx]};
    const auto &paramInformation {paramDeclList[idx]};
    switch (childNode->dataType.type) {
      case INT_TYPE: {
        if (isWriteFunction) {
          _genLoadFromMemoryLocation(REG_A0, childNode->place);
          break;
        }
        genAssignParameters(paramInformation, paramTotalSize, childNode->place, childNode->dataType.type);
        break;
      }
      case FLOAT_TYPE: {
        if (isWriteFunction) {
          _genLoadFromMemoryLocation(REG_FA0, childNode->place);
          break;
        }
        genAssignParameters(paramInformation, paramTotalSize, childNode->place, childNode->dataType.type);
        break;
      }
      case CONST_STRING_TYPE: {
        assert(isWriteFunction);
        _genLA(REG_A0, std::get<LabelInAssembly>(childNode->place.value));
        break;
      }
      case ARR_TYPE: {
        // if an array is passed as a parameter, then we pass the absolute memory address of the array
        assert(!isWriteFunction);
        genAssignParameters(paramInformation, paramTotalSize, childNode->place, childNode->dataType.type);
        break;
      }
      default:
        assert(false);
    }
  }
}


void CodeGeneration::genFunctionPrologue (const LabelInAssembly &funcLabel) {
  if (currentSection != ASSEMBLY_TEXT_SECTION) {
    ofs << "  " << ASSEMBLY_TEXT_SECTION << std::endl;
    currentSection = ASSEMBLY_TEXT_SECTION;
  }

  // step 1: move fp and sp
  //  * sp+0 : return address
  //  * sp+8 : old frame pointer
  ofs << funcLabel << ":" << std::endl;
  _genSD(REG_RA, 0, REG_SP);
  _genSD(REG_FP, -8, REG_SP);
  _genADDI(REG_FP, REG_SP, -8);
  _genADDI(REG_SP, REG_SP, -8); // -8 instead of -16 since sp should point at the last saved register after subtracted by frameSize, not point at **empty**
  _genLWorFLW(REG_T1, makeFrameSizeLabel(funcLabel));
  _genSUB(REG_SP, REG_SP, REG_T1);

  // step 2: store registers
  // implemented in genFunctionEpilogue()
  _genJ(makeFuncPrologueStoreRegistersLabelBefore(funcLabel));
  ofs << makeFuncPrologueStoreRegistersLabelAfter(funcLabel) << ":" << std::endl;
}


// the parameter $frameSize here does not contain the saved registers
void CodeGeneration::genFunctionEpilogue (const LabelInAssembly &funcLabel) {
  if (currentSection != ASSEMBLY_TEXT_SECTION) {
    ofs << "  " << ASSEMBLY_TEXT_SECTION << std::endl;
    currentSection = ASSEMBLY_TEXT_SECTION;
  }

  const std::string &currFuncName {symtab.getCurrentFunction()->name};
  ofs << makeFuncEndLabel(currFuncName) << ":" << std::endl;

  // step 1: restore registers
  std::set<Register> savedRegisters {regManager.getCalleeSaveRegistersOfCurrentProcedure()};
  size_t idx {0};
  for (const Register &reg : savedRegisters) {
    _genLWorFLW(reg, 8 * idx, REG_SP);
    ++idx;
  }

  // step 2: move fp and sp
  _genADDI(REG_SP, REG_FP, 8);
  _genLD(REG_FP, 0, REG_FP);
  _genLD(REG_RA, 0, REG_SP);
  _genRET();

  // step 3: generate frame size
  ofs << "  " << ASSEMBLY_DATA_SECTION << std::endl
      << makeFrameSizeLabel(funcLabel) << ": .word " << (stackMemManager.getProcedureMemoryConsumption() + 8 * savedRegisters.size()) << std::endl
      << "  " << ASSEMBLY_TEXT_SECTION << std::endl;

  // step 4: generate "store registers" of function prologue
  ofs << makeFuncPrologueStoreRegistersLabelBefore(funcLabel) << ":" << std::endl;
  idx = 0;
  for (const Register &reg : savedRegisters) {
    _genSWorFSW(reg, 8 * idx, REG_SP);
    ++idx;
  }
  _genJ(makeFuncPrologueStoreRegistersLabelAfter(funcLabel));
}


void CodeGeneration::genCallFunction (const LabelInAssembly &funcLabel, size_t paramTotalSize) {
  _genADDI(REG_SP, REG_SP, -(SIZEOF_REGISTER + paramTotalSize));
  _genCALL(funcLabel);
  _genADDI(REG_SP, REG_SP, (SIZEOF_REGISTER + paramTotalSize));
}


void CodeGeneration::genSaveReturnValue (const MemoryLocation &place, const DATA_TYPE &retType) {
  switch (retType) {
    case INT_TYPE:
      _genStoreToMemoryLocation(REG_A0, place);
      break;
    case FLOAT_TYPE:
      _genStoreToMemoryLocation(REG_FA0, place);
      break;
    default:
      assert(false);
  }
}


void CodeGeneration::genInitGlobalVarArray (const LabelInAssembly &label, size_t size) {
  ofs << "  " << ASSEMBLY_DATA_SECTION << std::endl
      << label << ":" << std::endl
      << "  .zero " << size << std::endl;

  if (currentSection != ASSEMBLY_DATA_SECTION)
    ofs << "  " << currentSection << std::endl;
}


void CodeGeneration::genInitGlobalVarScalar (const LabelInAssembly &label, const Const &value) {
  ofs << "  " << ASSEMBLY_DATA_SECTION << std::endl;

  switch (value.const_type) {
    case INTEGERC:
      ofs << label << ": .word " << std::get<int>(value.value) << std::endl;
      break;
    case FLOATC:
      ofs << label << ": .word " << float2intMemoryRepresent(std::get<float>(value.value)) << std::endl;
      break;
    case STRINGC:
      ofs << label << ": .string \"" << std::get<std::string>(value.value) << "\"" << std::endl;
      break;
  }

  if (currentSection != ASSEMBLY_DATA_SECTION)
    ofs << currentSection << std::endl;
}


void CodeGeneration::genConstString (const LabelInAssembly &strLabel, const std::string &str) {
  ofs << "  " << ASSEMBLY_DATA_SECTION << std::endl
      << "  .align 8" << std::endl
      << strLabel << ": .string " << str << std::endl
      << "  .zero 1" << std::endl;
  if (currentSection != ASSEMBLY_DATA_SECTION) {
    ofs << "  " << currentSection << std::endl;
  }
}


void CodeGeneration::genAssignExpr (const MemoryLocation &LHS, const MemoryLocation &RHS, const DATA_TYPE &typeLHS, const DATA_TYPE &typeRHS) {
  AllocatedRegister valueRegRHS;
  AllocatedRegister valueRegLHS;
  switch (typeRHS) {
    case INT_TYPE:
      valueRegRHS = regManager.getTempIntRegister("assign_RHS");
      break;
    case FLOAT_TYPE:
      valueRegRHS = regManager.getTempFloatRegister("assign_RHS");
      break;
    default:
      assert(false);
  }

  _genLoadFromMemoryLocation(valueRegRHS, RHS);

  // move or convert
  if (typeLHS == FLOAT_TYPE && typeRHS == INT_TYPE) {
    valueRegLHS = regManager.getTempFloatRegister("assign_LHS");
    _genFCVT_S_W(valueRegLHS, valueRegRHS);
  }
  else if (typeLHS == INT_TYPE && typeRHS == FLOAT_TYPE) {
    valueRegLHS = regManager.getTempIntRegister("assign_LHS");
    _genFCVT_W_S(valueRegLHS, valueRegRHS);
  } else
    valueRegLHS = valueRegRHS;

  // store
  _genStoreToMemoryLocation(valueRegLHS, LHS);
}


void CodeGeneration::genAssignConst (const MemoryLocation &LHS, const Const &value, const DATA_TYPE &typeLHS) {
  AllocatedRegister valueRegRHS;
  AllocatedRegister valueRegLHS;
  switch (value.const_type) {
    case INTEGERC:
      valueRegRHS = regManager.getTempIntRegister("assign_const_RHS");
      _genLI(valueRegRHS, std::get<int>(value.value));
      break;
    case FLOATC:
      valueRegRHS = regManager.getTempFloatRegister("assign_const_RHS");
      _genLoadFloatImm(valueRegRHS, std::get<float>(value.value));
      break;
    default:
      assert(false);
  }

  // move or convert
  if (typeLHS == FLOAT_TYPE && value.const_type == INTEGERC) {
    valueRegLHS = regManager.getTempFloatRegister("assign_const_LHS");
    _genFCVT_S_W(valueRegLHS, valueRegRHS);
  }
  else if (typeLHS == INT_TYPE && value.const_type == FLOATC) {
    valueRegLHS = regManager.getTempIntRegister("assign_const_LHS");
    _genFCVT_W_S(valueRegLHS, valueRegRHS);
  }
  else
    valueRegLHS = valueRegRHS;

  _genStoreToMemoryLocation(valueRegLHS, LHS);
}


void CodeGeneration::genAssignParameters (const FunctionParameter &paramInformation, size_t paramTotalSize, const MemoryLocation &srcLoc, const DATA_TYPE &srcType) {
  AllocatedRegister valueRegRHS;
  AllocatedRegister valueRegLHS;
  if (srcType != ARR_TYPE) {
    switch (srcType) {
      case INT_TYPE:
        valueRegRHS = regManager.getTempIntRegister("assign_param_RHS");
        break;
      case FLOAT_TYPE:
        valueRegRHS = regManager.getTempFloatRegister("assign_param_RHS");
        break;
      default:
        assert(false);
    }
    _genLoadFromMemoryLocation(valueRegRHS, srcLoc);
  }
  else { // srcType == ARR_TYPE
    valueRegRHS = regManager.getTempIntRegister("assign_param_RHS_array");
    if (srcLoc.isAbsoluteMemoryAddress)
      _genLD(valueRegRHS, std::get<StackMemoryOffset>(srcLoc.value), REG_FP);
    else if (std::holds_alternative<StackMemoryOffset>(srcLoc.value))
      _genADDI(valueRegRHS, REG_FP, std::get<StackMemoryOffset>(srcLoc.value));
    else { // holds LabelInAssembly
      _genLA(valueRegRHS, std::get<LabelInAssembly>(srcLoc.value));
    }
  }

  if (paramInformation.dataType.type == FLOAT_TYPE && srcType == INT_TYPE) {
    valueRegLHS = regManager.getTempFloatRegister("assign_param_LHS");
    _genFCVT_S_W(valueRegLHS, valueRegRHS);
  }
  else if (paramInformation.dataType.type == INT_TYPE && srcType == FLOAT_TYPE) {
    valueRegLHS = regManager.getTempIntRegister("assign_param_LHS");
    _genFCVT_W_S(valueRegLHS, valueRegRHS);
  }
  else
    valueRegLHS = valueRegRHS;

  if (std::holds_alternative<Register>(paramInformation.place))
    _genMV(std::get<Register>(paramInformation.place), valueRegLHS);
  else if (srcType != ARR_TYPE)
    _genSWorFSW(valueRegLHS, - paramTotalSize - 16 + std::get<StackMemoryOffset>(paramInformation.place), REG_SP);
  else
    _genSD(valueRegLHS, - paramTotalSize - 16 + std::get<StackMemoryOffset>(paramInformation.place), REG_SP);
}


void CodeGeneration::genLogicalNegation (const MemoryLocation &dstLoc, const MemoryLocation &srcLoc, const DATA_TYPE &srcType) {
  AllocatedRegister valueReg;
  AllocatedRegister resultReg;
  switch (srcType) {
    case INT_TYPE:
      valueReg = regManager.getTempIntRegister("logical_negation_value");
      break;
    case FLOAT_TYPE:
      valueReg = regManager.getTempFloatRegister("logical_negation_value");
      break;
    default:
      assert(false);
  }

  _genLoadFromMemoryLocation(valueReg, srcLoc);

  if (srcType == FLOAT_TYPE)
    resultReg = regManager.getTempIntRegister("logical_negate_result");
  else
    resultReg = valueReg;

  _genConvertToBool(resultReg, valueReg);
  _genSEQZ(resultReg, resultReg);
  _genStoreToMemoryLocation(resultReg, dstLoc);
}


void CodeGeneration::genUnaryNegative (const MemoryLocation &dstLoc, const MemoryLocation &srcLoc, const DATA_TYPE &srcType) {
  AllocatedRegister valueReg;
  switch (srcType) {
    case INT_TYPE:
      valueReg = regManager.getTempIntRegister("unary_negative_value");
      _genLoadFromMemoryLocation(valueReg, srcLoc);
      _genSUB(valueReg, REG_ZERO, valueReg);
      _genStoreToMemoryLocation(valueReg, dstLoc);
      break;
    case FLOAT_TYPE:
      valueReg = regManager.getTempFloatRegister("unary_negative_value");
      _genLoadFromMemoryLocation(valueReg, srcLoc);
      _genFNEG_S(valueReg, valueReg);
      _genStoreToMemoryLocation(valueReg, dstLoc);
      break;
    default:
      assert(false);
  }
}


void CodeGeneration::genArithmeticOperation (const BINARY_OPERATOR &op, const MemoryLocation &dstLoc, const MemoryLocation &srcLoc1, const MemoryLocation &srcLoc2, const DATA_TYPE &dstType, const DATA_TYPE &srcType1, const DATA_TYPE &srcType2) {
  AllocatedRegister resultReg, dstReg;
  bool resultFloat {false};
  { // create scope to let srcReg1 and srcReg2 freed automatically
    AllocatedRegister srcReg1, srcReg2;
    // load left operand
    switch (srcType1) {
      case INT_TYPE:
        srcReg1 = regManager.getTempIntRegister("arithmetic_src_LHS");
        break;
      case FLOAT_TYPE:
        resultFloat = true;
        srcReg1 = regManager.getTempFloatRegister("arithmetic_src_LHS");;
        break;
      default:
        assert(false);
    }
    _genLoadFromMemoryLocation(srcReg1, srcLoc1);
    // load right operand
    switch (srcType2) {
      case INT_TYPE:
        srcReg2 = regManager.getTempIntRegister("arithmetic_src_RHS");
        break;
      case FLOAT_TYPE:
        resultFloat = true;
        srcReg2 = regManager.getTempFloatRegister("arithmetic_src_RHS");
        break;
      default:
        assert(false);
    }
    _genLoadFromMemoryLocation(srcReg2, srcLoc2);
    // convert type if needed
    if (srcType1 == INT_TYPE && srcType2 == FLOAT_TYPE) {
      AllocatedRegister oldSrcReg1 {srcReg1};
      srcReg1 = regManager.getTempFloatRegister("arithmetic_src_LHS");
      _genFCVT_S_W(srcReg1, oldSrcReg1);
    }
    else if (srcType1 == FLOAT_TYPE && srcType2 == INT_TYPE) {
      AllocatedRegister oldSrcReg2 {srcReg2};
      srcReg2 = regManager.getTempFloatRegister("arithmetic_src_RHS");
      _genFCVT_S_W(srcReg2, oldSrcReg2);
    }
    // do operation
    if (resultFloat)
      resultReg = regManager.getTempFloatRegister("arithmetic_result");
    else
      resultReg = regManager.getTempIntRegister("arithmetic_result");

    switch (op) {
      case BINARY_OP_ADD:
        _genADD(resultReg, srcReg1, srcReg2);
        break;
      case BINARY_OP_SUB:
        _genSUB(resultReg, srcReg1, srcReg2);
        break;
      case BINARY_OP_MUL:
        _genMUL(resultReg, srcReg1, srcReg2);
        break;
      case BINARY_OP_DIV:
        _genDIV(resultReg, srcReg1, srcReg2);
        break;
      default:
        assert(false);
    }
  }
  // convert type if needed
  if (dstType == FLOAT_TYPE && !resultFloat) {
    dstReg = regManager.getTempFloatRegister("arithmetic_dst");
    _genFCVT_S_W(dstReg, resultReg);
  }
  else if (dstType == INT_TYPE && resultFloat) {
    dstReg = regManager.getTempIntRegister("arithmetic_dst");
    _genFCVT_W_S(dstReg, resultReg);
  } else
    dstReg = resultReg;

  // store
  _genStoreToMemoryLocation(dstReg, dstLoc);
}


void CodeGeneration::genLogicalOperation (const BINARY_OPERATOR &op, const MemoryLocation &dstLoc, const MemoryLocation &srcLoc1, const MemoryLocation &srcLoc2, const DATA_TYPE &dstType, const DATA_TYPE &srcType1, const DATA_TYPE &srcType2) {
  AllocatedRegister dstReg, resultReg;
  { // create scope to let srcReg1 and srcReg2 freed automatically
    AllocatedRegister srcReg1, srcReg2;
    // decide the needed preprocessing to be applied
    bool srcSameType;
    bool boolSrc;
    switch (op) {
      case BINARY_OP_EQ:
      case BINARY_OP_GE:
      case BINARY_OP_LE:
      case BINARY_OP_NE:
      case BINARY_OP_GT:
      case BINARY_OP_LT:
        srcSameType = true;
        boolSrc = false;
        break;
      case BINARY_OP_AND:
      case BINARY_OP_OR:
        srcSameType = false;
        boolSrc = true;
        break;
      default:
        assert(false);
    }
    // load left operand
    switch (srcType1) {
      case INT_TYPE:
        srcReg1 = regManager.getTempIntRegister("logical_src_LHS");
        break;
      case FLOAT_TYPE:
        srcReg1 = regManager.getTempFloatRegister("logical_src_LHS");
        break;
      default:
        assert(false);
    }
    _genLoadFromMemoryLocation(srcReg1, srcLoc1);
    // load right operand
    switch (srcType2) {
      case INT_TYPE:
        srcReg2 = regManager.getTempIntRegister("logical_src_RHS");
        break;
      case FLOAT_TYPE:
        srcReg2 = regManager.getTempFloatRegister("logical_src_RHS");
        break;
      default:
        assert(false);
    }
    _genLoadFromMemoryLocation(srcReg2, srcLoc2);
    // convert type if needed
    if (srcSameType) {
      if (srcType1 == INT_TYPE && srcType2 == FLOAT_TYPE) {
        AllocatedRegister oldSrcReg1 {srcReg1};
        srcReg1 = regManager.getTempFloatRegister("logical_src_LHS");
        _genFCVT_S_W(srcReg1, oldSrcReg1);
      }
      else if (srcType1 == FLOAT_TYPE && srcType2 == INT_TYPE) {
        AllocatedRegister oldSrcReg2 {srcReg2};
        srcReg2 = regManager.getTempFloatRegister("logical_src_RHS");
        _genFCVT_S_W(srcReg2, oldSrcReg2);
      }
    }
    if (boolSrc) {
      {
        AllocatedRegister oldSrcReg1 {srcReg1};
        srcReg1 = regManager.getTempIntRegister("logical_src_LHS_bool");
        _genConvertToBool(srcReg1, oldSrcReg1);
      }
      {
        AllocatedRegister oldSrcReg2 {srcReg2};
        srcReg2 = regManager.getTempIntRegister("logical_src_RHS_bool");
        _genConvertToBool(srcReg2, oldSrcReg2);
      }
    }
    // do operation
    resultReg = regManager.getTempIntRegister("logical_result");
    switch (op) {
        case BINARY_OP_EQ:
          if (isFloatRegister(srcReg1)){
            _genFEQ_S(resultReg, srcReg1, srcReg2);
          }
          else {
            _genSUB(resultReg, srcReg1, srcReg2);
            _genSEQZ(resultReg, resultReg);
          }
          break;
        case BINARY_OP_GE:
          if (isFloatRegister(srcReg1)) {
            _genFLE_S(resultReg, srcReg2, srcReg1);
          }
          else {
            _genSLT(resultReg, srcReg1, srcReg2);
            _genSEQZ(resultReg, resultReg);
          }
          break;
        case BINARY_OP_LE:
          if (isFloatRegister(srcReg1)) {
            _genFLE_S(resultReg, srcReg1, srcReg2);
          }
          else {
            _genSLT(resultReg, srcReg2, srcReg1);
            _genSEQZ(resultReg, resultReg);
          }
          break;
        case BINARY_OP_NE:
          if (isFloatRegister(srcReg1)){
            _genFEQ_S(resultReg, srcReg1, srcReg2);
            _genSEQZ(resultReg, resultReg);
          }
          else {
            _genSUB(resultReg, srcReg1, srcReg2);
            _genSNEZ(resultReg, resultReg);
          }
          break;
        case BINARY_OP_GT:
          if (isFloatRegister(srcReg1)) {
            _genFLT_S(resultReg, srcReg2, srcReg1);
          }
          else {
            _genSLT(resultReg, srcReg2, srcReg1);
          }
          break;
        case BINARY_OP_LT:
          if (isFloatRegister(srcReg1)) {
            _genFLT_S(resultReg, srcReg1, srcReg2);
          }
          else {
            _genSLT(resultReg, srcReg1, srcReg2);
          }
          break;
        case BINARY_OP_AND:
          _genAND(resultReg, srcReg1, srcReg2);
          break;
        case BINARY_OP_OR:
          _genOR(resultReg, srcReg1, srcReg2);
          break;
        default:
          assert(false);
    }
  }
  // convert type if needed
  switch (dstType) {
    case INT_TYPE:
      dstReg = resultReg;
      break;
    case FLOAT_TYPE:
      dstReg = regManager.getTempFloatRegister("logical_dst");
      _genFCVT_S_W(dstReg, resultReg);
      break;
    default:
      assert(false);
  }
  // store
  _genStoreToMemoryLocation(dstReg, dstLoc);
}


void CodeGeneration::genReturnValue (AST *exprNode) {
  const DATA_TYPE &exprType {exprNode->dataType.type};
  const DATA_TYPE &retType {std::get<FunctionSignature>(symtab.getCurrentFunction()->attribute).returnType};
  if (exprType == INT_TYPE && retType == INT_TYPE) {
    _genLoadFromMemoryLocation(REG_A0, exprNode->place);
  }
  else if (exprType == FLOAT_TYPE && retType == FLOAT_TYPE) {
    _genLoadFromMemoryLocation(REG_FA0, exprNode->place);
  }
  else if (exprType == INT_TYPE && retType == FLOAT_TYPE) {
    AllocatedRegister valueReg {regManager.getTempIntRegister("return_value_src_int")};
    _genLoadFromMemoryLocation(valueReg, exprNode->place);
    _genFCVT_S_W(REG_FA0, valueReg);
  }
  else if (exprType == FLOAT_TYPE && retType == INT_TYPE) {
    AllocatedRegister valueReg {regManager.getTempFloatRegister("return_value_src_float")};
    _genLoadFromMemoryLocation(valueReg, exprNode->place);
    _genFCVT_W_S(REG_A0, valueReg);
  }
  else
    assert(false);
}


void CodeGeneration::genBranchTest (AST *testNode, const LabelInAssembly &branchFalseLabel) {
  AllocatedRegister valueReg;
  AllocatedRegister resultReg;
  switch (testNode->dataType.type) {
    case INT_TYPE:
      valueReg = regManager.getTempIntRegister("branch_test_src");
      resultReg = valueReg;
      break;
    case FLOAT_TYPE:
      valueReg = regManager.getTempFloatRegister("branch_test_src");
      resultReg = regManager.getTempIntRegister("branch_test_result");
      break;
    default:
      assert(false);
  }
  _genLoadFromMemoryLocation(valueReg, testNode->place);
  _genConvertToBool(resultReg, valueReg);
  _genBEQZ(resultReg, branchFalseLabel);
}


void CodeGeneration::genShortCircuitEvaluation (const MemoryLocation &srcLoc, const DATA_TYPE &srcType, const BINARY_OPERATOR &op, const MemoryLocation &dstLoc, const LabelInAssembly &finalLabel) {
  AllocatedRegister valueReg;
  AllocatedRegister resultReg;
  switch (srcType) {
    case INT_TYPE:
      valueReg = regManager.getTempIntRegister("short_circuit_src");
      resultReg = valueReg;
      break;
    case FLOAT_TYPE:
      valueReg = regManager.getTempFloatRegister("short_circuit_src");
      resultReg = regManager.getTempIntRegister("shor_circuit_result");
      break;
    default:
      assert(false);
  }
  _genLoadFromMemoryLocation(valueReg, srcLoc);
  _genConvertToBool(resultReg, valueReg);
  _genStoreToMemoryLocation(resultReg, dstLoc);
  switch (op) {
    case BINARY_OP_AND:
      _genBEQZ(resultReg, finalLabel);
      break;
    case BINARY_OP_OR:
      _genBNEZ(resultReg, finalLabel);
      break;
    default:
      assert(false);
  }
}


void CodeGeneration::_genADD (const Register &rd, const Register &rs1, const Register &rs2) {
  assert(isFloatRegister(rd) == isFloatRegister(rs1) && isFloatRegister(rs1) == isFloatRegister(rs2));
  if (isFloatRegister(rd))
    ofs << "  fadd.s " << rd << ", " << rs1 << ", " << rs2 << std::endl;
  else
    ofs << "  add " << rd << ", " << rs1 << ", " << rs2 << std::endl;
}


void CodeGeneration::_genADDI (const Register &rd, const Register &rs1, int imm) {
  if (!(-2048 <= imm && imm < 2048)) {
    AllocatedRegister tmpIntReg {regManager.getTempIntRegister("addi_imm")};
    _genLI(tmpIntReg, imm);
    _genADD(rd, rs1, tmpIntReg);
  }
  else
    ofs << "  addi " << rd << ", " << rs1 << ", " << imm << std::endl;
}


void CodeGeneration::_genSUB (const Register &rd, const Register &rs1, const Register &rs2) {
  assert(isFloatRegister(rd) == isFloatRegister(rs1) && isFloatRegister(rs1) == isFloatRegister(rs2));
  if (isFloatRegister(rd))
    ofs << "  fsub.s " << rd << ", " << rs1 << ", " << rs2 << std::endl;
  else
    ofs << "  sub " << rd << ", " << rs1 << ", " << rs2 << std::endl;
}


void CodeGeneration::_genMUL (const Register &rd, const Register &rs1, const Register &rs2) {
  assert(isFloatRegister(rd) == isFloatRegister(rs1) && isFloatRegister(rs1) == isFloatRegister(rs2));
  if (isFloatRegister(rd))
    ofs << "  fmul.s " << rd << ", " << rs1 << ", " << rs2 << std::endl;
  else
    ofs << "  mul " << rd << ", " << rs1 << ", " << rs2 << std::endl;
}


void CodeGeneration::_genDIV (const Register &rd, const Register &rs1, const Register &rs2) {
  assert(isFloatRegister(rd) == isFloatRegister(rs1) && isFloatRegister(rs1) == isFloatRegister(rs2));
  if (isFloatRegister(rd))
    ofs << "  fdiv.s " << rd << ", " << rs1 << ", " << rs2 << std::endl;
  else
    ofs << "  div " << rd << ", " << rs1 << ", " << rs2 << std::endl;
}


void CodeGeneration::_genAND (const Register &rd, const Register &rs1, const Register &rs2) {
  ofs << "  and " << rd << ", " << rs1 << ", " << rs2 << std::endl;
}


void CodeGeneration::_genOR (const Register &rd, const Register &rs1, const Register &rs2) {
  ofs << "  or " << rd << ", " << rs1 << ", " << rs2 << std::endl;
}


void CodeGeneration::_genFNEG_S (const Register &rd, const Register &rs1) {
  ofs << "  fneg.s " << rd << ", " << rs1 << std::endl;
}


void CodeGeneration::_genSEQZ (const Register &rd, const Register &rs1) {
  ofs << "  seqz " << rd << ", " << rs1 << std::endl;
}


void CodeGeneration::_genSNEZ (const Register &rd, const Register &rs1) {
  ofs << "  snez " << rd << ", " << rs1 << std::endl;
}


void CodeGeneration::_genSLT (const Register &rd, const Register &rs1, const Register &rs2) {
  ofs << "  slt " << rd << ", " << rs1 << ", " << rs2 << std::endl;
}


void CodeGeneration::_genFEQ_S (const Register &rd, const Register &rs1, const Register &rs2) {
  ofs << "  feq.s " << rd << ", " << rs1 << ", " << rs2 << std::endl;
}


void CodeGeneration::_genFLT_S (const Register &rd, const Register &rs1, const Register &rs2) {
  ofs << "  flt.s " << rd << ", " << rs1 << ", " << rs2 << std::endl;
}


void CodeGeneration::_genFLE_S (const Register &rd, const Register &rs1, const Register &rs2) {
  ofs << "  fle.s " << rd << ", " << rs1 << ", " << rs2 << std::endl;
}


void CodeGeneration::_genBEQZ (const Register &rs1, const LabelInAssembly &offset) {
  LabelInAssembly middleLabel {makeBranchLabel()};
  ofs << "  bnez " << rs1 << ", " << middleLabel << std::endl;
  _genJ(offset); // handle too far away offset
  ofs << middleLabel << ":" << std::endl;
}


void CodeGeneration::_genBNEZ (const Register &rs1, const LabelInAssembly &offset) {
  LabelInAssembly middleLabel {makeBranchLabel()};
  ofs << "  beqz " << rs1 << ", " << middleLabel << std::endl;
  _genJ(offset); // handle too far away offset
  ofs << middleLabel << ":" << std::endl;
}


void CodeGeneration::_genJ (const LabelInAssembly &offset) {
  AllocatedRegister absAddrReg {regManager.getTempIntRegister("absolute_address_of(" + offset + ")")};
  _genLA(absAddrReg, offset);
  ofs << "  jr " << absAddrReg << std::endl;
}


void CodeGeneration::_genLWorFLW (const Register &rd, int imm, const Register &rs1) {
  assert(!isFloatRegister(rs1));
  assert(rd != REG_FP && rd != REG_SP);
  if (!(-2048 <= imm && imm < 2048)) {
    AllocatedRegister tmpIntReg {regManager.getTempIntRegister("lw_imm")};
    _genLI(tmpIntReg, imm);
    _genADD(tmpIntReg, tmpIntReg, rs1);
    if (isFloatRegister(rd))
      ofs << "  flw " << rd << ", " << 0 << "(" << tmpIntReg << ")" << std::endl;
    else
      ofs << "  lw " << rd << ", " << 0 << "(" << tmpIntReg << ")" << std::endl;
  }
  else {
    if (isFloatRegister(rd))
      ofs << "  flw " << rd << ", " << imm << "(" << rs1 << ")" << std::endl;
    else
      ofs << "  lw " << rd << ", " << imm << "(" << rs1 << ")" << std::endl;
  }
}


void CodeGeneration::_genLWorFLW (const Register &rd, const LabelInAssembly &symbol, const Register &rt) {
  assert(!isFloatRegister(rt));
  assert(rd != REG_FP && rd != REG_SP);
  if (isFloatRegister(rd))
    ofs << "  flw " << rd << ", " << symbol << ", " << rt << std::endl;
  else
    ofs << "  lw " << rd << ", " << symbol << std::endl;
}


void CodeGeneration::_genLWorFLW (const Register &rd, const LabelInAssembly &symbol) {
  assert(rd != REG_FP && rd != REG_SP);
  if (isFloatRegister(rd)) {
    AllocatedRegister tmpIntReg {regManager.getTempIntRegister("lw_tmp_int_reg")};
    ofs << "  flw " << rd << ", " << symbol << ", " << tmpIntReg << std::endl;
  }
  else
    ofs << "  lw " << rd << ", " << symbol << std::endl;
}


void CodeGeneration::_genLD (const Register &rd, int imm, const Register &rs1) {
  if (!(-2048 <= imm && imm < 2048)) {
    AllocatedRegister tmpIntReg {regManager.getTempIntRegister("ld_imm")};
    _genLI(tmpIntReg, imm);
    _genADD(tmpIntReg, tmpIntReg, rs1);
    ofs << "  ld " << rd << ", " << 0 << "(" << tmpIntReg << ")" << std::endl;
  }
  else {
    ofs << "  ld " << rd << ", " << imm << "(" << rs1 << ")" << std::endl;
  }
}


// this function cannot load 64-bit data
void CodeGeneration::_genLoadFromMemoryLocation (const Register &rd, const MemoryLocation &memLoc) {
  if (std::holds_alternative<LabelInAssembly>(memLoc.value))
    _genLWorFLW(rd, std::get<LabelInAssembly>(memLoc.value));
  else {
    if (memLoc.isAbsoluteMemoryAddress) {
      AllocatedRegister absAddrReg {regManager.getTempIntRegister("absolute_memory_address")};
      _genLD(absAddrReg, std::get<StackMemoryOffset>(memLoc.value), REG_FP);
      _genLWorFLW(rd, 0, absAddrReg);
    }
    else
      _genLWorFLW(rd, std::get<StackMemoryOffset>(memLoc.value), REG_FP);
  }
}


void CodeGeneration::_genSWorFSW (const Register &rd, int imm, const Register &rs2) {
  assert(!isFloatRegister(rs2));
  assert(rd != REG_FP && rd != REG_SP);
  if (!(-2048 <= imm && imm < 2048)) {
    AllocatedRegister tmpIntReg {regManager.getTempIntRegister("sw_imm")};
    _genLI(tmpIntReg, imm);
    _genADD(tmpIntReg, tmpIntReg, rs2);
    if (isFloatRegister(rd))
      ofs << "  fsw " << rd << ", " << 0 << "(" << tmpIntReg << ")" << std::endl;
    else
      ofs << "  sw " << rd << ", " << 0 << "(" << tmpIntReg << ")" << std::endl;
  }
  else {
    if (isFloatRegister(rd))
      ofs << "  fsw " << rd << ", " << imm << "(" << rs2 << ")" << std::endl;
    else
      ofs << "  sw " << rd << ", " << imm << "(" << rs2 << ")" << std::endl;
  }
}


void CodeGeneration::_genSWorFSW (const Register &rd, const LabelInAssembly &symbol) {
  assert(rd != REG_FP && rd != REG_SP);
  AllocatedRegister tmpIntReg {regManager.getTempIntRegister("sw_tmp_int_reg")};
  if (isFloatRegister(rd))
    ofs << "  fsw " << rd << ", " << symbol << ", " << tmpIntReg << std::endl;
  else
    ofs << "  sw " << rd << ", " << symbol << ", " << tmpIntReg << std::endl;
}


void CodeGeneration::_genSD (const Register &rd, int imm, const Register &rs2) {
  assert(!isFloatRegister(rd) && !isFloatRegister(rs2));
  if (!(-2048 <= imm && imm < 2048)) {
    AllocatedRegister tmpIntReg {regManager.getTempIntRegister("sd_imm")};
    _genLI(tmpIntReg, imm);
    _genADD(tmpIntReg, tmpIntReg, rs2);
    ofs << "  sd " << rd << ", " << 0 << "(" << tmpIntReg << ")" << std::endl;
  }
  else {
    ofs << "  sd " << rd << ", " << imm << "(" << rs2 << ")" << std::endl;
  }
}


void CodeGeneration::_genStoreToMemoryLocation (const Register &rd, const MemoryLocation &memLoc) {
  if (std::holds_alternative<LabelInAssembly>(memLoc.value))
    _genSWorFSW(rd, std::get<LabelInAssembly>(memLoc.value));
  else {
    if (memLoc.isAbsoluteMemoryAddress) {
      AllocatedRegister absAddrReg {regManager.getTempIntRegister("absolute_memory_address")};
      _genLD(absAddrReg, std::get<StackMemoryOffset>(memLoc.value), REG_FP);
      _genSWorFSW(rd, 0, absAddrReg);
    }
    else
      _genSWorFSW(rd, std::get<StackMemoryOffset>(memLoc.value), REG_FP);
  }
}


void CodeGeneration::_genLA (const Register &rd, const LabelInAssembly &symbol) {
  ofs << "  la " << rd << ", " << symbol << std::endl;
}


void CodeGeneration::_genLI (const Register &rd, int imm) {
  ofs << "  li " << rd << ", " << imm << std::endl;
}


void CodeGeneration::_genLoadFloatImm (const Register &rd, float imm) {
  AllocatedRegister tmpIntReg {regManager.getTempIntRegister("load_float_imm")};
  _genLI(tmpIntReg, float2intMemoryRepresent(imm));
  _genFMV_W_X(rd, tmpIntReg);
}


void CodeGeneration::_genMV (const Register &rd, const Register &rs1) {
  assert(isFloatRegister(rd) == isFloatRegister(rs1));
  if (isFloatRegister(rd)) {
    ofs << "  fmv.s " << rd << ", " << rs1 << std::endl;
  }
  else
    ofs << "  mv " << rd << ", " << rs1 << std::endl;
}


void CodeGeneration::_genFCVT_W_S (const Register &rd, const Register &rs1) {
  assert(!isFloatRegister(rd) && isFloatRegister(rs1));
  if (!hasSetRoundingMode) {
    _genLI(rd, 1);
    ofs << "  fsrm " << rd << std::endl;
    hasSetRoundingMode = true;
  }
  ofs << "  fcvt.w.s " << rd << ", " << rs1 << std::endl;
}


void CodeGeneration::_genFCVT_S_W (const Register &rd, const Register &rs1) {
  assert(isFloatRegister(rd) && !isFloatRegister(rs1));
  ofs << "  fcvt.s.w " << rd << ", " << rs1 << std::endl;
}


void CodeGeneration::_genFMV_W_X (const Register &rd, const Register &rs1) {
  assert(isFloatRegister(rd) && !isFloatRegister(rs1));
  ofs << "  fmv.w.x " << rd << ", " << rs1 << std::endl;
}


void CodeGeneration::_genConvertToBool (const Register &rd, const Register &rs1) {
  assert(!isFloatRegister(rd));
  if (isFloatRegister(rs1)) {
    AllocatedRegister zeroFloatReg {regManager.getTempFloatRegister("convert_bool_zero")};
    _genLoadFloatImm(zeroFloatReg, 0.f);
    _genFEQ_S(rd, rs1, zeroFloatReg);
    _genSEQZ(rd, rd);
  }
  else {
    _genSNEZ(rd, rs1);
  }
}


void CodeGeneration::_genCALL (const LabelInAssembly &funcLabel) {
  ofs << "  call " << funcLabel << std::endl;
}


void CodeGeneration::_genRET () {
  ofs << "  ret" << std::endl;
}
