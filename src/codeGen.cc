#include <fstream>
#include <cassert>
#include <cstring>
#include "codeGen.hh"
#include "print.hh" // for output operator of Const


CodeGeneration::CodeGeneration (AST *_prog, const std::string &_filename)
: filename{_filename}, prog{_prog}, ofs{_filename, std::ofstream::out}, currentSection{ASSEMBLY_DATA_SECTION} {
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
      std::cerr << typeDesc << std::endl;
      assert(false);
  }
}


inline LabelInAssembly CodeGeneration::makeGlobalVarLabel (const std::string &varName) {
  return "g_" + varName;
}


inline LabelInAssembly CodeGeneration::makeFuncLabel (const std::string &funcName) {
  if (funcName == "main") return "_start_MAIN";
  return "f_" + funcName;
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
      StackMemoryOffset stackOffset {stackMemManager.getMemory(varSize)};
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
  exprNode->place = stackMemManager.getMemory(dataSize);

  const EXPRSemanticValue &exprSemanticValue = std::get<EXPRSemanticValue>(exprNode->semanticValue);
  if (exprSemanticValue.isConstEval) {
    genAssignConst(exprNode->place, exprSemanticValue.constEvalValue, exprNode->dataType.type);
  }
  else {
    for (AST *operandNode : exprNode->children) {
      visitExpressionComponent(operandNode);
    }
    switch (exprSemanticValue.kind) {
      case UNARY_OPERATION: {
        UNARY_OPERATOR op {std::get<UNARY_OPERATOR>(exprSemanticValue.op)};
        AST *operand {exprNode->children[0]};
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
            genArithmeticOperation(op, exprNode->place, lOperand->place, rOperand->place, exprNode->dataType.type, lOperand->dataType.type, rOperand->dataType.type);
            break;
          case BINARY_OP_EQ:
          case BINARY_OP_GE:
          case BINARY_OP_LE:
          case BINARY_OP_NE:
          case BINARY_OP_GT:
          case BINARY_OP_LT:
          case BINARY_OP_AND:
          case BINARY_OP_OR:
            genLogicalOperation(op, exprNode->place, lOperand->place, rOperand->place, exprNode->dataType.type, lOperand->dataType.type, rOperand->dataType.type);
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
  stmtNode->place = stackMemManager.getMemory(dataSize);

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

  genCallerSaveRegisters();
  // TODO: pass argument (on register and on stack (if needed))
  genPassParametersBeforeFunctionCall(paramListNode);
  genCallFunction(funcLabel);
  if (!(stmtNode->dataType.type == VOID_TYPE))
    genSaveReturnValue(stmtNode->place, (funcName == "fread"));
  genClearParametersOnStackAfterFunctionCall(paramListNode);
  // TODO: clear argument (on stack only)
  genCallerRestoreRegisters();
}


void CodeGeneration::visitAssignmentStatement (AST *stmtNode) {
  AST *LHSIDNode {stmtNode->children[0]};
  AST *RHSExprNode {stmtNode->children[1]};

  visitVarRefLValue(LHSIDNode);
  visitExpressionComponent(RHSExprNode);

  size_t dataSize {getTypeSize(stmtNode->dataType)};
  stmtNode->place = stackMemManager.getMemory(dataSize);
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
  constNode->place = stackMemManager.getMemory(dataSize);
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
  Register valueReg {REG_T0};
  Register appliedReg {REG_T1};
  Register tmpIntReg {REG_T2};
  const ArrayProperties &arrProp {std::get<TypeDescriptor>(idEntry->attribute).arrayProperties};
  StackMemoryOffset valueOffset {stackMemManager.getMemory(SIZEOF_REGISTER)};
  _genSWorFSW(REG_ZERO, valueOffset, REG_FP);
  for (size_t idx {0}; idx < idNode->children.size(); ++idx) {
    // multiplied by the size of current dimension
    _genLWorFLW(valueReg, valueOffset, REG_FP);
    _genLI(appliedReg, arrProp.dimensions[idx]);
    _genMUL(valueReg, valueReg, appliedReg);
    _genSWorFSW(valueReg, valueOffset, REG_FP);
    // added by the value of current index
    const auto &dimComponentNode {idNode->children[idx]};
    visitExpressionComponent(dimComponentNode);
    _genLWorFLW(valueReg, valueOffset, REG_FP);
    _genLoadFromMemoryLocation(appliedReg, dimComponentNode->place, tmpIntReg);
    _genADD(valueReg, valueReg, appliedReg);
    _genSWorFSW(valueReg, valueOffset, REG_FP);
  }
  // multiplied by the size of array element type
  _genLWorFLW(valueReg, valueOffset, REG_FP);
  _genLI(appliedReg, getTypeSize(arrProp.elementType));
  _genMUL(valueReg, valueReg, appliedReg);
  // added by base address
  if (std::holds_alternative<LabelInAssembly>(idEntry->place.value))
    _genLA(appliedReg, std::get<LabelInAssembly>(idEntry->place.value));
  else {
    _genMV(appliedReg, REG_FP);
    _genADDI(appliedReg, appliedReg, std::get<StackMemoryOffset>(idEntry->place.value));
  }
  _genADD(valueReg, valueReg, appliedReg);
  _genSD(valueReg, valueOffset, REG_FP);

  idNode->place.value = valueOffset;
  idNode->place.isAbsoluteMemoryAddress = true;
}


void CodeGeneration::visitFunctionDeclaration (AST *declNode) {
  // Although the function definition will not conflict with declaration (by semantic check),
  // the reason why we cannot ignore this AST node is that there maybe type declaration in the return type.
  // We have to do something on symbol table as in semantic check.
  const auto &returnTypeNode {declNode->children[0]};
  const auto &funcNameIDNode {declNode->children[1]};
  const auto &paramListNode {declNode->children[2]};

  const std::string &funcName {std::get<IdentifierSemanticValue>(funcNameIDNode->semanticValue).identifierName};
  visitTypeSpecifier(returnTypeNode);

  symtab.openScope();
  const std::vector<TypeDescriptor> params {visitParameterDeclarationList(paramListNode, false)};
  symtab.closeScope();

  symtab.addFunctionSymbol(funcName, FunctionSignature{returnTypeNode->dataType.type, std::move(params), false});
}


void CodeGeneration::visitFunctionDefinition (AST *defiNode) {
  AST *returnTypeNode {defiNode->children[0]};
  AST *funcNameIDNode {defiNode->children[1]};
  AST *paramListNode {defiNode->children[2]};
  AST *bodyBlockNode {defiNode->children[3]};

  visitTypeSpecifier(returnTypeNode);
  const TypeDescriptor &returnTypeDesc {returnTypeNode->dataType};
  const std::string &funcName {std::get<IdentifierSemanticValue>(funcNameIDNode->semanticValue).identifierName};
  LabelInAssembly funcLabel {makeFuncLabel(funcName)};

  symtab.openScope();
  const std::vector<TypeDescriptor> params {visitParameterDeclarationList(paramListNode, true)};
  symtab.stashScope();
  SymbolTableEntry *funcEntry {symtab.addFunctionSymbol(funcName, {returnTypeDesc.type, std::move(params), true}, funcLabel)};
  symtab.popStash();
  symtab.enterFunction(funcEntry);
  stackMemManager.enterProcedure();
  genFunctionPrologue(funcLabel);
  visitBlock(bodyBlockNode);
  genFunctionEpilogue(funcLabel, stackMemManager.getProcedureMemoryConsumption());
  stackMemManager.leaveProcedure();
  symtab.leaveFunction();
  symtab.closeScope();
}


std::vector<TypeDescriptor> CodeGeneration::visitParameterDeclarationList (AST *paramListNode, bool isDefinition) {
  // TODO: handle function declaration/definition that has parameters
  return std::vector<TypeDescriptor>{};
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


void CodeGeneration::visitForStatement (AST *) {
  std::cerr << "CodeGeneration::visitForStatement not yet implemented" << std::endl;
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
  if (exprNode->nodeType == NUL_NODE) {
    return;
  }

  visitExpressionComponent(exprNode);
  genReturn(exprNode->place);
}


void CodeGeneration::genCallerSaveRegisters () {
  _genADDI(REG_SP, REG_SP, -8 * CALLER_SAVE_REGISTERS.size());
  for (size_t idx {0}; idx < CALLER_SAVE_REGISTERS.size(); ++idx) {
    _genSWorFSW(CALLER_SAVE_REGISTERS[idx], -8 * idx, REG_SP);
  }
}


void CodeGeneration::genCallerRestoreRegisters () {
  for (size_t idx {0}; idx < CALLER_SAVE_REGISTERS.size(); ++idx) {
    _genLWorFLW(CALLER_SAVE_REGISTERS[idx], -8 * idx, REG_SP);
  }
  _genADDI(REG_SP, REG_SP, -8 * CALLER_SAVE_REGISTERS.size());
}


void CodeGeneration::genPassParametersBeforeFunctionCall (const AST *paramListNode) {
  std::cerr << "CodeGeneration::genPassParametersBeforeFunctionCall only support one parameter (a0)" << std::endl;
  if (paramListNode->children.size() == 0)
    return;

  const auto &childNode {paramListNode->children[0]};
  if (childNode->dataType.type == CONST_STRING_TYPE) {
    _genLA(REG_A0, std::get<LabelInAssembly>(childNode->place.value));
  }
  else if (childNode->dataType.type == FLOAT_TYPE) {
    Register tmpIntReg {REG_T0};
    _genLoadFromMemoryLocation(REG_FA0, childNode->place, tmpIntReg);
  }
  else {
    Register tmpIntReg {REG_T0};
    _genLoadFromMemoryLocation(REG_A0, childNode->place, tmpIntReg);
  }
}


void CodeGeneration::genClearParametersOnStackAfterFunctionCall (const AST *) {
  std::cerr << "CodeGeneration::genClearParametersOnStackAfterFunctionCall not yet implemented" << std::endl;
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
  for (size_t idx {0}; idx < CALLEE_SAVE_REGISTERS.size(); ++idx) {
    _genSWorFSW(CALLEE_SAVE_REGISTERS[idx], 8 * idx, REG_SP);
  }
}


// the parameter $frameSize here does not contain the saved registers
void CodeGeneration::genFunctionEpilogue (const LabelInAssembly &funcLabel, size_t frameSize) {
  if (currentSection != ASSEMBLY_TEXT_SECTION) {
    ofs << "  " << ASSEMBLY_TEXT_SECTION << std::endl;
    currentSection = ASSEMBLY_TEXT_SECTION;
  }

  // step 1: restore registers
  for (size_t idx {0}; idx < CALLEE_SAVE_REGISTERS.size(); ++idx) {
    _genLWorFLW(CALLEE_SAVE_REGISTERS[idx], 8 * idx, REG_SP);
  }

  // step 2: move fp and sp
  _genADDI(REG_SP, REG_FP, 8);
  _genLD(REG_FP, 0, REG_FP);
  _genLD(REG_RA, 0, REG_SP);
  _genRET();

  // step 3: generate frame size
  ofs << "  " << ASSEMBLY_DATA_SECTION << std::endl
      << makeFrameSizeLabel(funcLabel) << ": .word " << (frameSize + 8 * CALLEE_SAVE_REGISTERS.size()) << std::endl
      << "  " << ASSEMBLY_TEXT_SECTION << std::endl;
}


void CodeGeneration::genCallFunction (const LabelInAssembly &funcLabel) {
  _genADDI(REG_SP, REG_SP, -SIZEOF_REGISTER);
  _genCALL(funcLabel);
  _genADDI(REG_SP, REG_SP, SIZEOF_REGISTER);
}


void CodeGeneration::genSaveReturnValue (const MemoryLocation &place, bool isFread) {
  // TODO: handle float return value differently ?
  Register tmpIntReg {REG_T0};
  if (isFread)
    _genStoreToMemoryLocation(REG_FA0, place, tmpIntReg);
  else
    _genStoreToMemoryLocation(REG_A0, place, tmpIntReg);
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
  Register tmpIntReg {REG_T0};
  Register valueRegRHS;
  Register valueRegLHS;
  switch (typeRHS) {
    case INT_TYPE:
      valueRegRHS = REG_T1;
      break;
    case FLOAT_TYPE:
      valueRegRHS = REG_FT1;
      break;
    default:
      assert(false);
  }
  switch (typeLHS) {
    case INT_TYPE:
      valueRegLHS = REG_T1;
      break;
    case FLOAT_TYPE:
      valueRegLHS = REG_FT1;
      break;
    default:
      assert(false);
  }

  _genLoadFromMemoryLocation(valueRegRHS, RHS, tmpIntReg);

  // move or convert
  if (typeLHS == FLOAT_TYPE && typeRHS == INT_TYPE)
    _genFCVT_S_W(valueRegLHS, valueRegRHS);
  else if (typeLHS == INT_TYPE && typeRHS == FLOAT_TYPE)
    _genFCVT_W_S(valueRegLHS, valueRegRHS);
  else
    _genMV(valueRegLHS, valueRegRHS); // this operation is redundant since LHS and RHS are exactly the same register

  // store
  _genStoreToMemoryLocation(valueRegLHS, LHS, tmpIntReg);
}


void CodeGeneration::genAssignConst (const MemoryLocation &LHS, const Const &value, const DATA_TYPE &typeLHS) {
  Register tmpIntReg {REG_T0};
  Register valueRegRHS;
  Register valueRegLHS;
  switch (typeLHS) {
    case INT_TYPE:
      valueRegLHS = REG_T1;
      break;
    case FLOAT_TYPE:
      valueRegLHS = REG_FT1;
      break;
    default:
      assert(false);
  }

  // load
  switch (value.const_type) {
    case INTEGERC:
      valueRegRHS = REG_T1;
      _genLI(valueRegRHS, std::get<int>(value.value));
      break;
    case FLOATC:
      valueRegRHS = REG_FT1;
      _genLoadFloatImm(valueRegRHS, std::get<float>(value.value));
      break;
    default:
      assert(false);
  }

  // move or convert
  if (typeLHS == FLOAT_TYPE && value.const_type == INTEGERC)
    _genFCVT_S_W(valueRegLHS, valueRegRHS);
  else if (typeLHS == INT_TYPE && value.const_type == FLOATC)
    _genFCVT_W_S(valueRegLHS, valueRegRHS);
  else
    _genMV(valueRegLHS, valueRegRHS);

  _genStoreToMemoryLocation(valueRegLHS, LHS, tmpIntReg);
}


void CodeGeneration::genLogicalNegation (const MemoryLocation &, const MemoryLocation &, const DATA_TYPE &) {
  std::cerr << "CodeGeneration::genLogicalNegation not yet implemented" << std::endl;
}


void CodeGeneration::genUnaryNegative (const MemoryLocation &, const MemoryLocation &, const DATA_TYPE &) {
  std::cerr << "CodeGeneration::genUnaryNegative not yet implemented" << std::endl;
}


void CodeGeneration::genArithmeticOperation (const BINARY_OPERATOR &op, const MemoryLocation &dstLoc, const MemoryLocation &srcLoc1, const MemoryLocation &srcLoc2, const DATA_TYPE &dstType, const DATA_TYPE &srcType1, const DATA_TYPE &srcType2) {
  Register tmpIntReg {REG_T0};
  Register dstReg, srcReg1, srcReg2;
  Register middleReg {REG_T6};
  // load left operand
  switch (srcType1) {
    case INT_TYPE:
      srcReg1 = REG_T1;
      break;
    case FLOAT_TYPE:
      middleReg = REG_FT6;
      srcReg1 = REG_FT1;
      break;
    default:
      assert(false);
  }
  _genLoadFromMemoryLocation(srcReg1, srcLoc1, tmpIntReg);
  // load right operand
  switch (srcType2) {
    case INT_TYPE:
      srcReg2 = REG_T2;
      break;
    case FLOAT_TYPE:
      middleReg = REG_FT6;
      srcReg2 = REG_FT2;
      break;
    default:
      assert(false);
  }
  _genLoadFromMemoryLocation(srcReg2, srcLoc2, tmpIntReg);
  // convert type if needed
  if (srcType1 == INT_TYPE && srcType2 == FLOAT_TYPE) {
    _genFCVT_S_W(REG_FT1, srcReg1);
    srcReg1 = REG_FT1;
  }
  else if (srcType1 == FLOAT_TYPE && srcType2 == INT_TYPE) {
    _genFCVT_S_W(REG_FT2, srcReg2);
    srcReg2 = REG_FT2;
  }
  // do operation
  switch (op) {
    case BINARY_OP_ADD:
      _genADD(middleReg, srcReg1, srcReg2);
      break;
    case BINARY_OP_SUB:
      _genSUB(middleReg, srcReg1, srcReg2);
      break;
    case BINARY_OP_MUL:
      _genMUL(middleReg, srcReg1, srcReg2);
      break;
    case BINARY_OP_DIV:
      _genDIV(middleReg, srcReg1, srcReg2);
      break;
    default:
      assert(false);
  }
  // convert type if needed
  switch (dstType) {
    case INT_TYPE:
      if (isFloatRegister(middleReg))
        _genFCVT_W_S((dstReg = REG_T3), middleReg);
      else
        dstReg = middleReg;
      break;
    case FLOAT_TYPE:
      if (isFloatRegister(middleReg))
        dstReg = middleReg;
      else
        _genFCVT_S_W((dstReg = REG_FT3), middleReg);
      break;
    default:
      assert(false);
  }
  // store
  _genStoreToMemoryLocation(dstReg, dstLoc, tmpIntReg);
}


void CodeGeneration::genLogicalOperation (const BINARY_OPERATOR &op, const MemoryLocation &dstLoc, const MemoryLocation &srcLoc1, const MemoryLocation &srcLoc2, const DATA_TYPE &dstType, const DATA_TYPE &srcType1, const DATA_TYPE &srcType2) {
  Register tmpIntReg {REG_T0};
  Register tmpFloatReg {REG_FT0};
  Register dstReg, srcReg1, srcReg2;
  Register middleReg {REG_T6};
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
      srcReg1 = REG_T1;
      break;
    case FLOAT_TYPE:
      srcReg1 = REG_FT1;
      break;
    default:
      assert(false);
  }
  _genLoadFromMemoryLocation(srcReg1, srcLoc1, tmpIntReg);
  // load right operand
  switch (srcType2) {
    case INT_TYPE:
      srcReg2 = REG_T2;
      break;
    case FLOAT_TYPE:
      srcReg2 = REG_FT2;
      break;
    default:
      assert(false);
  }
  _genLoadFromMemoryLocation(srcReg2, srcLoc2, tmpIntReg);
  // convert type if needed
  if (srcSameType) {
    if (srcType1 == INT_TYPE && srcType2 == FLOAT_TYPE) {
      _genFCVT_S_W(REG_FT1, srcReg1);
      srcReg1 = REG_FT1;
    }
    else if (srcType1 == FLOAT_TYPE && srcType2 == INT_TYPE) {
      _genFCVT_S_W(REG_FT2, srcReg2);
      srcReg2 = REG_FT2;
    }
  }
  if (boolSrc) {
    _genConvertToBool(REG_T1, srcReg1, tmpFloatReg);
    srcReg1 = REG_T1;
    _genConvertToBool(REG_T2, srcReg2, tmpFloatReg);
    srcReg2 = REG_T2;
  }
  // do operation
  switch (op) {
      case BINARY_OP_EQ:
        if (isFloatRegister(srcReg1)){
          _genFEQ_S(middleReg, srcReg1, srcReg2);
        }
        else {
          _genSUB(middleReg, srcReg1, srcReg2);
          _genSEQZ(middleReg, middleReg);
        }
        break;
      case BINARY_OP_GE:
        if (isFloatRegister(srcReg1)) {
          _genFLE_S(middleReg, srcReg2, srcReg1);
        }
        else {
          _genSLT(middleReg, srcReg1, srcReg2);
          _genSEQZ(middleReg, middleReg);
        }
        break;
      case BINARY_OP_LE:
        if (isFloatRegister(srcReg1)) {
          _genFLE_S(middleReg, srcReg1, srcReg2);
        }
        else {
          _genSLT(middleReg, srcReg2, srcReg1);
          _genSEQZ(middleReg, middleReg);
        }
        break;
      case BINARY_OP_NE:
        if (isFloatRegister(srcReg1)){
          _genFEQ_S(middleReg, srcReg1, srcReg2);
          _genSEQZ(middleReg, middleReg);
        }
        else {
          _genSUB(middleReg, srcReg1, srcReg2);
          _genSNEZ(middleReg, middleReg);
        }
        break;
      case BINARY_OP_GT:
        if (isFloatRegister(srcReg1)) {
          _genFLT_S(middleReg, srcReg2, srcReg1);
        }
        else {
          _genSLT(middleReg, srcReg2, srcReg1);
        }
        break;
      case BINARY_OP_LT:
        if (isFloatRegister(srcReg1)) {
          _genFLT_S(middleReg, srcReg1, srcReg2);
        }
        else {
          _genSLT(middleReg, srcReg1, srcReg2);
        }
        break;
      case BINARY_OP_AND:
        _genAND(middleReg, srcReg1, srcReg2);
        break;
      case BINARY_OP_OR:
        _genOR(middleReg, srcReg1, srcReg2);
        break;
      default:
        assert(false);
  }
  // convert type if needed
  switch (dstType) {
    case INT_TYPE:
      dstReg = middleReg;
      break;
    case FLOAT_TYPE:
      _genFCVT_S_W((dstReg = REG_FT3), middleReg);
      break;
    default:
      assert(false);
  }
  // store
  _genStoreToMemoryLocation(dstReg, dstLoc, tmpIntReg);
}


void CodeGeneration::genReturn (const MemoryLocation &value) {
  Register tmpIntReg {REG_T0};
  _genLoadFromMemoryLocation(REG_A0, value, tmpIntReg);
}


void CodeGeneration::genBranchTest (AST *testNode, const LabelInAssembly &branchFalseLabel) {
  Register tmpIntReg {REG_T0};
  Register tmpFloatReg {REG_FT0};
  Register valueReg;
  Register middleReg {REG_T2};
  switch (testNode->dataType.type) {
    case INT_TYPE:
      valueReg = REG_T1;
      break;
    case FLOAT_TYPE:
      valueReg = REG_FT1;
      break;
    default:
      assert(false);
  }
  _genLoadFromMemoryLocation(valueReg, testNode->place, tmpIntReg);
  _genConvertToBool(middleReg, valueReg, tmpFloatReg);
  _genBEQZ(middleReg, branchFalseLabel);
}


void CodeGeneration::_genADD (const Register &rd, const Register &rs1, const Register &rs2) {
  assert(isFloatRegister(rd) == isFloatRegister(rs1) && isFloatRegister(rs1) == isFloatRegister(rs2));
  if (isFloatRegister(rd))
    ofs << "  fadd.s " << rd << ", " << rs1 << ", " << rs2 << std::endl;
  else
    ofs << "  add " << rd << ", " << rs1 << ", " << rs2 << std::endl;
}


void CodeGeneration::_genADDI (const Register &rd, const Register &rs1, int imm) {
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
  ofs << "  beqz " << rs1 << ", " << offset << std::endl;
}


void CodeGeneration::_genJ (const LabelInAssembly &offset) {
  ofs << "  j " << offset << std::endl;
}


void CodeGeneration::_genLWorFLW (const Register &rd, int imm, const Register &rs1) {
  assert(!isFloatRegister(rs1));
  assert(rd != REG_FP && rd != REG_SP);
  if (isFloatRegister(rd))
    ofs << "  flw " << rd << ", " << imm << "(" << rs1 << ")" << std::endl;
  else
    ofs << "  lw " << rd << ", " << imm << "(" << rs1 << ")" << std::endl;
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
  assert(!isFloatRegister(rd));
  assert(rd != REG_FP && rd != REG_SP);
  ofs << "  lw " << rd << ", " << symbol << std::endl;
}


void CodeGeneration::_genLD (const Register &rd, int imm, const Register &rs1) {
  ofs << "  ld " << rd << ", " << imm << "(" << rs1 << ")" << std::endl;
}


void CodeGeneration::_genLoadFromMemoryLocation (const Register &rd, const MemoryLocation &memLoc, const Register &rt) {
  if (std::holds_alternative<LabelInAssembly>(memLoc.value))
    _genLWorFLW(rd, std::get<LabelInAssembly>(memLoc.value), rt);
  else {
    if (memLoc.isAbsoluteMemoryAddress) {
      _genLD(rt, std::get<StackMemoryOffset>(memLoc.value), REG_FP);
      _genLWorFLW(rd, 0, rt);
    }
    else
      _genLWorFLW(rd, std::get<StackMemoryOffset>(memLoc.value), REG_FP);
  }
}


void CodeGeneration::_genSWorFSW (const Register &rd, int imm, const Register &rs2) {
  assert(!isFloatRegister(rs2));
  assert(rd != REG_FP && rd != REG_SP);
  if (isFloatRegister(rd))
    ofs << "  fsw " << rd << ", " << imm << "(" << rs2 << ")" << std::endl;
  else
    ofs << "  sw " << rd << ", " << imm << "(" << rs2 << ")" << std::endl;
}


void CodeGeneration::_genSWorFSW (const Register &rd, const LabelInAssembly &symbol, const Register &rt) {
  assert(!isFloatRegister(rt));
  assert(rd != REG_FP && rd != REG_SP);
  if (isFloatRegister(rd))
    ofs << "  fsw " << rd << ", " << symbol << ", " << rt << std::endl;
  else
    ofs << "  sw " << rd << ", " << symbol << ", " << rt << std::endl;
}


void CodeGeneration::_genSD (const Register &rd, int imm, const Register &rs1) {
  ofs << "  sd " << rd << ", " << imm << "(" << rs1 << ")" << std::endl;
}


void CodeGeneration::_genStoreToMemoryLocation (const Register &rd, const MemoryLocation &memLoc, const Register &rt) {
  if (std::holds_alternative<LabelInAssembly>(memLoc.value))
    _genSWorFSW(rd, std::get<LabelInAssembly>(memLoc.value), rt);
  else {
    if (memLoc.isAbsoluteMemoryAddress) {
      _genLD(rt, std::get<StackMemoryOffset>(memLoc.value), REG_FP);
      _genSWorFSW(rd, 0, rt);
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
  // TODO: use register manager to avoid overwriting the temp register
  Register tmpIntReg {REG_T6};
  _genLI(tmpIntReg, float2intMemoryRepresent(imm));
  _genFMV_W_X(rd, tmpIntReg);
}


void CodeGeneration::_genMV (const Register &rd, const Register &rs1) {
  assert(isFloatRegister(rd) == isFloatRegister(rs1));
  if (isFloatRegister(rd)) {
    // TODO: use register manager to avoid overwriting the temp register
    Register tmpFloatReg {REG_FT11};
    _genFMV_W_X(REG_FT11, REG_ZERO);
    ofs << "  fadd.s " << rd << ", " << rs1 << ", " << tmpFloatReg << std::endl;
  }
  else
    ofs << "  mv " << rd << ", " << rs1 << std::endl;
}


void CodeGeneration::_genFCVT_W_S (const Register &rd, const Register &rs1) {
  ofs << "  fcvt.w.s " << rd << ", " << rs1 << std::endl;
}


void CodeGeneration::_genFCVT_S_W (const Register &rd, const Register &rs1) {
  ofs << "  fcvt.s.w " << rd << ", " << rs1 << std::endl;
}


void CodeGeneration::_genFMV_W_X (const Register &rd, const Register &rs1) {
  ofs << "  fmv.w.x " << rd << ", " << rs1 << std::endl;
}


void CodeGeneration::_genConvertToBool (const Register &rd, const Register &rs1) {
  assert(!isFloatRegister(rd) && !isFloatRegister(rs1));
  _genSNEZ(rd, rs1);
}


void CodeGeneration::_genConvertToBool (const Register &rd, const Register &rs1, const Register &tmpFloatReg) {
  assert(isFloatRegister(tmpFloatReg) && !isFloatRegister(rd));
  if (isFloatRegister(rs1)) {
    _genLoadFloatImm(tmpFloatReg, 0.f);
    _genFEQ_S(rd, rs1, tmpFloatReg);
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
