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
  if (funcName == "MAIN") return "_start_MAIN";
  return "f_" + funcName;
}


inline LabelInAssembly CodeGeneration::makeFrameSizeLabel (const LabelInAssembly &funcLabel) {
  return "frame_size_" + funcLabel;
}


inline LabelInAssembly CodeGeneration::makeConstStringLabel () {
  static int counter {0};
  return "s_" + std::to_string(counter++);
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


inline std::vector<TypeDescriptor> getParameterDeclarationList (AST *paramListNode) {
  return std::vector<TypeDescriptor>{};
  // TODO
  // std::vector<TypeDescriptor> paramDeclList;

  // for (const auto &childNode : paramListNode->children) {
  //   AST *typeNode {childNode->children[0]};
  //   AST *nameIDNode {childNode->children[1]};

  // }
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


void CodeGeneration::visitTypeDeclaration (AST *) {
  std::cerr << "CodeGeneration::visitTypeDeclaration not implemented yet" << std::endl;
}


void CodeGeneration::visitEnumNode (AST *) {
  std::cerr << "CodeGeneration::visitEnumNode not implemented yet" << std::endl;
}


void CodeGeneration::visitVariableDeclaration (AST *varDeclNode) {
  const TypeDescriptor &declTypeDesc {varDeclNode->children[0]->dataType};

  for (size_t idx = 1; idx < varDeclNode->children.size(); ++idx) {
    AST *varIDNode {varDeclNode->children[idx]};
    const TypeDescriptor varTypeDesc {combineTypeAndDecl(declTypeDesc, varIDNode)};
    size_t varSize {getTypeSize(varTypeDesc)};
    MemoryLocation varPlace;

    const IdentifierSemanticValue &varSemanticValue {std::get<IdentifierSemanticValue>(varIDNode->semanticValue)};
    if (symtab.isGlobalScope()) {
      LabelInAssembly label {makeGlobalVarLabel(varSemanticValue.identifierName)};
      varPlace = label;

      if (varSemanticValue.kind == ARRAY_ID) {
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
      varPlace = stackOffset;

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
      visitIdentifierRValue(expressionComponent);
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
    funcLabel = std::get<LabelInAssembly>(funcEntry->place);
  }

  // prepare all parameters
  for (const auto &paramNode : paramListNode->children) {
    visitExpressionComponent(paramNode);
  }

  genCallerSaveRegisters();
  // TODO: pass argument (on register and on stack (if needed))
  genPassParametersBeforeFunctionCall(paramListNode);
  genCallFunction(funcLabel);
  genSaveReturnValue(stmtNode->place, (funcName == "fread"), (stmtNode->dataType.type == VOID_TYPE));
  genClearParametersOnStackAfterFunctionCall(paramListNode);
  // TODO: clear argument (on stack only)
  genCallerRestoreRegisters();
}


void CodeGeneration::visitAssignmentStatement (AST *stmtNode) {
  AST *LHSIDNode {stmtNode->children[0]};
  AST *RHSExprNode {stmtNode->children[1]};

  visitIdentifierLValue(LHSIDNode);
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


void CodeGeneration::visitIdentifierRValue (AST *idNode) {
  const auto &idSemanticValue {std::get<IdentifierSemanticValue>(idNode->semanticValue)};
  SymbolTableEntry *idEntry {symtab.getSymbol(idSemanticValue.identifierName)};

  if (idEntry->symbolKind == ENUMERATOR_SYMBOL) {
    // whether idEntry has attribute "place" depends on implementation
    std::cerr << "var_ref of rvalue does not support enumeration currently" << std::endl;
    return;
  }

  if (idSemanticValue.kind == NORMAL_ID) {
    idNode->place = idEntry->place;
    return;
  }

  // array dereference
  Register addrReg {REG_T0};
  Register offsetReg {REG_T1};
  Register resultReg {REG_T2};
  Register tmpReg {REG_T3};
  _genLI(offsetReg, 0);
  const ArrayProperties &arrProp {std::get<TypeDescriptor>(idEntry->attribute).arrayProperties};
  for (size_t idx {0}; idx < idNode->children.size(); ++idx) {
    // multiplied by the size of current dimension
    _genMULI(offsetReg, offsetReg, arrProp.dimensions[idx]);

    // add by the value of current index
    const auto &dimComponentNode {idNode->children[idx]};
    visitExpressionComponent(dimComponentNode);
    genLoadFromMemoryLocation(tmpReg, dimComponentNode->place, addrReg);
    _genADD(offsetReg, offsetReg, tmpReg);
  }
  _genMULI(offsetReg, offsetReg, getTypeSize(arrProp.elementType));
  _genLA(resultReg, std::get<LabelInAssembly>(idEntry->place));
  _genADD(resultReg, resultReg, offsetReg);

  if (idNode->dataType.type == ARR_TYPE) {
    StackMemoryOffset offset {stackMemManager.getMemory(SIZEOF_REGISTER)};
    idNode->place = offset;
    _genSWorFSW(resultReg, offset, REG_FP);
  }
  else {
    StackMemoryOffset offset {stackMemManager.getMemory(getTypeSize(idNode->dataType.type))};
    idNode->place = offset;
    _genLWorFLW(tmpReg, 0, resultReg);
    _genSWorFSW(tmpReg, offset, REG_FP);
  }
}


void CodeGeneration::visitIdentifierLValue (AST *) {
  std::cerr << "CodeGeneration::visitIdentifierLValue not implemented yet" << std::endl;
}


void CodeGeneration::visitFunctionDeclaration ([[maybe_unused]] AST *declNode) {
  // no code needed to be generated in declaration
}


void CodeGeneration::visitFunctionDefinition (AST *defiNode) {
  AST *returnTypeNode {defiNode->children[0]};
  AST *funcNameIDNode {defiNode->children[1]};
  AST *paramListNode {defiNode->children[2]};
  AST *bodyBlockNode {defiNode->children[3]};

  const TypeDescriptor &returnTypeDesc {returnTypeNode->dataType};
  const std::string &funcName {std::get<IdentifierSemanticValue>(funcNameIDNode->semanticValue).identifierName};
  LabelInAssembly funcLabel {makeFuncLabel(funcName)};

  symtab.openScope();
  // TODO: handle parameter list
  // const std::vector<TypeDescriptor> &params {getParameterDeclarationList(paramListNode)};
  // SymbolTableEntry *funcEntry {symtab.addFunctionSymbol(funcName, {returnTypeDesc.type, params, true}, funcLabel)};
  SymbolTableEntry *funcEntry {symtab.addFunctionSymbol(funcName, {returnTypeDesc.type, {}, true}, funcLabel)};
  symtab.enterFunction(funcEntry);
  stackMemManager.enterProcedure();
  genFunctionPrologue(funcLabel);
  visitBlock(bodyBlockNode);
  genFunctionEpilogue(funcLabel, stackMemManager.getProcedureMemoryConsumption());
  stackMemManager.leaveProcedure();
  symtab.leaveFunction();
  symtab.closeScope();
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


void CodeGeneration::visitIfStatement (AST *) {
  std::cerr << "CodeGeneration::visitIfStatement not yet implemented" << std::endl;
}


void CodeGeneration::visitForStatement (AST *) {
  std::cerr << "CodeGeneration::visitForStatement not yet implemented" << std::endl;
}


void CodeGeneration::visitWhileStatement (AST *) {
  std::cerr << "CodeGeneration::visitWhileStatement not yet implemented" << std::endl;
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
    _genLA(REG_A0, std::get<LabelInAssembly>(childNode->place));
  }
  else if (childNode->dataType.type == FLOAT_TYPE) {
    Register tmpReg {REG_T0};
    genLoadFromMemoryLocation(REG_FA0, childNode->place, tmpReg);
  }
  else {
    Register tmpReg {REG_T0};
    genLoadFromMemoryLocation(REG_A0, childNode->place, tmpReg);
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
  _genSWorFSW(REG_RA, 0, REG_SP);
  _genSWorFSW(REG_FP, -8, REG_SP);
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
  _genLWorFLW(REG_FP, 0, REG_FP);
  _genLWorFLW(REG_RA, 0, REG_SP);
  _genRET();

  // step 3: generate frame size
  ofs << "  " << ASSEMBLY_DATA_SECTION << std::endl
      << makeFrameSizeLabel(funcLabel) << ": .word " << (frameSize + 8 * CALLEE_SAVE_REGISTERS.size()) << std::endl
      << "  " << ASSEMBLY_TEXT_SECTION << std::endl;
}


void CodeGeneration::genCallFunction (const LabelInAssembly &funcLabel) {
  _genADDI(REG_SP, REG_SP, -SIZEOF_REGISTER);
  _genCALL(funcLabel);
}


void CodeGeneration::genSaveReturnValue (const MemoryLocation &place, bool isFread, bool noRetVal) {
  _genADDI(REG_SP, REG_SP, SIZEOF_REGISTER);
  if (noRetVal)
    return;
  // TODO: handle float return value differently ?
  Register addrReg {REG_T0};
  if (isFread)
    genStoreToMemoryLocation(REG_FA0, place, addrReg);
  else
    genStoreToMemoryLocation(REG_A0, place, addrReg);
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
      << strLabel << ": .string " << str << std::endl;
  if (currentSection != ASSEMBLY_DATA_SECTION) {
    ofs << "  " << currentSection << std::endl;
  }
}


void CodeGeneration::genAssignExpr (const MemoryLocation &LHS, const MemoryLocation &RHS, const DATA_TYPE &typeLHS, const DATA_TYPE &typeRHS) {
  Register addrReg {REG_T0};
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

  genLoadFromMemoryLocation(valueRegRHS, RHS, addrReg);

  // move or convert
  if (typeLHS == FLOAT_TYPE && typeRHS == INT_TYPE)
    _genFCVT_S_W(valueRegLHS, valueRegRHS);
  else if (typeLHS == INT_TYPE && typeRHS == FLOAT_TYPE)
    _genFCVT_W_S(valueRegLHS, valueRegRHS);
  else
    _genMV(valueRegLHS, valueRegRHS);

  // store
  genStoreToMemoryLocation(valueRegLHS, LHS, addrReg);
}


void CodeGeneration::genAssignConst (const MemoryLocation &LHS, const Const &value, const DATA_TYPE &typeLHS) {
  Register addrReg {REG_T0};
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

  genStoreToMemoryLocation(valueRegLHS, LHS, addrReg);
}


void CodeGeneration::genLoadFromMemoryLocation (const Register &rd, const MemoryLocation &memLoc, const Register &rt) {
  if (std::holds_alternative<LabelInAssembly>(memLoc))
    _genLWorFLW(rd, std::get<LabelInAssembly>(memLoc), rt);
  else
    _genLWorFLW(rd, std::get<StackMemoryOffset>(memLoc), REG_FP);
}


void CodeGeneration::genStoreToMemoryLocation (const Register &rd, const MemoryLocation &memLoc, const Register &rt) {
  if (std::holds_alternative<LabelInAssembly>(memLoc))
    _genSWorFSW(rd, std::get<LabelInAssembly>(memLoc), rt);
  else
    _genSWorFSW(rd, std::get<StackMemoryOffset>(memLoc), REG_FP);
}


void CodeGeneration::genLogicalNegation (const MemoryLocation &, const MemoryLocation &, const DATA_TYPE &) {
  std::cerr << "CodeGeneration::genLogicalNegation not yet implemented" << std::endl;
}


void CodeGeneration::genUnaryNegative (const MemoryLocation &, const MemoryLocation &, const DATA_TYPE &) {
  std::cerr << "CodeGeneration::genUnaryNegative not yet implemented" << std::endl;
}


void CodeGeneration::genArithmeticOperation (const BINARY_OPERATOR &, const MemoryLocation &, const MemoryLocation &, const MemoryLocation &, const DATA_TYPE &, const DATA_TYPE &, const DATA_TYPE &) {
  std::cerr << "CodeGeneration::genArithmeticOperation not yet implemented" << std::endl;
}


void CodeGeneration::genLogicalOperation (const BINARY_OPERATOR &, const MemoryLocation &, const MemoryLocation &, const MemoryLocation &, const DATA_TYPE &, const DATA_TYPE &, const DATA_TYPE &) {
  std::cerr << "CodeGeneration::genLogicalOperation not yet implemented" << std::endl;
}


void CodeGeneration::genReturn (const MemoryLocation &value) {
  Register addrReg {REG_T0};
  Register valueReg {REG_A0};
  if (std::holds_alternative<LabelInAssembly>(value))
    _genLWorFLW(valueReg, std::get<LabelInAssembly>(value), addrReg);
  else
    _genLWorFLW(valueReg, std::get<StackMemoryOffset>(value), REG_FP);
}


void CodeGeneration::_genADD (const Register &rd, const Register &rs1, const Register &rs2) {
  ofs << "  add " << rd << ", " << rs1 << ", " << rs2 << std::endl;
}


void CodeGeneration::_genADDI (const Register &rd, const Register &rs1, int imm) {
  ofs << "  addi " << rd << ", " << rs1 << ", " << imm << std::endl;
}


void CodeGeneration::_genSUB (const Register &rd, const Register &rs1, const Register &rs2) {
  ofs << "  sub " << rd << ", " << rs1 << ", " << rs2 << std::endl;
}


void CodeGeneration::_genMULI (const Register &rd, const Register &rs1, int imm) {
  ofs << "  li " << rd << ", " << imm << std::endl
      << "  mul " << rd << ", " << rd << ", " << rs1 << std::endl;
}


void CodeGeneration::_genLWorFLW (const Register &rd, int imm, const Register &rs1) {
  assert(!isFloatRegister(rs1));
  if (isFloatRegister(rd))
    ofs << "  flw " << rd << ", " << imm << "(" << rs1 << ")" << std::endl;
  else
    ofs << "  lw " << rd << ", " << imm << "(" << rs1 << ")" << std::endl;
}


void CodeGeneration::_genLWorFLW (const Register &rd, const LabelInAssembly &symbol, const Register &rt) {
  assert(!isFloatRegister(rt));
  if (isFloatRegister(rd))
    ofs << "  flw " << rd << ", " << symbol << ", " << rt << std::endl;
  else
    ofs << "  lw " << rd << ", " << symbol << std::endl;
}


void CodeGeneration::_genLWorFLW (const Register &rd, const LabelInAssembly &symbol) {
  assert(!isFloatRegister(rd));
  ofs << "  lw " << rd << ", " << symbol << std::endl;
}


void CodeGeneration::_genSWorFSW (const Register &rd, int imm, const Register &rs2) {
  assert(!isFloatRegister(rs2));
  if (isFloatRegister(rd))
    ofs << "  fsw " << rd << ", " << imm << "(" << rs2 << ")" << std::endl;
  else
    ofs << "  sw " << rd << ", " << imm << "(" << rs2 << ")" << std::endl;
}


void CodeGeneration::_genSWorFSW (const Register &rd, const LabelInAssembly &symbol, const Register &rt) {
  assert(!isFloatRegister(rt));
  if (isFloatRegister(rd))
    ofs << "  fsw " << rd << ", " << symbol << ", " << rt << std::endl;
  else
    ofs << "  sw " << rd << ", " << symbol << std::endl;
}


void CodeGeneration::_genLA (const Register &rd, const LabelInAssembly &symbol) {
  ofs << "  la " << rd << ", " << symbol << std::endl;
}


void CodeGeneration::_genLI (const Register &rd, int imm) {
  ofs << "  li " << rd << ", " << imm << std::endl;
}


void CodeGeneration::_genLoadFloatImm (const Register &rd, float imm) {
  // TODO: use register manager to avoid overwriting the temp register
  Register tmpReg {REG_T6};
  _genLI(tmpReg, float2intMemoryRepresent(imm));
  _genFMV_W_X(rd, tmpReg);
}


void CodeGeneration::_genMV (const Register &rd, const Register &rs1) {
  assert(isFloatRegister(rd) == isFloatRegister(rs1));
  if (isFloatRegister(rd)) {
    // TODO: use register manager to avoid overwriting the temp register
    Register tmpReg {REG_FT11};
    _genFMV_W_X(REG_FT11, REG_ZERO);
    ofs << "  fadd.s " << rd << ", " << rs1 << ", " << tmpReg << std::endl;
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


void CodeGeneration::_genCALL (const LabelInAssembly &funcLabel) {
  ofs << "  call " << funcLabel << std::endl;
}


void CodeGeneration::_genRET () {
  ofs << "  ret" << std::endl;
}
