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


inline LabelInAssembly CodeGeneration::makeFuncLabel (const std::string &funcName) {
  if (funcName == "main") return "_start_MAIN";
  return "f_" + funcName;
}


inline LabelInAssembly CodeGeneration::makeFrameSizeLabel (const LabelInAssembly &funcLabel) {
  return "frame_size_" + funcLabel;
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

  SymbolTableEntry *funcEntry {symtab.getSymbol(std::get<IdentifierSemanticValue>(funcNameIDNode->semanticValue).identifierName)};

  for (const auto &paramNode : paramListNode->children) {
    visitExpressionComponent(paramNode);
  }

  genCallerSaveRegisters();
  // TODO: pass argument
  genCallFunction(std::get<LabelInAssembly>(funcEntry->place));
  genSaveReturnValue(stmtNode->place);
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
  size_t dataSize {getTypeSize(constNode->dataType)};
  constNode->place = stackMemManager.getMemory(dataSize);
  genAssignConst(constNode->place, getConstValue(constNode), constNode->dataType.type);
}


void CodeGeneration::visitIdentifierRValue (AST *) {
  std::cerr << "CodeGeneration::visitIdentifierRValue not implemented yet" << std::endl;
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
  std::cerr << "CodeGeneration::visitStatement not yet implemented" << std::endl;
}


void CodeGeneration::genCallerSaveRegisters () {
  std::cerr << "CodeGeneration::genCallerSaveRegisters not yet implemented" << std::endl;
}


void CodeGeneration::genCallerRestoreRegisters () {
  std::cerr << "CodeGeneration::genCallerRestoreRegisters not yet implemented" << std::endl;
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
  _genADDI(REG_SP, REG_SP, -16);
  _genLA(REG_T0, makeFrameSizeLabel(funcLabel));
  _genLWorFLW(REG_T1, 0, REG_T0);
  _genSUB(REG_SP, REG_SP, REG_T1);

  // step 2: store registers
  for (size_t idx {0}; idx < REGISTER_TO_BE_STORED.size(); ++idx) {
    _genSWorFSW(REGISTER_TO_BE_STORED[idx], 8 * (idx + 1), REG_SP);
  }
}


void CodeGeneration::genFunctionEpilogue (const LabelInAssembly &funcLabel, size_t frameSize) {
  if (currentSection != ASSEMBLY_TEXT_SECTION) {
    ofs << "  " << ASSEMBLY_TEXT_SECTION << std::endl;
    currentSection = ASSEMBLY_TEXT_SECTION;
  }

  // step 1: restore registers
  for (size_t idx {0}; idx < REGISTER_TO_BE_STORED.size(); ++idx) {
    _genLWorFLW(REGISTER_TO_BE_STORED[idx], 8 * (idx + 1), REG_SP);
  }

  // step 2: move fp and sp
  _genMV(REG_SP, REG_FP);
  _genADDI(REG_SP, REG_SP, 8);
  _genLWorFLW(REG_FP, 0, REG_FP);
  _genLWorFLW(REG_RA, 0, REG_SP);
  _genRET();

  // step 3: generate frame size
  ofs << "  " << ASSEMBLY_DATA_SECTION << std::endl
      << makeFrameSizeLabel(funcLabel) << ": .word " << frameSize << std::endl
      << "  " << ASSEMBLY_TEXT_SECTION << std::endl;
}


void CodeGeneration::genCallFunction (const LabelInAssembly &) {
  std::cerr << "CodeGeneration::genCallFunction not yet implemented" << std::endl;
}


void CodeGeneration::genSaveReturnValue (const MemoryLocation &) {
  std::cerr << "CodeGeneration::genSaveReturnValue not yet implemented" << std::endl;
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

  // load
  if (std::holds_alternative<LabelInAssembly>(RHS)) {
    _genLA(addrReg, std::get<LabelInAssembly>(RHS));
    _genLWorFLW(valueRegRHS, 0, addrReg);
  }
  else {
    _genLWorFLW(valueRegRHS, std::get<StackMemoryOffset>(RHS), REG_FP);
  }

  // move or convert
  if (typeLHS == FLOAT_TYPE && typeRHS == INT_TYPE)
    _genFCVT_S_W(valueRegLHS, valueRegRHS);
  else if (typeLHS == INT_TYPE && typeRHS == FLOAT_TYPE)
    _genFCVT_W_S(valueRegLHS, valueRegRHS);
  else
    _genMV(valueRegLHS, valueRegRHS);

  // store
  if (std::holds_alternative<LabelInAssembly>(LHS)) {
    _genLA(addrReg, std::get<LabelInAssembly>(LHS));
    _genSWorFSW(valueRegLHS, 0, addrReg);
  }
  else {
    _genSWorFSW(valueRegLHS, std::get<StackMemoryOffset>(LHS), REG_FP);
  }
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

  // store
  if (std::holds_alternative<LabelInAssembly>(LHS)) {
    _genLA(addrReg, std::get<LabelInAssembly>(LHS));
    _genSWorFSW(valueRegLHS, 0, addrReg);
  }
  else {
    _genSWorFSW(valueRegLHS, std::get<StackMemoryOffset>(LHS), REG_FP);
  }
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


void CodeGeneration::_genADDI (const Register &rd, const Register &rs1, int imm) {
  ofs << "  addi " << rd << ", " << rs1 << ", " << imm << std::endl;
}


void CodeGeneration::_genSUB (const Register &rd, const Register &rs1, const Register &rs2) {
  ofs << "  sub " << rd << ", " << rs1 << ", " << rs2 << std::endl;
}


void CodeGeneration::_genLWorFLW (const Register &rd, int imm, const Register &rs1) {
  if (isFloatRegister(rs1)) std::cerr << rs1 << std::endl;
  assert(!isFloatRegister(rs1));
  if (isFloatRegister(rd))
    ofs << "  flw " << rd << ", " << imm << "(" << rs1 << ")" << std::endl;
  else
    ofs << "  lw " << rd << ", " << imm << "(" << rs1 << ")" << std::endl;
}


void CodeGeneration::_genSWorFSW (const Register &rd, int imm, const Register &rs2) {
  assert(!isFloatRegister(rs2));
  if (isFloatRegister(rd))
    ofs << "  fsw " << rd << ", " << imm << "(" << rs2 << ")" << std::endl;
  else
    ofs << "  sw " << rd << ", " << imm << "(" << rs2 << ")" << std::endl;
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
  // TODO: use register manager to avoid overwriting the temp register
  Register tmpReg {REG_FT11};
  if (isFloatRegister(rd)) {
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


void CodeGeneration::_genRET () {
  ofs << "  ret" << std::endl;
}
