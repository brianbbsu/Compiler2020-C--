#ifndef __CODEGEN_HH__
#define __CODEGEN_HH__


#include "header.hh"
#include "symbolTable.hh"
#include "resourceAllocation.hh"


class CodeGeneration {
private:
  std::string filename;
  AST *prog;
  std::ofstream ofs;

  SymbolTable symtab;
  StackMemoryManager stackMemManager;
  AssemblySection currentSection;

  static TypeDescriptor combineTypeAndDecl (const TypeDescriptor &, AST *);
  static size_t getTypeSize (const TypeDescriptor &);
  static LabelInAssembly makeGlobalVarLabel (const std::string &);
  static LabelInAssembly makeFuncLabel (const std::string &);
  static LabelInAssembly makeFrameSizeLabel (const LabelInAssembly &);
  static LabelInAssembly makeConstStringLabel ();
  static Const getConstValue (AST *);
  static std::vector<TypeDescriptor> getParameterDeclarationList (AST *);
  static int32_t float2intMemoryRepresent (float);
  static bool isFloatRegister (const Register &);

  void visitProgramNode (AST *);
  void visitVariableDeclarationList (AST *);
  void visitFunctionDefinition (AST *);
  void visitVariableDeclaration (AST *);
  void visitTypeDeclaration (AST *);
  void visitEnumNode (AST *);
  void visitFunctionDeclaration (AST *);
  void visitExpressionComponent (AST *);
  void visitExpression (AST *);
  void visitFunctionCallStatement (AST *);
  void visitAssignmentStatement (AST *);
  void visitConstNode (AST *);
  void visitIdentifierRValue (AST *);
  void visitIdentifierLValue (AST *);
  void visitBlock (AST *);
  void visitStatement (AST *);
  void visitIfStatement (AST *);
  void visitForStatement (AST *);
  void visitWhileStatement (AST *);
  void visitReturnStatement (AST *);

  void genCallerSaveRegisters ();
  void genCallerRestoreRegisters ();
  void genFunctionPrologue (const LabelInAssembly &);
  void genFunctionEpilogue (const LabelInAssembly &, size_t);
  void genCallFunction (const LabelInAssembly &);
  void genPassParametersBeforeFunctionCall (const AST *);
  void genClearParametersOnStackAfterFunctionCall (const AST *);
  void genSaveReturnValue (const MemoryLocation &, bool, bool);
  void genInitGlobalVarArray (const LabelInAssembly &, size_t);
  void genInitGlobalVarScalar (const LabelInAssembly &, const Const &);
  void genConstString (const LabelInAssembly &, const std::string &);
  void genAssignExpr (const MemoryLocation &, const MemoryLocation &, const DATA_TYPE &, const DATA_TYPE &);
  void genAssignConst (const MemoryLocation &, const Const &, const DATA_TYPE &);
  void genLoadFromMemoryLocation (const Register &, const MemoryLocation &, const Register &);
  void genStoreToMemoryLocation (const Register &, const MemoryLocation &, const Register &);
  void genLogicalNegation (const MemoryLocation &, const MemoryLocation &, const DATA_TYPE &);
  void genUnaryNegative (const MemoryLocation &, const MemoryLocation &, const DATA_TYPE &);
  void genArithmeticOperation (const BINARY_OPERATOR &, const MemoryLocation &, const MemoryLocation &, const MemoryLocation &, const DATA_TYPE &, const DATA_TYPE &, const DATA_TYPE &);
  void genLogicalOperation (const BINARY_OPERATOR &, const MemoryLocation &, const MemoryLocation &, const MemoryLocation &, const DATA_TYPE &, const DATA_TYPE &, const DATA_TYPE &);
  void genReturn (const MemoryLocation &);

  void _genADD (const Register &, const Register &, const Register &);
  void _genADDI (const Register &, const Register &, int);
  void _genSUB (const Register &, const Register &, const Register &);
  void _genMULI (const Register &, const Register &, int);
  void _genLWorFLW (const Register &, int, const Register &);
  void _genLWorFLW (const Register &, const LabelInAssembly &, const Register &);
  void _genLWorFLW (const Register &, const LabelInAssembly &);
  void _genSWorFSW (const Register &, int, const Register &);
  void _genSWorFSW (const Register &, const LabelInAssembly &, const Register &);
  void _genLA (const Register &, const LabelInAssembly &);
  void _genLI (const Register &, int);
  void _genLoadFloatImm (const Register &, float);
  void _genMV (const Register &, const Register &);
  void _genFCVT_W_S (const Register &, const Register &); // convert float to int
  void _genFCVT_S_W (const Register &, const Register &); // convert int to float
  void _genFMV_W_X (const Register &, const Register &);
  void _genCALL (const LabelInAssembly &);
  void _genRET ();

public:
  CodeGeneration (AST *, const std::string &);
  ~CodeGeneration ();

  void run ();
};


#endif // __CODEGEN_HH__
