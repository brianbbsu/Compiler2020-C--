#include "semanticAnalysis.hh"

#include <cassert>
#include <cstdlib>
#include <iostream>

SemanticAnalysis::SemanticAnalysis(AST *_prog) : prog(_prog), symbolTable() {}

void SemanticAnalysis::runAnalysis() {
  symbolTable.resetSymbolTable();
  anyerror = false;
  processProgramNode(prog);
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
      raiseError("Array ID not implemented");
      if (variableTypeDesc.kind == SCALAR_TYPE_DESCRIPTOR) {
        // Turn scalar type into array type
        variableTypeDesc.kind = ARRAY_TYPE_DESCRIPTOR;
        DATA_TYPE origType = std::get<DATA_TYPE>(variableTypeDesc.description);
        variableTypeDesc.description.emplace<ArrayProperties>(ArrayProperties{origType, {}});
      }
      for (AST *child : variableIDNode->children) {
        // TODO: call processExpressionNode
        // TODO: check type of dimension argument
      }
    }
    // Step 3
    if (variableSemanticValue.kind == WITH_INIT_ID) {
      // TODO: parse initial value and do type check
      // This should not affect symbol table (right?)
      raiseError("ID with initial value not implemented");
    }
    // Step 4
    symbolTable.addVariableSymbol(variableSemanticValue.identifierName,
                                  std::move(variableTypeDesc));
  }
}

void SemanticAnalysis::processFunctionDefinition(AST *declarationNode) {
  assert(declarationNode->nodeType == DECLARATION_NODE);
  assert(std::get<DECLSemanticValue>(declarationNode->semanticValue).kind == FUNCTION_DECL);
  // TODO: Process function definition
}