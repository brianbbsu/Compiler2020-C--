#ifndef __SYMBOL_TABLE_HH__
#define __SYMBOL_TABLE_HH__

#include <stack>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "header.hh"

enum SymbolKind { VARIABLE_SYMBOL, TYPE_SYMBOL, FUNCTION_SYMBOL, ENUMERATOR_SYMBOL };

struct FunctionSignature {
  DATA_TYPE returnType;
  std::vector<TypeDescriptor> parameters;
  bool hasDefinition;
};

struct SymbolTableEntry {
  int level;
  SymbolKind symbolKind;
  // TypeDescriptor for variable and type, FunctionSignature for function, int for enum const
  std::variant<TypeDescriptor, FunctionSignature, int> attribute;
};

class SymbolTable {
  // use vector for stack
  template <typename T>
  using vstack = std::stack<T, std::vector<T>>;
  using entryStack = vstack<SymbolTableEntry *>;

  int currentLevel;
  std::unordered_map<std::string, entryStack> table;
  vstack<std::vector<entryStack *>> scopeModifiedStack;

  bool hasStash;
  std::vector<std::pair<entryStack *, SymbolTableEntry *>> rewindBuffer;

  SymbolTableEntry *_addSymbol(const std::string &name, SymbolTableEntry *entry);

 public:
  SymbolTable();
  void resetSymbolTable();
  void openScope();
  void closeScope();

  void stashScope();
  void popStash();
  void dropStash();

  bool isGlobalScope();
  bool declaredLocally(const std::string &name);
  SymbolTableEntry *getSymbol(const std::string &name);
  SymbolTableEntry *addVariableSymbol(const std::string &name, TypeDescriptor type);
  SymbolTableEntry *addTypeSymbol(const std::string &name, TypeDescriptor type);
  SymbolTableEntry *addFunctionSymbol(const std::string &name, FunctionSignature signature);
  SymbolTableEntry *addEnumeratorSymbol(const std::string &name, int value);
};

#endif  // ! __SYMBOL_TABLE_HH__