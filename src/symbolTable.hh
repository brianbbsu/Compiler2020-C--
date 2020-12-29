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
  std::string name;
  SymbolKind symbolKind;
  // TypeDescriptor for variable and type, FunctionSignature for function, int for enum const
  std::variant<TypeDescriptor, FunctionSignature, int> attribute;
  // MemoryLocation only used in code generation phase
  MemoryLocation place;
  SymbolTableEntry (int level_, std::string name_, SymbolKind symbolKind_, std::variant<TypeDescriptor, FunctionSignature, int> attribute_) : level(level_), name(std::move(name_)), symbolKind(symbolKind_), attribute(attribute_) {}
  SymbolTableEntry (int level_, std::string name_, SymbolKind symbolKind_, std::variant<TypeDescriptor, FunctionSignature, int> attribute_, MemoryLocation place_) : level(level_), name(std::move(name_)), symbolKind(symbolKind_), attribute(attribute_), place(place_) {}
};

class SymbolTable {
  // use vector for stack
  template <typename T>
  using vstack = std::stack<T, std::vector<T>>;
  using entryStack = vstack<SymbolTableEntry *>;

  int currentLevel;
  SymbolTableEntry *currentFunctionEntry;
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

  void enterFunction(SymbolTableEntry *functionEntry);
  SymbolTableEntry *getCurrentFunction();
  void leaveFunction();

  std::vector<std::string> getFunctionsWithNoDefinition();

  bool isGlobalScope();
  bool declaredLocally(const std::string &name);
  SymbolTableEntry *getSymbol(const std::string &name);
  SymbolTableEntry *addVariableSymbol(const std::string &name, TypeDescriptor type);
  SymbolTableEntry *addVariableSymbol(const std::string &name, TypeDescriptor type, const MemoryLocation &place);
  SymbolTableEntry *addTypeSymbol(const std::string &name, TypeDescriptor type);
  SymbolTableEntry *addFunctionSymbol(const std::string &name, FunctionSignature signature);
  SymbolTableEntry *addFunctionSymbol(const std::string &name, FunctionSignature signature, const MemoryLocation &place);
  SymbolTableEntry *addEnumeratorSymbol(const std::string &name, int value);
};

#endif  // ! __SYMBOL_TABLE_HH__
