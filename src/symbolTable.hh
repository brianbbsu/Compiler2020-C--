#ifndef __SYMBOL_TABLE_HH__
#define __SYMBOL_TABLE_HH__

#include <stack>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "header.hh"

enum SymbolKind { VARIABLE_SYMBOL, TYPE_SYMBOL, FUNCTION_SYMBOL };

enum TypeDescriptorKind { SCALAR_TYPE_DESCRIPTOR, ARRAY_TYPE_DESCRIPTOR };

struct ArrayProperties {
  DATA_TYPE elementType;
  std::vector<int> dimensions;
};

struct TypeDescriptor {
  TypeDescriptorKind kind;
  // DATA_TYPE for scalar, ArrayProperties for array
  std::variant<DATA_TYPE, ArrayProperties> description;
};

struct FunctionParameter {
  TypeDescriptor type;
  std::string name;
};

struct FunctionSignature {
  DATA_TYPE returnType;
  std::vector<FunctionParameter> parameters;
  // TODO: function forward declaration (declaration vs definition)?
};

struct SymbolTableEntry {
  int level;
  SymbolKind symbolKind;
  // TypeDescriptor for variable and type, FunctionSignature for function
  std::variant<TypeDescriptor, FunctionSignature> attribute;
};

class SymbolTable {
  // use vector for stack
  template <typename T>
  using vstack = std::stack<T, std::vector<T>>;
  using entryStack = vstack<SymbolTableEntry *>;

  int currentLevel;
  std::unordered_map<std::string, entryStack> table;
  vstack<std::vector<entryStack *>> scopeModifiedStack;

  SymbolTableEntry *_addSymbol(const std::string &name, SymbolTableEntry *entry);

 public:
  SymbolTable();
  void resetSymbolTable();
  void openScope();
  void closeScope();
  bool declaredLocally(const std::string &name);
  SymbolTableEntry *getSymbol(const std::string &name);
  SymbolTableEntry *addVariableSymbol(const std::string &name, TypeDescriptor type);
  SymbolTableEntry *addTypeSymbol(const std::string &name, TypeDescriptor type);
  SymbolTableEntry *addFunctionSymbol(const std::string &name, FunctionSignature signature);
};

#endif  // ! __SYMBOL_TABLE_HH__