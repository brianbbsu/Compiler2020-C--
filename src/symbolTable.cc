#include "symbolTable.hh"

#include <cassert>

SymbolTable::SymbolTable() : currentLevel(-1) { resetSymbolTable(); }

void SymbolTable::resetSymbolTable() {
  while (currentLevel >= 0) closeScope();
  openScope();  // open global scope
  // add builtin type and functions
  addTypeSymbol("int", TypeDescriptor{SCALAR_TYPE_DESCRIPTOR, INT_TYPE});
  addTypeSymbol("float", TypeDescriptor{SCALAR_TYPE_DESCRIPTOR, FLOAT_TYPE});
  addTypeSymbol("void", TypeDescriptor{SCALAR_TYPE_DESCRIPTOR, VOID_TYPE});
  addFunctionSymbol("read", FunctionSignature{INT_TYPE, {}});
  addFunctionSymbol("fread", FunctionSignature{FLOAT_TYPE, {}});
  // TODO: how to handle write?
}

void SymbolTable::openScope() {
  currentLevel += 1;
  scopeModifiedStack.emplace();
}

void SymbolTable::closeScope() {
  assert(currentLevel >= 0);
  currentLevel -= 1;
  for (const auto &ptr : scopeModifiedStack.top()) {
    delete ptr->top();
    ptr->pop();
  }
  scopeModifiedStack.pop();
}

bool SymbolTable::declaredLocally(const std::string &name) {
  auto ite = table.find(name);
  return ite != table.end() && ite->second.size() &&
         ite->second.top()->level == currentLevel;
}

SymbolTableEntry *SymbolTable::getSymbol(const std::string &name) {
  auto ite = table.find(name);
  if (ite == table.end() || !ite->second.size())
    return nullptr;
  else
    return ite->second.top();
}

SymbolTableEntry *SymbolTable::_addSymbol(const std::string &name,
                                          SymbolTableEntry *entry) {
  auto &stk = table[name];
  // The name should not be declared in this scope
  assert(!stk.size() || stk.top()->level < entry->level);
  stk.push(entry);
  scopeModifiedStack.top().push_back(&stk);
  return entry;
}

SymbolTableEntry *SymbolTable::addVariableSymbol(const std::string &name,
                                                 TypeDescriptor type) {
  SymbolTableEntry *entry =
      new SymbolTableEntry{currentLevel, VARIABLE_SYMBOL, std::move(type)};
  return _addSymbol(name, entry);
}

SymbolTableEntry *SymbolTable::addTypeSymbol(const std::string &name,
                                             TypeDescriptor type) {
  SymbolTableEntry *entry =
      new SymbolTableEntry{currentLevel, TYPE_SYMBOL, std::move(type)};
  return _addSymbol(name, entry);
}

SymbolTableEntry *SymbolTable::addFunctionSymbol(const std::string &name,
                                                 FunctionSignature signature) {
  SymbolTableEntry *entry =
      new SymbolTableEntry{currentLevel, TYPE_SYMBOL, std::move(signature)};
  return _addSymbol(name, entry);
}