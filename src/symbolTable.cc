#include "symbolTable.hh"

#include <cassert>

#include "print.hh"

SymbolTable::SymbolTable() : currentLevel(-1), hasStash(false) {
  resetSymbolTable();
}

void SymbolTable::resetSymbolTable() {
  while (currentLevel >= 0) closeScope();
  if (hasStash) dropStash();
  currentFunctionEntry = nullptr;
  openScope();  // open global scope
  // add builtin type and functions
  addTypeSymbol("int", TypeDescriptor(INT_TYPE));
  addTypeSymbol("float", TypeDescriptor(FLOAT_TYPE));
  addTypeSymbol("void", TypeDescriptor(VOID_TYPE));
  addFunctionSymbol("read", FunctionSignature{INT_TYPE, {}, true}, LabelInAssembly{"_read_int"});
  addFunctionSymbol("fread", FunctionSignature{FLOAT_TYPE, {}, true}, LabelInAssembly{"_read_float"});
  addFunctionSymbol("write", FunctionSignature{VOID_TYPE, {WRITE_PARAMETER_TYPE}, true});
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

void SymbolTable::stashScope() {
  assert(currentLevel > 0);
  assert(!hasStash);
  for (entryStack *stackPtr : scopeModifiedStack.top()) {
    rewindBuffer.push_back({stackPtr, stackPtr->top()});
    stackPtr->pop();
  }
  scopeModifiedStack.pop();
  currentLevel--;
  hasStash = true;
}

void SymbolTable::popStash() {
  assert(hasStash);
  hasStash = false;
  currentLevel++;
  scopeModifiedStack.emplace();
  for (auto [stackPtr, entry] : rewindBuffer) {
    stackPtr->push(entry);
    scopeModifiedStack.top().push_back(stackPtr);
  }
  rewindBuffer.clear();
}

void SymbolTable::dropStash() {
  assert(hasStash);
  hasStash = false;
  for (auto [stackPtr, entry] : rewindBuffer) {
    delete entry;
  }
  rewindBuffer.clear();
}

bool SymbolTable::declaredLocally(const std::string &name) {
  auto ite = table.find(name);
  return ite != table.end() && ite->second.size() && ite->second.top()->level == currentLevel;
}

void SymbolTable::enterFunction(SymbolTableEntry *functionEntry) {
  assert(currentFunctionEntry == nullptr);
  currentFunctionEntry = functionEntry;
}

SymbolTableEntry *SymbolTable::getCurrentFunction() {
  assert(currentFunctionEntry != nullptr);
  return currentFunctionEntry;
}

void SymbolTable::leaveFunction() {
  assert(currentFunctionEntry != nullptr);
  currentFunctionEntry = nullptr;
}

bool SymbolTable::isGlobalScope() { return currentLevel == 0; }

SymbolTableEntry *SymbolTable::getSymbol(const std::string &name) {
  auto ite = table.find(name);
  if (ite == table.end() || !ite->second.size())
    return nullptr;
  else
    return ite->second.top();
}

SymbolTableEntry *SymbolTable::_addSymbol(const std::string &name, SymbolTableEntry *entry) {
  auto &stk = table[name];
  // The name should not be declared in this scope
  assert(!stk.size() || stk.top()->level < entry->level);
  stk.push(entry);
  scopeModifiedStack.top().push_back(&stk);
  return entry;
}

SymbolTableEntry *SymbolTable::addVariableSymbol(const std::string &name, TypeDescriptor type) {
  SymbolTableEntry *entry = new SymbolTableEntry{currentLevel, VARIABLE_SYMBOL, std::move(type)};
  return _addSymbol(name, entry);
}

SymbolTableEntry *SymbolTable::addVariableSymbol(const std::string &name, TypeDescriptor type, const MemoryLocation &place) {
  SymbolTableEntry *entry = new SymbolTableEntry{currentLevel, VARIABLE_SYMBOL, std::move(type), place};
  return _addSymbol(name, entry);
}

SymbolTableEntry *SymbolTable::addTypeSymbol(const std::string &name, TypeDescriptor type) {
  SymbolTableEntry *entry = new SymbolTableEntry{currentLevel, TYPE_SYMBOL, std::move(type)};
  return _addSymbol(name, entry);
}

SymbolTableEntry *SymbolTable::addFunctionSymbol(const std::string &name,
                                                 FunctionSignature signature) {
  SymbolTableEntry *entry =
      new SymbolTableEntry{currentLevel, FUNCTION_SYMBOL, std::move(signature)};
  return _addSymbol(name, entry);
}

SymbolTableEntry *SymbolTable::addFunctionSymbol(const std::string &name,
                                                 FunctionSignature signature,
                                                 const MemoryLocation &place) {
  SymbolTableEntry *entry =
      new SymbolTableEntry{currentLevel, FUNCTION_SYMBOL, std::move(signature), place};
  return _addSymbol(name, entry);
}

SymbolTableEntry *SymbolTable::addEnumeratorSymbol(const std::string &name, int value) {
  SymbolTableEntry *entry = new SymbolTableEntry{currentLevel, ENUMERATOR_SYMBOL, value};
  return _addSymbol(name, entry);
}
