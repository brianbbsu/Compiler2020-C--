#include <cassert>
#include "resourceAllocation.hh"



StackMemoryManager::StackMemoryManager () : isInProcedure{false}, isInParameter{false}, insertOffset{insertOffsetInitValue}, parameterOffset{parameterOffsetInitValue} {
}


StackMemoryManager::~StackMemoryManager () {}


StackMemoryOffset StackMemoryManager::getVariableMemory (size_t size) {
  assert(isInProcedure);
  insertOffset -= size;
  return insertOffset;
}


StackMemoryOffset StackMemoryManager::getParameterMemory (size_t size) {
  assert(isInParameter);
  StackMemoryOffset retval {parameterOffset};
  parameterOffset += size;
  return retval;
}


size_t StackMemoryManager::getProcedureMemoryConsumption () {
  return static_cast<size_t>(-insertOffset) + 8 * CALLEE_SAVE_REGISTERS.size();
}


size_t StackMemoryManager::getParameterMemoryConsumption () {
  return static_cast<size_t>(parameterOffset - parameterOffsetInitValue);
}


void StackMemoryManager::enterProcedure () {
  assert(!isInProcedure);
  isInProcedure = true;
  insertOffset = insertOffsetInitValue;
}


void StackMemoryManager::leaveProcedure () {
  assert(isInProcedure);
  isInProcedure = false;
}


void StackMemoryManager::enterParameterDeclaration () {
  assert(!isInParameter);
  isInParameter = true;
  parameterOffset = parameterOffsetInitValue;
}


void StackMemoryManager::leaveParameterDeclaration () {
  assert(isInParameter);
  isInParameter = false;
}


RegisterManager::RegisterManager () {
}


RegisterManager::~RegisterManager () {}


Register RegisterManager::getRegister () {
  std::cerr << "RegisterManager::getRegister not implemented yet" << std::endl;
  return REG_X0;
}


void RegisterManager::freeRegister (Register reg) {
  std::cerr << "RegisterManager::freeRegister not implemented yet" << std::endl;
}
