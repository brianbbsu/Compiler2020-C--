#include <cassert>
#include "resourceAllocation.hh"



StackMemoryManager::StackMemoryManager () : isInProcedure{false}, insertOffset{0} {
}


StackMemoryManager::~StackMemoryManager () {}


StackMemoryOffset StackMemoryManager::getMemory (size_t size) {
  assert(isInProcedure);
  insertOffset -= size;
  return insertOffset;
}


size_t StackMemoryManager::getProcedureMemoryConsumption () {
  return static_cast<size_t>(-insertOffset) + 8 * REGISTER_TO_BE_STORED.size();
}


void StackMemoryManager::enterProcedure () {
  assert(!isInProcedure);
  isInProcedure = true;
  insertOffset = 0;
}


void StackMemoryManager::leaveProcedure () {
  assert(isInProcedure);
  isInProcedure = false;
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
