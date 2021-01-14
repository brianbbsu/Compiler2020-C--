#include <cassert>
#include "resourceAllocation.hh"


AllocatedRegister::~AllocatedRegister () {
  manager->freeRegister(*this);
}


std::ostream &operator<< (std::ostream &os, const AllocatedRegister &reg) {
  return os << reg.reg;
}


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


// the memory of stored registers is not included here
size_t StackMemoryManager::getProcedureMemoryConsumption () {
  return static_cast<size_t>(-insertOffset);
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
  for (const Register &reg : CALLER_SAVE_INT_REGISTERS)
    tempIntRegisters.emplace(reg, RegisterPoolEntry{reg, false});

  for (const Register &reg : CALLEE_SAVE_INT_REGISTERS)
    saveIntRegisters.emplace(reg, RegisterPoolEntry{reg, false});

  for (const Register &reg : CALLER_SAVE_FLOAT_REGISTERS)
    tempFloatRegisters.emplace(reg, RegisterPoolEntry{reg, false});

  for (const Register &reg : CALLEE_SAVE_FLOAT_REGISTERS)
    saveFloatRegisters.emplace(reg, RegisterPoolEntry{reg, false});
}


RegisterManager::~RegisterManager () {}


void RegisterManager::enterProcedure () {
  currProcedureUsedRegisters.clear();
}


void RegisterManager::leaveProcedure () {
  currProcedureUsedRegisters.clear();
}


AllocatedRegister_Returned RegisterManager::_getRegisterFromPool (RegisterPool &pool, const std::string &debugMsg) {
  for (auto & [reg, entry] : pool) {
    if (!entry.inUse) {
      entry.inUse = true;
      entry.debugMsg = debugMsg;
      currProcedureUsedRegisters.emplace(reg);
      return {entry.reg, this};
    }
  }

  std::cerr << "RegisterManager::_getRegisterFromPool : no available register (this should not happen !!!)" << std::endl;
  assert(false);
}


AllocatedRegister_Returned RegisterManager::getSaveIntRegister (const std::string &debugMsg) {
  return _getRegisterFromPool(saveIntRegisters, debugMsg);
}


AllocatedRegister_Returned RegisterManager::getTempIntRegister (const std::string &debugMsg) {
  return _getRegisterFromPool(tempIntRegisters, debugMsg);
}


AllocatedRegister_Returned RegisterManager::getSaveFloatRegister (const std::string &debugMsg) {
  return _getRegisterFromPool(saveFloatRegisters, debugMsg);
}


AllocatedRegister_Returned RegisterManager::getTempFloatRegister (const std::string &debugMsg) {
  return _getRegisterFromPool(tempFloatRegisters, debugMsg);
}


void RegisterManager::freeRegister (const AllocatedRegister &reg) {
  // TODO: more efficient way to find entry
  for (auto pool_ptr : std::vector<RegisterPool*>{&saveIntRegisters, &tempIntRegisters, &saveFloatRegisters, &tempFloatRegisters}) {
    if (auto iter {pool_ptr->find(reg)}; iter != pool_ptr->end()) {
      // note: allow double free
      iter->second.inUse = false;
      return;
    }
  }

  std::cerr << "RegisterManager::freeRegister : " << reg.reg << " not in any RegisterPool, cannot free !!!" << std::endl;
  assert(false);
}


std::set<Register> RegisterManager::getCallerSaveRegistersOfCurrentProcedure () {
  // TODO : better searching performance
  std::set<Register> usedRegisters;
  for (const auto &reg : currProcedureUsedRegisters) {
    if (CALLER_SAVE_INT_REGISTERS.find(reg) != CALLER_SAVE_INT_REGISTERS.end()) {
      if (tempIntRegisters.find(reg)->second.inUse) {
        usedRegisters.emplace(reg);
      }
    }
    else if (CALLER_SAVE_FLOAT_REGISTERS.find(reg) != CALLER_SAVE_FLOAT_REGISTERS.end()) {
      if (tempFloatRegisters.find(reg)->second.inUse) {
        usedRegisters.emplace(reg);
      }
    }
  }
  return usedRegisters;
}


std::set<Register> RegisterManager::getCalleeSaveRegistersOfCurrentProcedure () {
  std::set<Register> usedRegisters;
  for (const auto &reg : currProcedureUsedRegisters) {
    if (CALLEE_SAVE_INT_REGISTERS.find(reg) != CALLEE_SAVE_INT_REGISTERS.end()
        || CALLER_SAVE_FLOAT_REGISTERS.find(reg) != CALLEE_SAVE_FLOAT_REGISTERS.end()) {
      usedRegisters.emplace(reg);
    }
  }
  return usedRegisters;
}
