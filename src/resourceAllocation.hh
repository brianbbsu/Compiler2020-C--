#ifndef __RESOURCEALLOCATION_HH__
#define __RESOURCEALLOCATION_HH__


#include <unordered_map>
#include <set>
#include <tuple>

#include "header.hh"


const std::set<Register> CALLER_SAVE_INT_REGISTERS {
  REG_T0, REG_T1, REG_T2, REG_T3, REG_T4, REG_T5, REG_T6,
  // To use the following registers, register manager has to know if there is function parameter passed by register.
  // Otherwise, using these registers will overwrite the data in it
  // REG_A1, REG_A2, REG_A3, REG_A4, REG_A5, REG_A6, REG_A7
};


const std::set<Register> CALLEE_SAVE_INT_REGISTERS {
  REG_S1, REG_S2, REG_S3, REG_S4, REG_S5, REG_S6, REG_S7, REG_S8, REG_S9, REG_S10, REG_S11,
};


const std::set<Register> CALLER_SAVE_FLOAT_REGISTERS {
  REG_FT0, REG_FT1, REG_FT2, REG_FT3, REG_FT4, REG_FT5, REG_FT6, REG_FT7, REG_FT8, REG_FT9, REG_FT10, REG_FT11,
  // To use the following registers, register manager has to know if there is function parameter passed by register.
  // Otherwise, using these registers will overwrite the data in it
  // REG_FA1, REG_FA2, REG_FA3, REG_FA4, REG_FA5, REG_FA6, REG_FA7
};


const std::set<Register> CALLEE_SAVE_FLOAT_REGISTERS {
  REG_FS0, REG_FS1, REG_FS2, REG_FS3, REG_FS4, REG_FS5, REG_FS6, REG_FS7, REG_FS8, REG_FS9, REG_FS10, REG_FS11,
};


struct RegisterPoolEntry {
  const Register reg;
  bool inUse;
  std::string debugMsg;
  RegisterPoolEntry (Register reg_, bool inUse_) : reg{reg_}, inUse{inUse_} {}
};

typedef std::unordered_map<Register, RegisterPoolEntry> RegisterPool;


// forward declaration
class RegisterManager;


// This type is to avoid the creation of AllocatedRegister on function returning.
// If an AllocatedRegister is created on function return, it will be destroyed right away, causing freeRegister() being called.
typedef std::tuple<Register, RegisterManager *> AllocatedRegister_Returned;


struct AllocatedRegister {
  Register reg;
  RegisterManager *manager;
  AllocatedRegister () = default;
  AllocatedRegister (const AllocatedRegister_Returned &tuple_) : reg{std::get<0>(tuple_)}, manager{std::get<1>(tuple_)} {}
  ~AllocatedRegister (); // forward declaration
  operator Register () const { return reg; } // conversion function from AllocatedRegister to Register
  AllocatedRegister& operator= (const AllocatedRegister_Returned tuple_) {
    reg = std::get<0>(tuple_);
    manager = std::get<1>(tuple_);
    return *this;
  }
};

std::ostream &operator<< (std::ostream &os, const AllocatedRegister &reg); // forward declaration


/** A program-level stack manager.
 *  The layout of stack:
 *
 *  * inside a procedure call                        * before "call f_funcLabel"
 *  high address
 *                +------------                                   +------------
 *                | ...                                           | ...
 *                +------------                                   +------------
 *                | param 2                                       | param 10
 *                +------------                                   +------------
 *       fp+16 -> | param 1                              fp+16 -> | param 9
 *                +------------                                   +------------
 *        fp+8 -> | ret addr                              fp+8 -> | ret addr
 *                +------------                                   +------------
 *          fp -> | old frame pointer                       fp -> | old frame pointer
 *                +------------                                   +------------
 *                | local var 1                                   | local var 1
 *                +------------                                   +------------
 *                | local var 2                                   | local var 2
 *                +------------                                   +------------
 *                | ...                                           | ...
 *                +------------                                   +------------
 *                | local var n                                   | local var n
 *                +------------                                   +------------
 *                | saved reg 1                                   | saved reg 1
 *                +------------                                   +------------
 *                | ...                                           | ...
 *                +------------                                   +------------
 *        sp+8 -> | saved reg m-1                                 | saved reg m-1
 *                +------------                                   +------------
 *          sp -> | saved reg m   <-+                             | saved reg m
 *                +------------     |                             +------------
 *                | **empty**     <-+                             | param k
 *                +------------     |                             +------------
 *                                  |                             | param k-1
 *  +-------------------------------+-------------+               +------------
 *  | different from course slides !!!            |               | ...
 *  | sp points to **empty** in the course slides |               +------------
 *  +---------------------------------------------+       sp+8 -> | param 1
 *                                                                +------------
 *                                                          sp -> | **empty**
 *                                                                +------------
 *
 */
class StackMemoryManager {
private:
  static const int insertOffsetInitValue {0};
  static const int parameterOffsetInitValue {16};

  bool isInProcedure;
  bool isInParameter;
  int insertOffset;
  int parameterOffset;

public:
  StackMemoryManager ();
  ~StackMemoryManager ();

  StackMemoryOffset getVariableMemory (size_t);
  size_t getProcedureMemoryConsumption ();

  StackMemoryOffset getParameterMemory (size_t);
  size_t getParameterMemoryConsumption ();

  void enterProcedure ();
  void leaveProcedure ();

  void enterParameterDeclaration ();
  void leaveParameterDeclaration ();
};


/* A procedure-level register manager */
class RegisterManager {
private:
  RegisterPool saveIntRegisters;
  RegisterPool tempIntRegisters;
  RegisterPool saveFloatRegisters;
  RegisterPool tempFloatRegisters;

  std::set<Register> currProcedureUsedRegisters;

  AllocatedRegister_Returned _getRegisterFromPool (RegisterPool &, const std::string &);

public:
  RegisterManager ();
  ~RegisterManager ();

  AllocatedRegister_Returned getSaveIntRegister (const std::string &);
  AllocatedRegister_Returned getTempIntRegister (const std::string &);
  AllocatedRegister_Returned getSaveFloatRegister (const std::string &);
  AllocatedRegister_Returned getTempFloatRegister (const std::string &);
  void freeRegister (const AllocatedRegister &);

  void enterProcedure ();
  void leaveProcedure ();

  std::set<Register> getCallerSaveRegistersOfCurrentProcedure ();
  std::set<Register> getCalleeSaveRegistersOfCurrentProcedure ();
};


#endif // __RESOURCEALLOCATION_HH__
