#ifndef __RESOURCEALLOCATION_HH__
#define __RESOURCEALLOCATION_HH__


#include "header.hh"


// TODO: adjust the size of stored register based on every procedure
const std::vector<Register> CALLEE_SAVE_REGISTERS {
  REG_S1, REG_S2, REG_S3, REG_S4, REG_S5, REG_S6, REG_S7, REG_S8, REG_S9, REG_S10, REG_S11,
  REG_FS0, REG_FS1, REG_FS2, REG_FS3, REG_FS4, REG_FS5, REG_FS6, REG_FS7, REG_FS8, REG_FS9, REG_FS10, REG_FS11
};

const std::vector<Register> CALLER_SAVE_REGISTERS {
  REG_RA, REG_T0, REG_T1, REG_T2, REG_A0, REG_A1, REG_A2, REG_A3, REG_A4, REG_A5, REG_A6, REG_A7, REG_T3, REG_T4, REG_T5, REG_T6,
  REG_FT0, REG_FT1, REG_FT2, REG_FT3, REG_FT4, REG_FT5, REG_FT6, REG_FT7, REG_FA0, REG_FA1, REG_FA2, REG_FA3, REG_FA4, REG_FA5, REG_FA6, REG_FA7, REG_FT8, REG_FT9, REG_FT10, REG_FT11
};


/** A program-level stack manager.
 *  The layout of stack:
 *
 *  * inside a procedure call                        * before "call f_funcLabel"
 *  high address
 *                +------------                                   +------------
 *                | ...                                           | ...
 *                +------------                                   +------------
 *                | param 10                                      | param 10
 *                +------------                                   +------------
 *       fp+16 -> | param 9                              fp+16 -> | param 9
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
 *  +---------------------------------------------+       sp+8 -> | param 8
 *                                                                +------------
 *                                                          sp -> | **empty**
 *                                                                +------------
 *
 */
class StackMemoryManager {
private:
  bool isInProcedure;
  int insertOffset; // initialized to 0

public:
  StackMemoryManager ();
  ~StackMemoryManager ();

  StackMemoryOffset getMemory (size_t);
  size_t getProcedureMemoryConsumption ();

  void enterProcedure ();
  void leaveProcedure ();
};


/* A procedure-level register manager */
class RegisterManager {
private:

public:
  RegisterManager ();
  ~RegisterManager ();

  Register getRegister ();
  void freeRegister (Register);
};



#endif // __RESOURCEALLOCATION_HH__
