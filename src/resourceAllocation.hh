#ifndef __RESOURCEALLOCATION_HH__
#define __RESOURCEALLOCATION_HH__


#include "header.hh"


// TODO: adjust the size of stored register based on every procedure
const std::vector<Register> REGISTER_TO_BE_STORED {
  REG_S1, REG_S2, REG_S3, REG_S4, REG_S5, REG_S6, REG_S7, REG_S8, REG_S9, REG_S10, REG_S11,
  REG_FS0, REG_FS1, REG_FS2, REG_FS3, REG_FS4, REG_FS5, REG_FS6, REG_FS7, REG_FS8, REG_FS9, REG_FS10, REG_FS11
};


/** A program-level stack manager.
 *  The layout of stack:
 *
 *    high address
 *                  +------------
 *                  | ...
 *                  +------------
 *                  | param 10
 *                  +------------
 *         fp+16 -> | param 9
 *                  +------------
 *          fp+8 -> | ret addr
 *                  +------------
 *            fp -> | old frame pointer
 *                  +------------
 *                  | local var 1
 *                  +------------
 *                  | local var 2
 *                  +------------
 *                  | ...
 *                  +------------
 *                  | local var n
 *                  +------------
 *                  | saved reg 1
 *                  +------------
 *                  | saved reg 2
 *                  +------------
 *                  | ...
 *                  +------------
 *          sp+8 -> | saved reg m
 *                  +------------
 *            sp -> | **empty**
 *                  +------------
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
