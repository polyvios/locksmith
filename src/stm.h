#ifndef __STM_H_
#define __STM_H_

/*......................................................................*/

/*
 * 0. Data type definitions.  
 * 
 * These can be left alone for 'stand-alone' use of libstm within an
 * application program.  They may need to be defined for incorporation
 * in more specialized environments, e.g.  for interaction with a
 * garbage collector.  
 */

#ifndef STM_DONE_ENV_DEFINITION
#define STM_DONE_ENV_DEFINITION
#define STM_ENV void *
#endif

#ifndef STM_DONE_TYPE_DEFINITIONS
#define STM_DONE_TYPE_DEFINITIONS
#if (defined(ENV_IA64) || defined(ENV_SPARC_V9))
typedef long long word_t;
#else
typedef int word_t;
#endif
typedef int volatile *addr_t;
#endif

/*......................................................................*/

/*
 * 1. Operations for invocation when there is no current transaction
 */

/* 
 * Start a new transaction.  
 *
 * Returns TRUE if OK so far.  FALSE => allocation failure.
 */

extern int STMStartTransaction (STM_ENV *env);

/* 
 * Linearizable write to a shared location outside any transaction.
 */

extern void STMVolatileWriteValue (STM_ENV *env, addr_t addr, word_t val);

/*
 * Linearizable read from a shared location outside any transaction.
 */

extern word_t STMVolatileReadValue (STM_ENV *env, addr_t addr);

/*
 * Non-linearizable write to a shared location outside any transaction
 * (possibly causing non-linearizability of concurrent transactions and 
 * STMVolatileWriteValue to that same location.
 */

static void STMBareWriteValue (STM_ENV *env, addr_t addr, word_t val) {
  *(word_t *) addr = val;
}

/*
 * Non-linearizable read from a shared location outside any transaction.
 */

static word_t STMBareReadValue (STM_ENV *env, addr_t addr) {
  return *(word_t *) addr;
}

/*......................................................................*/

/*
 * 2. Operations for invocation when there is a current transaction
 */

/*
 * Validate a read-only transaction, returning either (i) if validation
 * fails or (ii) when or before one of the locations accessed may have
 * been updates.
 */

extern void STMWait (STM_ENV *env);

/*
 * Attempt to commit the current transaction.  
 *
 * Returns TRUE => successful, FALSE => failure.
 */

extern int STMCommitTransaction (STM_ENV *env);

/*
 * Abort the current transaction.
 */

extern void STMAbortTransaction (STM_ENV *env);

/*
 * Perform a write of "val" to "addr" within the current transaction.
 */

extern void STMWriteValue (STM_ENV *env, addr_t addr, word_t val);

/*
 * Perform a read from "addr" within the current transaction.
 *
 * Returns the value read.
 *
 * Note that consistency between reads from multiple locations is
 * guaranteed only upon commit or validate.
 */

extern word_t STMReadValue (STM_ENV *env, addr_t addr);

/*......................................................................*/

/*
 * 3. Management operations
 */

/*
 * Start-of-day initialization.
 */

extern void STMInit (void);

/*
 * End-of-day clean-up.
 */

extern void STMDone (void);

/*
 * Management of the set of threads known by the STM implementation.
 * The implementation assumes that these are called rarely, e.g. 
 * during thread creation and deletion.  They do not have to be called 
 * by the thread in question.
 */

extern void STMAddThread (STM_ENV *env);

extern void STMRemoveThread (STM_ENV *env);

#endif /* __STM_H_ */
