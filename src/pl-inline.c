
/* This file contains implementations of the usually-inlined functions
 * in pl-inline.h (and other header files). If every usage of a PL_INLINE
 * function does get inlined, it will get stripped out of the final library.
 * If not, however, all non-inlined references will call the same function.
 */

#define EMIT_SHARED_INLINES

/* This list of headers MUST include all files with a PL_INLINE declaration */
#include "pl-incl.h"
#include "pl-arith.h"
#include "pl-codelist.h"
#include "pl-comp.h"
#include "pl-dict.h"
#include "pl-event.h"
#include "pl-gmp.h"
#include "pl-inline.h"
#include "pl-privitf.h"
#include "pl-segstack.h"
#include "pl-thread.h"
#include "pl-trie.h"
#include "os/pl-buffer.h"
#include "os/pl-table.h"
#include "os/pl-text.h"
#include "os/pl-utf8.h"
