/*  Part of SWI-Prolog

    Author:        Peter Ludemann
    E-mail:        peter.ludemann@gmail.com
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2017-2022, VU University Amsterdam
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

/* This is used by test_ffi.pl */

#include <SWI-Prolog.h>
#include <SWI-Stream.h>
#include <assert.h>
#include <string.h>

/* range_ffi/3 is used in regression tests
   - PL_foreign_context() passing an int for the context.
 */
static foreign_t
range_ffi(term_t t_low, term_t t_high, term_t t_result, control_t handle)
{ long result;

  switch( PL_foreign_control(handle) )
  { case PL_FIRST_CALL:
      if ( !PL_get_long_ex(t_low, &result) )
	PL_fail;
      break;
    case PL_REDO:
      result = PL_foreign_context(handle);
      break;
    case PL_PRUNED:
      PL_succeed;
    default:
      assert(0);
  }

  { long high;
    if ( !PL_get_long_ex(t_high, &high) ||
	 result >= high ||
	 !PL_unify_integer(t_result, result) )
      PL_fail;
    PL_retry(result + 1);
  }
}

/* range_ffialloc/3 is used in regression tests
   - PL_foreign_context_address() and malloc()-ed context.
*/
struct range_ctxt
{ long i;
};

static foreign_t
range_ffialloc(term_t t_low, term_t t_high, term_t t_result, control_t handle)
{ struct range_ctxt *ctxt;

  switch( PL_foreign_control(handle) )
  { case PL_FIRST_CALL:
      { long low;
	if ( !PL_get_long_ex(t_low, &low) )
	  PL_fail;
	if ( !(ctxt = malloc(sizeof *ctxt) ) )
	  return PL_resource_error("memory");
	ctxt->i = low;
      }
      break;
    case PL_REDO:
      ctxt = PL_foreign_context_address(handle);
      break;
    case PL_PRUNED:
      ctxt = PL_foreign_context_address(handle);
      free(ctxt);
      PL_succeed;
    default:
      assert(0);
  }

  { long high;
    if ( !PL_get_long_ex(t_high, &high) ||
	 ctxt->i >= high ||
	 !PL_unify_integer(t_result, ctxt->i) )
    { free(ctxt);
      PL_fail;
    }
    ctxt->i += 1;
    PL_retry_address(ctxt);
  }
}

static char* range_ffi_str;
#define RANGE_FFI_STR_LEN 100
#define RANGE_FFI_STR_CONTENTS "RANGE_FFI"


install_t
install_range_ffi4pl(void)
{ PL_register_foreign("range_ffi", 3, range_ffi, PL_FA_NONDETERMINISTIC);
  PL_register_foreign("range_ffialloc", 3, range_ffialloc, PL_FA_NONDETERMINISTIC);
  range_ffi_str = malloc(RANGE_FFI_STR_LEN);
  assert(range_ffi_str);
  strncpy(range_ffi_str, RANGE_FFI_STR_CONTENTS, RANGE_FFI_STR_LEN);
  assert(0 == strncmp(range_ffi_str, RANGE_FFI_STR_CONTENTS, RANGE_FFI_STR_LEN));
  #ifdef O_DEBUG
    Sdprintf("install_range_ffi4pl %s\n", range_ffi_str);
  #endif
}

install_t
uninstall_range_ffi4pl(void)
{ /* If run with ASAN, this also tests that cleanup is done */
  #ifdef O_DEBUG
    Sdprintf("uninstall_range_ffi4pl %s\n", range_ffi_str);
  #endif
  assert(0 == strncmp(range_ffi_str, RANGE_FFI_STR_CONTENTS, RANGE_FFI_STR_LEN));
  free(range_ffi_str);
}

