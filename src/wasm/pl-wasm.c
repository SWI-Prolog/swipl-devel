/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022, University of Amsterdam
                         VU University Amsterdam
		         CWI, Amsterdam
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

#include "../pl-incl.h"
#include <emscripten.h>

PL_EXPORT(const char *)		WASM_ttymode(void);
PL_EXPORT(const char *)		WASM_yield_request(void);
PL_EXPORT(void)			WASM_set_yield_result(const char *s);

const char *
WASM_ttymode(void)
{ switch(PL_ttymode(Suser_input))
  { case PL_RAWTTY:
      return "raw";
    case PL_COOKEDTTY:
      return "cooked";
    case PL_NOTTY:
    default:
      return "notty";
  }
}


#define CHARS_FLAGS (REP_UTF8|CVT_EXCEPTION|CVT_ATOM|CVT_STRING|CVT_LIST)

/** js_yield(+In, -Out) is det.
 *
 * Yield execution to JavaScript, passing In (a string) and receiving
 * Out
 */

static char *yield_request = NULL;
static char *yield_result  = NULL;

static
PRED_IMPL("js_yield", 2, js_yield, PL_FA_NONDETERMINISTIC)
{ switch(CTX_CNTRL)
  { case FRG_FIRST_CALL:
    { char *s;

      if ( PL_get_chars(A1, &s, BUF_MALLOC|CHARS_FLAGS) )
      { yield_request = s;
	PL_yield_address(s);
	if ( yield_result )
	{ free(yield_result);
	  yield_result = NULL;
	}
      }
    }
    case PL_RESUME:
    { char *s = CTX_PTR;
      int rc = TRUE;

      PL_free(s);
      yield_request = NULL;
      if ( yield_result )
      { rc = PL_unify_chars(A2, PL_STRING|REP_UTF8, (size_t)-1, yield_result);
	free(yield_result);
	yield_result = NULL;
      }

      return rc;
    }
    case PL_PRUNED:
    default:
      return TRUE;
  }
}

const char *
WASM_yield_request(void)
{ return yield_request;
}

void
WASM_set_yield_result(const char *s)
{ yield_result = strdup(s);
}

static
PRED_IMPL("js_call", 1, js_call, 0)
{ char *s;

  PL_STRINGS_MARK();
  if ( PL_get_chars(A1, &s, BUF_STACK|CHARS_FLAGS) )
  { emscripten_run_script(s);
  }
  PL_STRINGS_RELEASE();

  return TRUE;
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(wasm)
  PRED_DEF("js_yield",     2, js_yield,     PL_FA_NONDETERMINISTIC)
  PRED_DEF("js_call",      1, js_call,      0)
EndPredDefs
