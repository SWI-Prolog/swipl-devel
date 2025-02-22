/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022-2025, SWI-Prolog Solutions.h
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
PL_EXPORT(void)			WASM_bind_standard_streams(void);
PL_EXPORT(term_t)		WASM_yield_request(void);
PL_EXPORT(void)			WASM_set_yield_result(term_t result);
PL_EXPORT(size_t)		WASM_variable_id(term_t t);
PL_EXPORT(int)			js_unify_obj(term_t t, int32_t id);
PL_EXPORT(int32_t)		js_get_obj(term_t t);

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

size_t
WASM_variable_id(term_t t)
{ GET_LD
  Word p = valTermRef(t);

  deRef(p);
  return (p > (Word)lBase) ? (p - (Word)lBase)*2
			   : (p - gBase)*2+1;
}

#define CHARS_FLAGS (REP_UTF8|CVT_EXCEPTION|CVT_ATOM|CVT_STRING|CVT_LIST)

/** '$await'(+In, -Out) is det.
 *
 * Yield execution to JavaScript, passing In (a string) and receiving
 * Out
 */

static
PRED_IMPL("$await", 2, await, PL_FA_NONDETERMINISTIC)
{ PRED_LD

  switch(CTX_CNTRL)
  { case FRG_FIRST_CALL:
    { DEBUG(MSG_WASM_ASYNC, Sdprintf("$await: wait on engine %p\n", LD));
      LD->wasm.yield_request = A1;
      LD->wasm.yield_result  = A2;
      LD->wasm.yield_unified = false;
      PL_yield_address(&LD->wasm.yield_request);
    }
    case PL_RESUME:
    { DEBUG(MSG_WASM_ASYNC, Sdprintf("$await: resume on engine %p\n", LD));
      bool rc = LD->wasm.yield_unified;

      LD->wasm.yield_request = 0;
      LD->wasm.yield_result  = 0;
      LD->wasm.yield_unified = false;

      return rc;
    }
    case PL_PRUNED:
    default:
      return true;
  }
}

term_t
WASM_yield_request(void)
{ GET_LD
  return LD->wasm.yield_request;
}

void
WASM_set_yield_result(term_t result)
{ GET_LD

  DEBUG(MSG_WASM_ASYNC,
	Sdprintf("set_yield_result: on engine %p; "
		 "LD->wasm.yield_result = %zd\n",
		 LD, LD->wasm.yield_result));
  if ( !LD->wasm.yield_result )
    PL_api_error("WASM_set_yield_result(): not in await/2");

  LD->wasm.yield_unified = PL_unify(LD->wasm.yield_result, result);
}

static
PRED_IMPL("js_run_script", 1, js_run_script, 0)
{ char *s;

  PL_STRINGS_MARK();
  if ( PL_get_chars(A1, &s, BUF_STACK|CHARS_FLAGS) )
  { emscripten_run_script(s);
  }
  PL_STRINGS_RELEASE();

  return true;
}

static
PRED_IMPL("$js_call", 2, js_call, 0)
{ int rc = EM_ASM_INT({ return prolog_js_call($0, $1);
		      }, (int32_t)A1, (int32_t)A2);
  return !!rc;
}

		 /*******************************
		 *	JAVASCRIPT OBJECTS	*
		 *******************************/

typedef struct objref
{ int32_t id;
} objref;


static atom_t
js_obj_class(int32_t id)
{ char *str = (char*)EM_ASM_PTR({
      const s = prolog_js_obj_class_name($0);
      const len = lengthBytesUTF8(s)+1;
      const mem = _malloc(len);
      stringToUTF8(s, mem, len);
      return mem;
    }, id);
  atom_t a = PL_new_atom_mbchars(REP_UTF8, (size_t)-1, str);
  free(str);

  return a;
}


static int
write_jsobj_ref(IOSTREAM *out, atom_t aref, int flags)
{ objref *ref = PL_blob_data(aref, NULL, NULL);
  (void)flags;
  atom_t cname = js_obj_class(ref->id);
  const wchar_t *s;

  PL_STRINGS_MARK();
  s = PL_atom_wchars(cname, NULL);
  SfprintfX(out, "<js_%Ws>(%d)", s, ref->id);
  PL_STRINGS_RELEASE();

  return true;
}


static int
release_jsobj_blob(atom_t aref)
{ objref *ref = PL_blob_data(aref, NULL, NULL);

  EM_ASM({ release_registered_object($0); }, ref->id);

  return true;
}


static int
save_jsobj_ref(atom_t aref, IOSTREAM *fd)
{ objref *ref = PL_blob_data(aref, NULL, NULL);
  (void)fd;

  return PL_warning("Cannot save reference to <js_object>(%d)",
		    ref->id);
}


static atom_t
load_jsobj_ref(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<saved-js-object>");
}


static PL_blob_t js_obj_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
  "js_object",
  release_jsobj_blob,
  NULL,
  write_jsobj_ref,
  NULL,
  save_jsobj_ref,
  load_jsobj_ref
};


int
js_unify_obj(term_t t, int32_t id)
{ objref ref = { .id = id };

  return PL_unify_blob(t, &ref, sizeof(ref), &js_obj_blob);
}

int32_t
js_get_obj(term_t t)
{ void *data;
  PL_blob_t *type;

  if ( PL_get_blob(t, &data, NULL, &type) &&
       type == &js_obj_blob )
  { objref *ref = data;
    return ref->id;
  }

  return -1;
}

		 /*******************************
		 *       STANDARD STREAMS       *
		 *******************************/

static IOFUNCTIONS orig_functions;
static IOFUNCTIONS wasm_functions;

static void
wasm_flush(void *handle)
{ int32_t fd = (int32_t)(intptr_t)handle;

  EM_ASM({ flush_std_stream($0); }, fd);
}

static ssize_t
wasm_write_std(void *handle, char *buf, size_t size)
{ ssize_t rc = orig_functions.write(handle, buf, size);
  if ( rc == size )
    wasm_flush(handle);
  return rc;
}

static int
wasm_control_std(void *handle, int cmd, void *closure)
{ int rc = orig_functions.control(handle, cmd, closure);
  if ( rc == 0 && cmd == SIO_FLUSHOUTPUT )
    wasm_flush(handle);
  return rc;
}

void
WASM_bind_standard_streams(void)
{ orig_functions = wasm_functions = *Serror->functions;
  wasm_functions.write = wasm_write_std;
  wasm_functions.control = wasm_control_std;
  Serror->functions = &wasm_functions;
  Soutput->functions = &wasm_functions;
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(wasm)
  PRED_DEF("$await",        2, await,         PL_FA_NONDETERMINISTIC)
  PRED_DEF("js_run_script", 1, js_run_script, 0)
  PRED_DEF("$js_call",      2, js_call,       0)
EndPredDefs
