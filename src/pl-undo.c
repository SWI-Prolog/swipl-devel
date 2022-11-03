/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2022, SWI-Prolog Solutions b.v.
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

#include "pl-incl.h"
#include "pl-fli.h"
#include "pl-dbref.h"
#include "pl-undo.h"
#include "pl-wam.h"

term_t
init_undo_list(void)
{ GET_LD
  term_t t = PL_new_term_ref();
  Word p = allocGlobal(1);

  p[0] = ATOM_nil;

  *valTermRef(t) = makeRefG(p);

  return t;
}

void
free_undo_data(PL_local_data_t *ld)
{ if ( ld->undo.scheduled )
  { discardBuffer(ld->undo.scheduled);
    free(ld->undo.scheduled);
  }
}


static int
write_undo_ref(IOSTREAM *s, atom_t aref, int flags)
{ record_t *ref = PL_blob_data(aref, NULL, NULL);
  record_t r = *ref;
  (void)flags;

  Sfprintf(s, "<undo>(%p)", r);
  return TRUE;
}

static void
acquire_undo_blob(atom_t aref)
{
}

static int
release_undo_blob(atom_t aref)
{ record_t *ref = PL_blob_data(aref, NULL, NULL);
  record_t r = *ref;

  PL_erase(r);
  return TRUE;
}

static int
save_undo_ref(atom_t aref, IOSTREAM *fd)
{ record_t *ref = PL_blob_data(aref, NULL, NULL);
  record_t r = *ref;
  (void)fd;

  return PL_warning("Cannot save reference to <undo>(%p)",
		    r);
}

static atom_t
load_undo_ref(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<saved-undo-ref>");
}

static PL_blob_t undo_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
  "undo",
  release_undo_blob,
  NULL,
  write_undo_ref,
  acquire_undo_blob,
  save_undo_ref,
  load_undo_ref
};

static atom_t
save_undo(term_t goal)
{ record_t r = PL_record(goal);
  int new;

  return lookupBlob((const char*)&r, sizeof(r), &undo_blob, &new);
}

static int
put_undo(term_t t, atom_t u)
{ record_t *ref = PL_blob_data(u, NULL, NULL);
  record_t r = *ref;

  return PL_recorded(r, t);
}


static
PRED_IMPL("$undo", 1, undo, 0)
{ PRED_LD

  if ( !LD->undo.running )
  { atom_t u = save_undo(A1);
    Word p;

    if ( (p=allocGlobal(3)) )
    { Word ul = valTermRef(LD->undo.undo_list);

      deRef(ul);
      p[0] = FUNCTOR_dot2;
      p[1] = u;
      p[2] = *ul;
      TrailAssignment(ul);
      DEBUG(MSG_UNDO, Sdprintf("Trailed %p\n", ul));
      *ul = consPtr(p, TAG_COMPOUND|STG_GLOBAL);

      return TRUE;
    } else
      return FALSE;
  } else
    return PL_permission_error("undo", "goal", A1);
}


void
push_undo(DECL_LD Word p)
{ deRef(p);
  if ( isTerm(*p) )
  { Functor f = valueTerm(*p);
    atom_t u;

    if ( f->definition == FUNCTOR_dot2 )
    { u = f->arguments[0];
      if ( !isAtom(u) )
	return;
    } else
      return;

    DEBUG(MSG_UNDO, Sdprintf("Scheduling undo\n"));

    if ( !LD->undo.scheduled )
    { if ( !(LD->undo.scheduled = malloc(sizeof(*LD->undo.scheduled))) )
	outOfCore();
      initBuffer(LD->undo.scheduled);
    }

    PL_register_atom(u);
    addBuffer(LD->undo.scheduled, u, atom_t);

    if ( !(LD->alerted&ALERT_UNDO) )
      updateAlerted(LD);
  }
}


#define put_scheduled_undo(list) LDFUNC(put_scheduled_undo, list)
static int
put_scheduled_undo(DECL_LD term_t list)
{ PL_put_nil(list);

  if ( LD->undo.scheduled )
  { atom_t *base = baseBuffer(LD->undo.scheduled, atom_t);
    atom_t *top0 = topBuffer(LD->undo.scheduled, atom_t);
    atom_t *top  = top0;
    term_t h = PL_new_term_ref();

    DEBUG(MSG_UNDO, Sdprintf("Running %zd undo hooks\n", top-base));

    for(; top > base; )
    { atom_t u = *--top;

      if ( !put_undo(h, u) ||
	   !PL_cons_list(list, h, list) )
	return FALSE;
    }

    for(; top0 > top; )
    { atom_t u = *--top0;
      PL_unregister_atom(u);
    }
    emptyBuffer(LD->undo.scheduled, 1024);
  }

  return TRUE;
}

int
run_undo_hooks(DECL_LD)
{ fid_t fid;

  if ( (fid=PL_open_foreign_frame()) )
  { predicate_t pred;
    term_t list;
    int rc;

    pred = _PL_predicate("$run_undo", 1, "$syspreds", &GD->procedures.drun_undo1);

    LD->undo.running++;
    rc =  ( (list = PL_new_term_ref()) &&
	    put_scheduled_undo(list) &&
	    PL_call_predicate(NULL, PL_Q_PASS_EXCEPTION, pred, list) );
    LD->undo.running--;

    PL_close_foreign_frame(fid);
    return rc;
  }

  return FALSE;
}


BeginPredDefs(undo)
  PRED_DEF("$undo",           1, undo,           0)
EndPredDefs
