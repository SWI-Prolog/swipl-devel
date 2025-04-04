/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2025, SWI-Prolog Solutions b.v.
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

#define USE_FLI_INLINES 1
#include "pl-incl.h"
#include "pl-argnames.h"
#include "pl-comp.h"
#include "pl-wam.h"
#include "pl-dict.h"
#include "pl-fli.h"
#include "os/pl-buffer.h"

#define META PL_FA_TRANSPARENT
#define NDET PL_FA_NONDETERMINISTIC

#define AN_EXPORTED	0x1
#define AN_REDEFINE	0x2

static bool equalArgNames(const argnames *a1, const argnames *a2);
static void freeArgNamesLink(argnames_link *link);
static bool unify_argnames(term_t t, const argnames *an);

static void
freeArgNamesSymbol(table_key_t name, table_value_t value)
{ DEBUG(MSG_CLEANUP,
	Sdprintf("freeArgNamesSymbol(%s)\n",
		 PL_atom_chars((atom_t)name)));
  freeArgNamesLink(val2ptr(value));
}

static TableWP
moduleArgNamesTable(Module m)
{ if ( !m->static_dicts )
  { TableWP ht = newHTableWP(4);
    ht->free_symbol = freeArgNamesSymbol;
    if ( !COMPARE_AND_SWAP_PTR(&m->static_dicts, NULL, ht) )
      destroyHTableWP(ht);
  }

  return m->static_dicts;
}

#define registerArgNames(m, name, an, flags)	\
	LDFUNC(registerArgNames, m, name, an, flags)

static bool
registerArgNames(DECL_LD Module m, atom_t name,
		 argnames *an, unsigned int flags)
{ TableWP table = moduleArgNamesTable(m);
  argnames_link *link = allocHeap(sizeof(*link));
  link->exported = !!(flags&AN_EXPORTED);
  link->argnames = an;

  argnames_link *old;
  if ( (flags&AN_REDEFINE) )
  { old = updateHTableWP(table, name, link);
    if ( old != link )
      freeArgNamesLink(old);
  } else
  { if ( (old=addHTableWP(table, name, link)) != link )
    { bool eq = equalArgNames(an, old->argnames);
      freeArgNamesLink(link);
      if ( !eq )
      { term_t ex;

	return ( (ex=PL_new_term_ref()) &&
		 unify_argnames(ex, an) &&
		 PL_permission_error("define", "argnames", ex) );
      }
    }
  }

  return true;
}

#define createArgNames(m, decl, flags) \
	LDFUNC(createArgNames, m, decl, flags)

static bool
createArgNames(DECL_LD Module m, term_t decl, unsigned int flags)
{ atom_t name;
  size_t arity;

  if ( PL_get_name_arity(decl, &name, &arity) )
  { tmp_buffer buf;
    term_t arg = PL_new_term_ref();

    initBuffer(&buf);
    for(size_t i=1; i<=arity; i++)
    { atom_t aname;

      _PL_get_arg(i, decl, arg);
      if ( PL_get_atom_ex(arg, &aname) )
      { addBuffer(&buf, aname, atom_t);
      } else
      { discardBuffer(&buf);
	return false;
      }
    }

    argnames *an = allocHeap(sizeof(*an));
    memset(an, 0, sizeof(*an));
    an->references = 1;
    an->module     = m;
    an->functor    = PL_new_functor(name, arity);
    an->names      = allocHeap(sizeof(*an->names)*arity);
    memcpy(an->names, baseBuffer(&buf, atom_t), sizeof(*an->names)*arity);
    discardBuffer(&buf);
    for(size_t i=0; i<arity; i++)
      PL_register_atom(an->names[i]);

    return registerArgNames(m, name, an, flags);
  } else
  { return PL_type_error("compound", decl);
  }
}

static bool
equalArgNames(const argnames *a1, const argnames *a2)
{ if ( a1->functor == a2->functor )
  { size_t arity = arityFunctor(a1->functor);
    return memcmp(a1->names, a2->names, arity*sizeof(*a1->names)) == 0;
  }

  return false;
}

const argnames *
duplicateArgNames(argnames *an)
{ ATOMIC_INC(&an->references);
  return an;
}

static void
freeArgNames(argnames *an)
{ if ( ATOMIC_DEC(&an->references) )
  { size_t arity = PL_functor_arity(an->functor);

    for(size_t i=0; i<arity; i++)
      PL_unregister_atom(an->names[i]);
    freeHeap(an->names, arity*sizeof(*an->names));
    freeHeap(an, sizeof(*an));
  }
}

static void
freeArgNamesLink(argnames_link *link)
{ freeArgNames(link->argnames);
  freeHeap(link, sizeof(*link));
}

#define lookupArgNamesLink(m, name) LDFUNC(lookupArgNamesLink, m, name)

static argnames_link *
lookupArgNamesLink(DECL_LD const Module m, atom_t name)
{ argnames_link *link;
  ListCell c;

  if ( m->static_dicts &&
       (link=lookupHTableWP(m->static_dicts, name)) )
    return link;

  for(c = m->supers; c; c=c->next)
  { if ( (link = lookupArgNamesLink(c->value, name)) )
      return link;
  }

  return NULL;
}

const argnames *
lookupArgNames(DECL_LD const Module m, atom_t name)
{ argnames_link *link = lookupArgNamesLink(m, name);
  if ( link )
    return link->argnames;
  return NULL;
}

/**
 * Return 1-based argument index for `aname`.
 */

size_t
argNamesArg(const argnames *an, atom_t aname)
{ size_t arity = arityFunctor(an->functor);

  for(size_t i=0; i<arity; i++) {
    if ( an->names[i] == aname )
      return i+1;
  }

  return 0;
}

size_t
arityArgNames(const argnames *an)
{ return arityFunctor(an->functor);
}

static bool
unify_argnames(term_t t, const argnames *an)
{ if ( PL_unify_functor(t, an->functor) )
  { size_t a = arityArgNames(an);
    term_t tmp = PL_new_term_ref();

    for(size_t i=0; i<a; i++)
    { _PL_get_arg(i+1, t, tmp);
      if ( !PL_unify_atom(tmp, an->names[i]) )
	return false;
    }

    return true;
  }

  return false;
}

#define get_argnames_link(t, plain, error) \
	LDFUNC(get_argnames_link, t, plain, error)

const argnames_link *
get_argnames_link(DECL_LD term_t t, term_t plain, bool error)
{ Module m = NULL;
  atom_t name;

  if ( !(plain || (plain=PL_new_term_ref())) ||
       !PL_strip_module(t, &m, plain) )
    return false;

  if ( PL_get_name_arity(plain, &name, NULL) )
  { const argnames_link *link = lookupArgNamesLink(m, name);
    if ( !link && error )
      return PL_existence_error("argnames", t),NULL;
    return link;
  }
  if ( PL_is_callable(t) )
    return PL_type_error("callable", t),NULL;

  return false;
}

#define get_argnames(t, plain, error) \
	LDFUNC(get_argnames, t, plain, error)

const argnames *
get_argnames(DECL_LD term_t t, term_t plain, bool error)
{ const argnames_link *link = get_argnames_link(t, plain, error);

  if ( link )
    return link->argnames;
  return NULL;
}


		 /*******************************
		 *      MODULE OPERATIONS       *
		 *******************************/

static bool
noArgNames(Module m, atom_t name)
{ term_t av;

  return ( (av=PL_new_term_refs(2)) &&
	   PL_put_atom(av+0, m->name) &&
	   PL_put_atom(av+1, name) &&
	   PL_cons_functor_v(av+0, FUNCTOR_colon2, av) &&
	   PL_existence_error("argnames", av+0) );
}

#define exportArgNames(m, name, export) \
	LDFUNC(exportArgNames, m, name, export)

static bool
exportArgNames(DECL_LD Module m, atom_t name, bool export)
{ argnames_link *link = lookupArgNamesLink(m, name);

  if ( link )
  { link->exported = export;
    return true;
  }

  return noArgNames(m, name);
}

#define importArgNames(info, from, name, flags) \
	LDFUNC(importArgNames, info, from, name, flags)

static bool
importArgNames(DECL_LD Module into, Module from, atom_t name,
	       unsigned int flags)
{ if ( name )
  { argnames_link *link = lookupArgNamesLink(from, name);
    if ( link )
    { return registerArgNames(into, name, link->argnames, flags);
    } else
    { return noArgNames(from, name);
    }
  } else
  { if ( from->static_dicts )
    { FOR_TABLE(from->static_dicts, k, v)
      { argnames_link *link = val2ptr(v);
	if ( link->exported )
	{ if ( !importArgNames(into, from, k, flags) )
	    return false;
	}
      }
    }

    return true;
  }
}

		 /*******************************
		 *       DICT INTEGRATION       *
		 *******************************/

#define argnamesToDict(t, d, tag, nonvar) \
	LDFUNC(argnamesToDict, t, d, tag, nonvar)

static size_t
nonvarArgs(term_t t)
{ Word p = valTermRef(t);
  deRef(p);
  assert(isTerm(*p));
  size_t arity = arityTerm(*p);
  Word a = argTermP(*p, 0);
  size_t count = 0;

  for(size_t i=0; i<arity; i++, a++)
  { Word a2;
    deRef2(a, a2);
    if ( !isVar(*a2) )
      count++;
  }

  return count;
}

static bool
argnamesToDict(DECL_LD term_t t, term_t d, atom_t tag, bool nonvar)
{ term_t c;
  const argnames *an;

  if ( (c=PL_new_term_ref()) &&
       (an=get_argnames(t, c, true)) )
  { size_t arity = nonvar ? nonvarArgs(c) : arityArgNames(an);
    Word dict = allocGlobal(2+arity*2);

    if ( dict )
    { Word p = dict;
      Word s = valTermRef(c);

      deRef(s);
      s = argTermP(*s, 0);

      *p++ = dict_functor(arity);
      *p++ = tag ? tag : nameFunctor(an->functor);
      for(size_t i=0; i<arity; i++)
      { if ( nonvar )
	{ Word a;
	  deRef2(&s[i], a);
	  if ( isVar(*a) )
	    continue;
	}
	*p++ = linkValNoG(&s[i]);
	*p++ = an->names[i];
      }

      if ( dict_order(dict, NULL) != true )
	return false;

      setHandle(d, consPtr(dict, TAG_COMPOUND|STG_GLOBAL));
      return true;
    }
  }

  return false;
}


		 /*******************************
		 *      PROLOG PREDICATES       *
		 *******************************/

static const PL_option_t argnames_options[] =
{ { ATOM_exported,	    OPT_BOOL },
  { ATOM_redefine,	    OPT_BOOL },
  { NULL_ATOM,		    0 }
};

static
PRED_IMPL("argnames", 2, argnames, PL_FA_TRANSPARENT)
{ PRED_LD
  Module m = NULL;
  term_t decl = PL_new_term_ref();
  int exported = false;
  int redefine = false;\
  unsigned int flags = 0;

  if ( !PL_strip_module(A1, &m, decl) )
    return false;
  if ( !PL_scan_options(A2, 0, "argnames_options", argnames_options,
			&exported, &redefine) )
    return false;
  if ( exported )
    flags |= AN_EXPORTED;
  if ( redefine )
    flags |= AN_REDEFINE;

  return createArgNames(m, decl, flags);
}

/** current_argnames(?Name, :Term) is nondet.
 */

static void
scanVisibleArgNames(Module m, atom_t name, Buffer b, bool inherit)
{ if ( !m )
  { FOR_TABLE(GD->tables.modules, k, v)
    { scanVisibleArgNames(val2ptr(v), name, b, false);
    }
    return;
  }

  if ( m->static_dicts )
  { FOR_TABLE(m->static_dicts, n, v)
    { argnames_link *link = val2ptr(v);
      addBuffer(b, link->argnames, argnames*);
    }
  }

  if ( inherit )
  { ListCell c;

    for(c=m->supers; c; c=c->next)
      scanVisibleArgNames(c->value, name, b, inherit);
  }
}

typedef struct
{ buffer	buffer;
  size_t	index;
  bool		var_module;
} argnames_enum;

static
PRED_IMPL("current_argnames", 2, current_argnames,
	  PL_FA_NONDETERMINISTIC|PL_FA_TRANSPARENT)
{ PRED_LD
  Module m = NULL;
  atom_t name = 0;
  size_t arity;
  term_t a2;
  const argnames *an;
  argnames_enum *e;
  Buffer b;
  term_t mt = 0;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { if ( !(a2=PL_new_term_ref()) ||
	   !PL_strip_module(A2, &m, a2) )
	return false;

      /* Called as current_argnames(Name, M:ArgNames) */
      if ( PL_is_functor(a2, FUNCTOR_colon2) )
      { mt = PL_new_term_ref();
	_PL_get_arg(1, a2, mt);
	_PL_get_arg(2, a2, a2);
	if ( !PL_is_variable(mt) )
	  return PL_type_error("module", mt);
      }

      if ( PL_get_name_arity(a2, &name, &arity) )
      { if ( !mt )		/* specified module */
	{ if ( (an=lookupArgNames(m, name)) )
	    return unify_argnames(a2, an);
	  return false;
	}
      } else if ( !PL_is_variable(a2) )
      { return PL_type_error("compound", a2);
      }

      if ( PL_get_atom(A1, &name) )
      { if ( !mt )		/* specified module */
	{ if ( (an=lookupArgNames(m, name)) )
	    return unify_argnames(a2, an);
	  return false;
	}
      } else if ( !PL_is_variable(A1) )
      { return PL_type_error("atom", A1);
      }

      /* enumerate */
      e = allocHeapOrHalt(sizeof(*e));
      b = &e->buffer;
      initBuffer(b);
      e->index = 0;
      e->var_module = !!mt;
      scanVisibleArgNames(mt?NULL:m, name, b, true);
      break;
    }
    case FRG_REDO:
    { e = CTX_PTR;
      b = &e->buffer;
      if ( !(a2=PL_new_term_ref()) ||
	   !PL_strip_module(A2, &m, a2) )
	return false;
      if ( e->var_module )
      { mt = PL_new_term_ref();
	_PL_get_arg(1, a2, mt);
	_PL_get_arg(2, a2, a2);
      }
      break;
    }
    case FRG_CUTTED:
    { e = CTX_PTR;

      if ( e )
      { discardBuffer(&e->buffer);
	freeHeap(e, sizeof(*e));
      }

      return true;
    }
    default:
      assert(0);
      return false;
  }

  fid_t fid = PL_open_foreign_frame();
  size_t mx = entriesBuffer(b, const argnames*);
  const argnames **match = &baseBuffer(b, const argnames*)[e->index];
  for(; e->index++<mx; match++)
  { an = *match;

    if ( PL_unify_atom(A1, nameFunctor(an->functor)) &&
	 (!mt || PL_unify_atom(mt, an->module->name)) &&
	 unify_argnames(a2, an) )
    { if ( e->index == mx )
      { discardBuffer(&e->buffer);
        freeHeap(e, sizeof(*e));
	return true;
      }
      ForeignRedoPtr(e);
    }

    PL_rewind_foreign_frame(fid);
  }

  discardBuffer(&e->buffer);
  freeHeap(e, sizeof(*e));

  return false;
}

static
PRED_IMPL("arg_name", 3, arg_name,
	  PL_FA_TRANSPARENT|PL_FA_NONDETERMINISTIC)
{ PRED_LD
  const argnames *an;
  size_t ai, arity;

  switch(CTX_CNTRL)
  { case FRG_FIRST_CALL:
    { an = get_argnames(A1, 0, true);

      if ( an )
      { int64_t iai;
	atom_t name;
	arity = arityArgNames(an);

	if ( PL_get_atom(A3, &name) )
	{ for(size_t i=0; i<arity; i++)
	  { if ( an->names[i] == name )
	      return PL_unify_int64(A2, i+1);
	  }
	  return false;
	}
	if ( PL_get_int64(A2, &iai) )
	{ if ( iai >= 1 && iai <= arity )
	    return PL_unify_atom(A3, an->names[iai-1]);
	  return false;
	}
	if ( PL_is_variable(A2) && PL_is_variable(A3) )
	{ ai = 0;
	  break;
	}
      }
    }
    case FRG_REDO:
      an = get_argnames(A1, 0, true);
      arity = arityArgNames(an);
      ai = CTX_INT;
      break;
    case FRG_CUTTED:
      return true;
    default:
      assert(0);
      return false;
  }

  fid_t fid = PL_open_foreign_frame();
  for(; ai < arity; ai++)
  { if ( PL_unify_int64(A2, ai+1) &&
	 PL_unify_atom(A3, an->names[ai]) )
    { PL_close_foreign_frame(fid);
      if ( ai == arity-1 )
	return true;
      ForeignRedoInt(ai+1);
    }
    PL_rewind_foreign_frame(fid);
  }
  PL_close_foreign_frame(fid);

  return false;
}

static
PRED_IMPL("$argnames_property", 3, argnames_property, META)
{ PRED_LD
    const argnames_link *link = get_argnames_link(A1, 0, false);
  atom_t prop;

  if ( link && PL_get_atom_ex(A2, &prop) )
  { if ( prop == ATOM_arity )
      return PL_unify_int64(A3, arityArgNames(link->argnames));
    if ( prop == ATOM_functor )
      return unify_functor(A3, link->argnames->functor, GP_NAMEARITY);
    if ( prop == ATOM_exported )
      return PL_unify_bool(A3, link->exported);
    return PL_domain_error("argnames_property", A2);
  }

  return FALSE;
}

static
PRED_IMPL("$import_argnames", 1, import_argnames, META)
{ PRED_LD
  Module destination = contextModule(environment_frame);
  Module source = NULL;
  term_t a1 = PL_new_term_ref();
  atom_t name;

  if ( !PL_strip_module(A1, &source, a1) )
    return false;
  if ( PL_is_variable(a1) )
    name = 0;
  else if ( !PL_get_atom_ex(a1, &name) )
    return false;

  return importArgNames(destination, source, name, 0);
}

static
PRED_IMPL("$export_argnames", 1, export_argnames, META)
{ Module from = NULL;
  term_t a1 = PL_new_term_ref();
  atom_t name;

  if ( !PL_strip_module(A1, &from, a1) )
    return false;
  if ( !PL_get_atom_ex(a1, &name) )
    return false;

  return exportArgNames(from, name, true);
}

static const PL_option_t argnames_dict_options[] =
{ { ATOM_tag,	    OPT_ATOM },
  { ATOM_nonvar,    OPT_BOOL },
  { NULL_ATOM,      0 }
};




static
PRED_IMPL("argnames_dict", 3, argnames_dict, META)
{ PRED_LD
  atom_t tag = 0;
  int nonvar = false;
  term_t tmp = PL_new_term_ref();
  Module m = NULL;

  if ( !PL_scan_options(A3, 0, "argnames_dict_options", argnames_dict_options,
			&tag, &nonvar) )
    return false;

  if ( !PL_strip_module(A1, &m, tmp) )
    return false;
  if ( !PL_is_variable(tmp) )
  { return ( argnamesToDict(A1, tmp, tag, nonvar) &&
	     PL_unify(tmp, A2) );
  } else
  { assert(0);
    return false;
  }
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(argnames)
  PRED_DEF("argnames",           2, argnames,          META)
  PRED_DEF("arg_name",           3, arg_name,          META|NDET)
  PRED_DEF("current_argnames",   2, current_argnames,  META|NDET)
  PRED_DEF("$argnames_property", 3, argnames_property, META)
  PRED_DEF("$import_argnames",   1, import_argnames,   META)
  PRED_DEF("$export_argnames",   1, export_argnames,   META)
  PRED_DEF("argnames_dict",      3, argnames_dict,     META)
EndPredDefs
