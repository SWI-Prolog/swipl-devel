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
#include "pl-prims.h"
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

#define isDuplicateKey(buf, key) LDFUNC(isDuplicateKey, buf, key)

static bool
isDuplicateKey(DECL_LD const Buffer buf, atom_t name)
{ const atom_t *found = baseBuffer(buf, const atom_t);
  const size_t  count = entriesBuffer(buf, const atom_t);

  for(size_t i=0; i<count; i++)
  { if ( found[i] == name )
      return true;
  }

  return false;
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

    if ( arity == 0 )
    { if ( PL_is_compound(decl) )
	return PL_error(NULL, 0, "no arguments",
			ERR_DOMAIN, ATOM_argnames, decl);
      else
	return PL_type_error("compound", decl);
    }
    initBuffer(&buf);
    for(size_t i=1; i<=arity; i++)
    { atom_t aname;

      _PL_get_arg(i, decl, arg);
      if ( PL_get_atom_ex(arg, &aname) )
      { if ( isDuplicateKey((Buffer)&buf, aname) )
	{ discardBuffer(&buf);
	  return PL_error(NULL, 0, "duplicate key",
			  ERR_DOMAIN, ATOM_argnames, decl);
	}
	addBuffer(&buf, aname, atom_t);
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

static argnames *
duplicateArgNames(argnames *an)
{ ATOMIC_INC(&an->references);
  return an;
}

static void
freeArgNames(argnames *an)
{ if ( ATOMIC_DEC(&an->references) == 0 )
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

#define get_argnames_link(t, plain, module, error)	\
	LDFUNC(get_argnames_link, t, plain, module, error)

#define AN_ERR_EXISTENCE 0x1
#define AN_ERR_TYPE      0x2
#define AN_DECL		 0x4

static const argnames_link *
get_argnames_link(DECL_LD term_t t, term_t plain, Module *module, int flags)
{ Module m = NULL;
  atom_t name;

  if ( !(plain || (plain=PL_new_term_ref())) ||
       !PL_strip_module(t, &m, plain) )
    return false;

  if ( !(flags&AN_DECL) )
  { Word p = valTermRef(plain);
    deRef(p);
    if ( isTerm(*p) )
    { name = nameFunctor(functorTerm(*p));
    } else
    { if ( (flags&AN_ERR_TYPE) )
	PL_type_error("compound", t);
      return NULL;
    }
  } else if ( !PL_get_name_arity(plain, &name, NULL) )
  { return PL_type_error("callable", t),NULL;
  }

  const argnames_link *link = lookupArgNamesLink(m, name);
  if ( !link && (flags&AN_ERR_EXISTENCE) )
    return PL_existence_error("argnames", t),NULL;
  if ( module )
    *module = m;

  return link;
}

#define get_argnames(t, plain, error) \
	LDFUNC(get_argnames, t, plain, error)

const argnames *
get_argnames(DECL_LD term_t t, term_t plain, int error)
{ const argnames_link *link = get_argnames_link(t, plain, NULL, error);

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
    { return registerArgNames(into, name, duplicateArgNames(link->argnames),
			      flags);
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

bool
argnamesToDict(DECL_LD const argnames *an, term_t c, term_t d,
	       atom_t tag, bool nonvar)
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

  return false;
}

/* dictToArgNames(+dict, argnames*, -term) */

#define dictToArgNames(dict, an, argnames) \
	LDFUNC(dictToArgNames, dict, an, argnames)

static bool
dictToArgNames(DECL_LD term_t dict, const argnames *an, term_t argnames)
{ size_t arity = arityArgNames(an);
  Word p = allocGlobal(1+arity);

  if ( p )
  { Word dp = valTermRef(dict);
    deRef(dp);

    if ( !termIsDict(*dp) )
    { gTop = p;
      return PL_type_error("dict", dict);
    }

    setHandle(argnames, consPtr(p, TAG_COMPOUND|STG_GLOBAL));
    *p++ = an->functor;
    for(size_t i=0; i<arity; i++, p++)
    { Word vp = dict_lookup_ptr(*dp, an->names[i], NULL);
      if ( vp )
	*p = linkValNoG(vp);
      else
	setVar(*p);
    }

    return true;
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
  int redefine = false;
  unsigned int flags = 0;

  if ( !PL_strip_module(A1, &m, decl) )
    return false;
  if ( CTX_ARITY == 2 )
  { if ( !PL_scan_options(A2, 0, "argnames_options", argnames_options,
			  &exported, &redefine) )
      return false;
    if ( exported )
      flags |= AN_EXPORTED;
    if ( redefine )
      flags |= AN_REDEFINE;
  }

  return createArgNames(m, decl, flags);
}

static
PRED_IMPL("argnames", 1, argnames, PL_FA_TRANSPARENT)
{ return pl_argnames2_va(PL__t0, PL__ac, PL__ctx);
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
	    return ( unify_argnames(a2, an) &&
		     PL_unify_atom(A1, name) );
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

#define unify_arg(i, plain, value) LDFUNC(unify_arg, i, plain, value)

static bool
unify_arg(DECL_LD size_t i, term_t plain, term_t Value)
{ Word p = valTermRef(plain);
  deRef(p);
  assert(isTerm(*p));
  assert(i < arityTerm(*p));
  p = argTermP(*p, i);
  return unify_ptrs(p, valTermRef(Value), 0);
}

static foreign_t
pl_arg_name(term_t Term, term_t Arg, term_t Name, term_t Value,
	    control_t PL__ctx)
{ PRED_LD
  const argnames *an;
  size_t ai, arity;
  term_t plain = 0;

  switch(CTX_CNTRL)
  { case FRG_FIRST_CALL:
    { if ( Value ) plain = PL_new_term_ref();
      an = get_argnames(Term, plain, AN_ERR_EXISTENCE|AN_ERR_TYPE);

      if ( an )
      { int64_t iai;
	atom_t name;
	arity = arityArgNames(an);

	if ( PL_get_atom(Name, &name) )
	{ for(size_t i=0; i<arity; i++)
	  { if ( an->names[i] == name )
	    { if ( Arg )
		return PL_unify_int64(Arg, i+1);
	      else
		return unify_arg(i, plain, Value);
	    }
	  }
#if 0
	  if ( Value )
	    return PL_error(NULL, 0, NULL, ERR_EXISTENCE3,
			    ATOM_key, Term, Name);
#endif
	  return false;
	}
	if ( Arg && PL_get_int64(Arg, &iai) )
	{ if ( iai >= 1 && iai <= arity )
	    return PL_unify_atom(Name, an->names[iai-1]);
	  return false;
	}
	if ( (!Arg || PL_is_variable(Arg)) && PL_is_variable(Name) )
	{ ai = 0;
	  break;
	}
      } else
      { return false;
      }
    }
    case FRG_REDO:
      if ( Value ) plain = PL_new_term_ref();
      an = get_argnames(Term, plain, AN_ERR_EXISTENCE|AN_ERR_TYPE);
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
  { if ( (!Arg || PL_unify_int64(Arg, ai+1)) &&
	 PL_unify_atom(Name, an->names[ai]) &&
	 unify_arg(ai, plain, Value) )
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

/** get_argnames(?Name, :Term, ?Value) is nondet.
 */

static
PRED_IMPL("get_argnames", 3, get_argnames,
	  PL_FA_TRANSPARENT|PL_FA_NONDETERMINISTIC)
{ return pl_arg_name(A2, 0, A1, A3, PL__ctx);
}

static
PRED_IMPL("$argnames_property", 3, argnames_property, META)
{ PRED_LD
  Module m = NULL;
  const argnames_link *link = get_argnames_link(A1, 0, &m, AN_DECL);
  atom_t prop;

  if ( link && PL_get_atom_ex(A2, &prop) )
  { if ( prop == ATOM_arity )
      return PL_unify_int64(A3, arityArgNames(link->argnames));
    if ( prop == ATOM_functor )
      return unify_functor(A3, link->argnames->functor, GP_NAMEARITY);
    if ( prop == ATOM_exported )
      return PL_unify_bool(A3, link->exported);
    if ( prop == ATOM_imported )
      return ( m != link->argnames->module &&
	       PL_unify_atom(A3, link->argnames->module->name) );
    return PL_domain_error("argnames_property", A2);
  }

  return false;
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
PRED_IMPL("argnames_to_dict", 3, argnames_to_dict, META)
{ PRED_LD
  atom_t tag = ATOM_dyndict;
  int nonvar = false;
  term_t tmp = PL_new_term_ref();
  Module m = NULL;

  if ( !PL_scan_options(A3, 0, "argnames_dict_options", argnames_dict_options,
			&tag, &nonvar) )
    return false;

  if ( !PL_strip_module(A1, &m, tmp) )
    return false;
  if ( !PL_is_variable(tmp) )
  { const argnames *an = get_argnames(A1, tmp, AN_ERR_EXISTENCE|AN_ERR_TYPE);
    return ( an && argnamesToDict(an, tmp, tmp, tag, nonvar) &&
	     PL_unify(tmp, A2) );
  } else
  { assert(0);
    return false;
  }
}

static
PRED_IMPL("dict_to_argnames", 3, dict_to_argnames, META)
{ PRED_LD
  term_t tmp = PL_new_term_ref();
  Module m = NULL;
  atom_t name;

  if ( !PL_strip_module(A2, &m, tmp) ||
       !PL_get_atom_ex(tmp, &name) )
    return false;
  const argnames_link *link = lookupArgNamesLink(m, name);
  if ( !link )
    return PL_existence_error("argnames", A2);

  return ( dictToArgNames(A1, link->argnames, tmp) &&
	   PL_unify(A3, tmp) );
}

		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(argnames)
  PRED_DEF("argnames",           1, argnames,          META)
  PRED_DEF("argnames",           2, argnames,          META)
  PRED_DEF("get_argnames",       3, get_argnames,      META|NDET)
  PRED_DEF("current_argnames",   2, current_argnames,  META|NDET)
  PRED_DEF("$argnames_property", 3, argnames_property, META)
  PRED_DEF("$import_argnames",   1, import_argnames,   META)
  PRED_DEF("$export_argnames",   1, export_argnames,   META)
  PRED_DEF("argnames_to_dict",   3, argnames_to_dict,  META)
  PRED_DEF("dict_to_argnames",   3, dict_to_argnames,  META)
EndPredDefs
