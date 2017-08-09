/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2013-2015, VU University Amsterdam
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
#include "pl-locale.h"
#include <errno.h>

#if defined(__sun) || __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ < 1070
#undef HAVE_WCSDUP			/* No prototype, so better use our own */
#endif

#ifdef O_LOCALE

#include <locale.h>

#undef LD				/* fetch LD once per function */
#define LD LOCAL_LD

#define LSTR_MAX 16

#ifndef HAVE_LOCALECONV
typedef struct
{ char *decimal_point;
  char *thousands_sep;
  char *grouping;
} lconv;

struct lconv *
localeconv(void)
{ static struct lconv defl =
  { ".",
    ",",
    "\003\003"
  };

  return &defl;
}
#endif


#ifndef HAVE_WCSDUP
static wchar_t *
my_wcsdup(const wchar_t *in)
{ wchar_t *copy = malloc((wcslen(in)+1)*sizeof(wchar_t));

  if ( copy )
    return wcscpy(copy, in);

  return NULL;
}
#define wcsdup(ws) my_wcsdup(ws)
#endif


static wchar_t *
ls_to_wcs(const char *in, const wchar_t *on_error)
{ wchar_t buf[LSTR_MAX];
  mbstate_t state;

  memset(&state, 0, sizeof(state));
  mbsrtowcs(buf, &in, LSTR_MAX, &state);
  if ( in == NULL )
  { return wcsdup(buf);
  } else
  { Sdprintf("Illegal locale string: %s\n", in);
    return wcsdup(on_error);
  }
}


static int
init_locale_strings(PL_locale *l, struct lconv *conv)
{ if ( conv )
  { l->decimal_point = ls_to_wcs(conv->decimal_point, L".");
    l->thousands_sep = ls_to_wcs(conv->thousands_sep, L",");
    l->grouping      = strdup(conv->grouping);

    return TRUE;
  } else
  { l->decimal_point = wcsdup(L".");
    l->thousands_sep = wcsdup(L",");
    l->grouping      = strdup("\003");

    return FALSE;
  }
}


static PL_locale *
new_locale(PL_locale *proto)
{ PL_locale *new = PL_malloc(sizeof(*new));

  if ( new )
  { memset(new, 0, sizeof(*new));
    new->magic = LOCALE_MAGIC;

    if ( proto )
    { new->decimal_point = wcsdup(proto->decimal_point);
      new->thousands_sep = wcsdup(proto->thousands_sep);
      new->grouping      = strdup(proto->grouping);
    } else
    { init_locale_strings(new, localeconv());
    }
  }

  return new;
}


static void
free_locale_strings(PL_locale *l)
{ free(l->decimal_point);
  free(l->thousands_sep);
  free(l->grouping);
}

static void
free_locale(PL_locale *l)
{ if ( l )
  { free_locale_strings(l);

    if ( l->alias )
      PL_unregister_atom(l->alias);

    PL_free(l);
  }
}

static void
update_locale(PL_locale *l, int category, const char *locale)
{ free_locale_strings(l);
  init_locale_strings(l, localeconv());
}


static int
alias_locale(PL_locale *l, atom_t alias)
{ GET_LD
  int rc;

  PL_LOCK(L_LOCALE);

  if ( !GD->locale.localeTable )
    GD->locale.localeTable = newHTable(16);

  if ( lookupHTable(GD->locale.localeTable, (void*)alias) )
  { GET_LD
    term_t obj = PL_new_term_ref();

    PL_put_atom(obj, alias);
    rc = PL_error("locale_create", 2, "Alias name already taken",
		  ERR_PERMISSION, ATOM_create, ATOM_locale, obj);
  }
  else
  { addNewHTable(GD->locale.localeTable, (void*)alias, l);
    l->alias = alias;
    PL_register_atom(alias);
    rc = TRUE;
  }
  PL_UNLOCK(L_LOCALE);

  return rc;
}


		 /*******************************
		 *	  LOCALE BLOB		*
		 *******************************/

typedef struct locale_ref
{ PL_locale	*data;
} locale_ref;


static int
write_locale_ref(IOSTREAM *s, atom_t aref, int flags)
{ locale_ref *ref = PL_blob_data(aref, NULL, NULL);
  (void)flags;

  Sfprintf(s, "<locale>(%p)", ref->data);

  return TRUE;
}


static void
acquire_locale_ref(atom_t aref)
{ locale_ref *ref = PL_blob_data(aref, NULL, NULL);

  (void)ref;
}


static int
release_locale_ref(atom_t aref)
{ locale_ref *ref = PL_blob_data(aref, NULL, NULL);

  PL_LOCK(L_LOCALE);
  if ( ref->data->references == 0 )
    free_locale(ref->data);
  else
    ref->data->symbol = 0;
  PL_UNLOCK(L_LOCALE);

  return TRUE;
}


static int
save_locale_ref(atom_t aref, IOSTREAM *fd)
{ locale_ref *ref = PL_blob_data(aref, NULL, NULL);
  (void)fd;

  return PL_warning("Cannot save reference to <locale>(%p)", ref->data);
}


static atom_t
load_locale_ref(IOSTREAM *fd)
{ (void)fd;

  return PL_new_atom("<saved-locale-ref>");
}


static PL_blob_t locale_blob =
{ PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE,
  "locale",
  release_locale_ref,
  NULL,
  write_locale_ref,
  acquire_locale_ref,
  save_locale_ref,
  load_locale_ref
};


		 /*******************************
		 *	   PROLOG HANDLE	*
		 *******************************/

int
unifyLocale(term_t t, PL_locale *l, int alias)
{ GET_LD
  term_t b;

  if ( l->alias && alias )
    return PL_unify_atom(t, l->alias);

  if ( l->symbol )
    return PL_unify_atom(t, l->symbol);

  if ( (b=PL_new_term_ref()) &&
       PL_put_blob(b, &l, sizeof(l), &locale_blob) )
  { PL_get_atom(b, &l->symbol);
    assert(l->symbol);
    return PL_unify(t, b);
  }

  return FALSE;
}


int
getLocale(term_t t, PL_locale **lp)
{ GET_LD
  atom_t a;

  if ( PL_get_atom(t, &a) )
  { PL_locale *l = NULL;
    PL_blob_t *bt;
    locale_ref *ref;

    if ( a == ATOM_current_locale )
    { GET_LD

      l = LD->locale.current;
    } else if ( (ref=PL_blob_data(a, NULL, &bt)) && bt == &locale_blob )
    { l = ref->data;
    } else if ( GD->locale.localeTable )
    { l = lookupHTable(GD->locale.localeTable, (void*)a);
    }

    if ( l )
    { assert(l->magic == LOCALE_MAGIC);
      *lp = acquireLocale(l);
      return TRUE;
    }
  }

  return FALSE;
}


int
getLocaleEx(term_t t, PL_locale **lp)
{ GET_LD

  if ( getLocale(t, lp) )
    return TRUE;

  if ( PL_is_atom(t) )
    PL_existence_error("locale", t);
  else
    PL_type_error("locale", t);

  return FALSE;
}


		 /*******************************
		 *	 PROLOG BINDING		*
		 *******************************/

static int		/* locale_property(Mutex, alias(Name)) */
locale_alias_property(PL_locale *l, term_t prop ARG_LD)
{ if ( l->alias )
    return PL_unify_atom(prop, l->alias);

  return FALSE;
}

static int		/* locale_property(Locale, decimal_point(Atom)) */
locale_decimal_point_property(PL_locale *l, term_t prop ARG_LD)
{ if ( l->decimal_point && l->decimal_point[0] )
    return PL_unify_wchars(prop, PL_ATOM, (size_t)-1, l->decimal_point);

  return FALSE;
}

static int		/* locale_property(Locale, thousands_sep(Atom)) */
locale_thousands_sep_property(PL_locale *l, term_t prop ARG_LD)
{ if ( l->thousands_sep && l->thousands_sep[0] )
    return PL_unify_wchars(prop, PL_ATOM, (size_t)-1, l->thousands_sep);

  return FALSE;
}

static int		/* locale_property(Locale, grouping(List)) */
locale_grouping_property(PL_locale *l, term_t prop ARG_LD)
{ if ( l->grouping && l->grouping[0] )
  { term_t tail = PL_copy_term_ref(prop);
    term_t head = PL_new_term_ref();
    char *s;

    for(s=l->grouping; ; s++)
    { if ( !PL_unify_list(tail, head, tail) )
	return FALSE;
      if ( s[1] == 0 || (s[1] == s[0] && s[2] == 0) )
	return ( PL_unify_term(head, PL_FUNCTOR, FUNCTOR_repeat1,
			       PL_INT, (int)s[0]) &&
		 PL_unify_nil(tail)
	       );
      if ( s[0] == CHAR_MAX )
	return PL_unify_nil(tail);
      if ( !PL_unify_integer(head, s[0]) )
	return FALSE;
    }
  }

  return FALSE;
}


static const tprop lprop_list [] =
{ { FUNCTOR_alias1,	    locale_alias_property },
  { FUNCTOR_decimal_point1, locale_decimal_point_property },
  { FUNCTOR_thousands_sep1, locale_thousands_sep_property },
  { FUNCTOR_grouping1,      locale_grouping_property },
  { 0,			    NULL }
};

typedef struct
{ TableEnum e;				/* Enumerator on mutex-table */
  PL_locale *l;				/* current locale */
  const tprop *p;			/* Pointer in properties */
  int enum_properties;			/* Enumerate the properties */
} lprop_enum;


static int
advance_lstate(lprop_enum *state)
{ if ( state->enum_properties )
  { state->p++;
    if ( state->p->functor )
      return TRUE;

    state->p = lprop_list;
  }
  if ( state->e )
  { PL_locale *l;

    if ( advanceTableEnum(state->e, NULL, (void**)&l) )
    { state->l = l;

      return TRUE;
    }
  }

  return FALSE;
}


static void
free_lstate(lprop_enum *state)
{ if ( state->e )
    freeTableEnum(state->e);
  else if ( state->l )
    releaseLocale(state->l);

  freeForeignState(state, sizeof(*state));
}


static int
get_atom_arg(term_t t, atom_t *a)
{ GET_LD
  term_t t2 = PL_new_term_ref();

  return PL_get_arg(1, t, t2) && PL_get_atom(t2, a);
}


/** locale_property(?Locale, ?Property) is nondet.
*/

static
PRED_IMPL("locale_property", 2, locale_property, PL_FA_NONDETERMINISTIC)
{ PRED_LD
  term_t locale = A1;
  term_t property = A2;
  lprop_enum statebuf;
  lprop_enum *state;

  switch( CTX_CNTRL )
  { case FRG_FIRST_CALL:
    { memset(&statebuf, 0, sizeof(statebuf));
      state = &statebuf;

      if ( PL_is_variable(locale) )
      { switch( get_prop_def(property, ATOM_locale_property,
			     lprop_list, &state->p) )
	{ case 1:
	  { atom_t alias;

	    if ( state->p->functor == FUNCTOR_alias1 &&
		 get_atom_arg(property, &alias) )
	    { PL_locale *l;

	      if ( (l = lookupHTable(GD->locale.localeTable, (void*)alias)) )
		return unifyLocale(locale, l, FALSE);
	      else
		return FALSE;
	    }
	    state->e = newTableEnum(GD->locale.localeTable);
	    goto enumerate;
	  }
	  case 0:
	    state->e = newTableEnum(GD->locale.localeTable);
	    state->p = lprop_list;
	    state->enum_properties = TRUE;
	    goto enumerate;
	  case -1:
	    return FALSE;
	}
      } else if ( getLocale(locale, &state->l) )
      { switch( get_prop_def(property, ATOM_locale_property,
			     lprop_list, &state->p) )
	{ case 1:
	    goto enumerate;
	  case 0:
	    state->p = lprop_list;
	    state->enum_properties = TRUE;
	    goto enumerate;
	  case -1:
	    return FALSE;
	}
      } else
      { return FALSE;
      }
    }
    case FRG_REDO:
      state = CTX_PTR;
      break;
    case FRG_CUTTED:
      state = CTX_PTR;
      free_lstate(state);
      succeed;
    default:
      assert(0);
      fail;
  }

enumerate:
  if ( !state->l )			/* first time, enumerating locales */
  { PL_locale *l;

    assert(state->e);
    if ( advanceTableEnum(state->e, NULL, (void**)&l) )
    { state->l = l;
    } else
    { freeTableEnum(state->e);
      assert(state != &statebuf);
      return FALSE;
    }
  }

  { term_t arg = PL_new_term_ref();

    if ( !state->enum_properties )
      _PL_get_arg(1, property, arg);

    for(;;)
    { if ( (*state->p->function)(state->l, arg PASS_LD) )
      { if ( state->enum_properties )
	{ if ( !PL_unify_term(property,
			      PL_FUNCTOR, state->p->functor,
			        PL_TERM, arg) )
	    goto error;
	}
	if ( state->e )
	{ if ( !unifyLocale(locale, state->l, TRUE) )
	    goto error;
	}

	if ( advance_lstate(state) )
	{ if ( state == &statebuf )
	  { lprop_enum *copy = allocForeignState(sizeof(*copy));

	    *copy = *state;
	    state = copy;
	  }

	  ForeignRedoPtr(state);
	}

	if ( state != &statebuf )
	  free_lstate(state);
	return TRUE;
      }

      if ( !advance_lstate(state) )
      { error:
	if ( state != &statebuf )
	  free_lstate(state);
	return FALSE;
      }
    }
  }
}


static int
set_chars(term_t t, wchar_t **valp)
{ wchar_t *s;

  if ( PL_get_wchars(t, NULL, &s, CVT_ATOM|CVT_EXCEPTION) )
  { free(*valp);
    if ( (*valp = wcsdup(s)) )
      return TRUE;
    return PL_no_memory();
  }

  return FALSE;
}


#define MAX_GROUPING 10

static int
get_group_size_ex(term_t t, int *s)
{ int i;

  if ( PL_get_integer_ex(t, &i) )
  { if ( i > 0 && i < CHAR_MAX )
    { *s = i;
      return TRUE;
    }
    PL_domain_error("digit_group_size", t);
  }

  return FALSE;
}


static int
set_grouping(term_t t, char **valp)
{ GET_LD
  char s[MAX_GROUPING];
  term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();
  char *o = s;

  while(PL_get_list_ex(tail, head, tail))
  { int g;

    if ( o-s+2 >= MAX_GROUPING )
      return PL_representation_error("digit_groups");

    if ( PL_is_functor(head, FUNCTOR_repeat1) )
    { if ( !PL_get_nil_ex(tail) )
	return FALSE;

      _PL_get_arg(1, head, head);
      if ( get_group_size_ex(head, &g) )
      { *o++ = g;
	goto end;
      }
      return FALSE;
    }
    if ( get_group_size_ex(head, &g) )
    { *o++ = g;
    } else
      return FALSE;
  }

  if ( PL_get_nil_ex(tail) )
  { *o++ = CHAR_MAX;				/* no more grouping */
  end:
    *o++ = '\0';
    free(*valp);
    if ( (*valp = strdup(s)) )
      return TRUE;
    return PL_no_memory();
  }

  return FALSE;
}


/** locale_create(-Locale, +Default, +Options) is det.
*/

static
PRED_IMPL("locale_create", 3, locale_create, 0)
{ PRED_LD
  PL_locale *def, *new = NULL;
  char *lname;

  if ( PL_get_chars(A2, &lname, CVT_LIST|CVT_STRING|REP_MB) )
  { const char *old;

    PL_LOCK(L_LOCALE);
    if ( (old=setlocale(LC_NUMERIC, lname)) )
    { new = new_locale(NULL);
      setlocale(LC_NUMERIC, old);
    }
    PL_UNLOCK(L_LOCALE);
    if ( !old )
    { if ( errno == ENOENT )
	return PL_existence_error("locale", A2);
      else
	return PL_error(NULL, 0, MSG_ERRNO, ERR_SYSCALL, "setlocale");
    }
  } else
  { if ( !getLocaleEx(A2, &def) )
      return FALSE;
    new = new_locale(def);
    releaseLocale(def);
  }

  if ( new )
  { atom_t alias = 0;
    term_t tail = PL_copy_term_ref(A3);
    term_t head = PL_new_term_ref();
    term_t arg  = PL_new_term_ref();

    while(PL_get_list_ex(tail, head, tail))
    { atom_t pname;
      size_t parity;

      if ( !PL_get_name_arity(head, &pname, &parity) ||
	   parity != 1 ||
	   !PL_get_arg(1, head, arg) )
      { PL_type_error("locale_property", head);
	goto error;
      }
      if ( pname == ATOM_alias )
      { if ( !PL_get_atom_ex(arg, &alias) )
	  goto error;
      } else if ( pname == ATOM_decimal_point )
      { if ( !set_chars(arg, &new->decimal_point) )
	  goto error;
      } else if ( pname == ATOM_thousands_sep )
      { if ( !set_chars(arg, &new->thousands_sep) )
	  goto error;
      } else if ( pname == ATOM_grouping )
      { if ( !set_grouping(arg, &new->grouping) )
	  goto error;
      }
    }
    if ( !PL_get_nil_ex(tail) )
    {
    error:
      free_locale(new);
      return FALSE;
    }

    if ( alias && !alias_locale(new, alias) )
      goto error;

    return unifyLocale(A1, new, TRUE);
  } else
  { return PL_no_memory();
  }
}


static
PRED_IMPL("locale_destroy", 1, locale_destroy, 0)
{ GET_LD
  PL_locale *l;

  if ( getLocaleEx(A1, &l) )
  { if ( l->alias )
    { atom_t alias = l->alias;

      PL_LOCK(L_LOCALE);
      if ( lookupHTable(GD->locale.localeTable, (void*)alias) )
	deleteHTable(GD->locale.localeTable, (void*)alias);
      l->alias = 0;
      PL_unregister_atom(alias);
      PL_UNLOCK(L_LOCALE);
    }

    releaseLocale(l);
    return TRUE;
  }

  return FALSE;
}


/** set_locale(+Locale) is det.
*/

static
PRED_IMPL("set_locale", 1, set_locale, 0)
{ PRED_LD
  PL_locale *l;

  if ( getLocaleEx(A1, &l) )
  { PL_locale *ol = LD->locale.current;

    if ( l != ol )
    { IOSTREAM **sp;

      LD->locale.current = l;		/* already acquired */
      if ( ol )
	releaseLocale(ol);

      if ( (sp=_PL_streams()) )		/* set locale of standard streams */
      { int i;

	for(i=0; i<5; i++)
	  Ssetlocale(sp[i], l, NULL);
      }
    }

    return TRUE;
  }

  return FALSE;
}


/** current_locale(-Locale) is det.
*/

static
PRED_IMPL("current_locale", 1, current_locale, 0)
{ PRED_LD

  if ( LD->locale.current )
    return unifyLocale(A1, LD->locale.current, TRUE);

  return FALSE;
}



		 /*******************************
		 *	     C INTERFACE	*
		 *******************************/


static void
initDefaultsStreamsLocale(PL_locale *l)
{ IOSTREAM *s = S__getiob();
  int i;

  for(i=0; i<2; i++, s++)
  { if ( !s->locale )
      s->locale = acquireLocale(l);
  }
}


void
initLocale(void)
{ GET_LD
  PL_locale *def;

  if ( !setlocale(LC_NUMERIC, "") )
  { DEBUG(0, Sdprintf("Failed to set LC_NUMERIC locale\n"));
  }

  if ( (def = new_locale(NULL)) )
  { alias_locale(def, ATOM_default);
    def->references++;
    GD->locale.default_locale = def;
    LD->locale.current = acquireLocale(def);

    initDefaultsStreamsLocale(def);
  }
}


void
updateLocale(int category, const char *locale)
{ update_locale(GD->locale.default_locale, category, locale);
}


int
initStreamLocale(IOSTREAM *s)
{ GET_LD
  PL_locale *l;

  if ( HAS_LD )					/* a Prolog thread */
    l = LD->locale.current;
  else
    l = GD->locale.default_locale;

  if ( l )
    s->locale = acquireLocale(l);

  return TRUE;
}


PL_locale *
acquireLocale(PL_locale *l)
{ PL_LOCK(L_LOCALE);
  l->references++;
  PL_UNLOCK(L_LOCALE);

  return l;
}


void
releaseLocale(PL_locale *l)
{ PL_LOCK(L_LOCALE);
  if ( --l->references == 0 && l->symbol == 0 && l->alias == 0 )
    free_locale(l);
  PL_UNLOCK(L_LOCALE);
}




		 /*******************************
		 *      PUBLISH PREDICATES	*
		 *******************************/

BeginPredDefs(locale)
  PRED_DEF("locale_property", 2, locale_property, PL_FA_NONDETERMINISTIC)
  PRED_DEF("locale_create",   3, locale_create,   0)
  PRED_DEF("locale_destroy",  1, locale_destroy,  0)
  PRED_DEF("set_locale",      1, set_locale,      0)
  PRED_DEF("current_locale",  1, current_locale,  0)
EndPredDefs

#endif /*O_LOCALE*/
