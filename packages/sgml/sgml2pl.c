/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#include "dtd.h"
#include "catalog.h"
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <errno.h>
#include "error.h"
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif

#define streq(s1, s2) (strcmp(s1, s2) == 0)

#define MAX_ERRORS	50
#define MAX_WARNINGS	50

		 /*******************************
		 *     PARSER CONTEXT DATA	*
		 *******************************/

#define PD_MAGIC	0x36472ba1	/* just a number */

typedef enum
{ SA_FILE = 0,				/* Stop at end-of-file */
  SA_ELEMENT,				/* Stop after first element */
  SA_CONTENT				/* Stop after close */
} stopat;


typedef struct _env
{ term_t	tail;
  struct _env *parent;
} env;


typedef struct _parser_data
{ int	      magic;			/* PD_MAGIC */
  dtd_parser *parser;			/* parser itself */

  int	      warnings;			/* #warnings seen */
  int	      errors;			/* #errors seen */
  int	      max_errors;		/* error limit */
  int	      max_warnings;		/* warning limit */

  predicate_t on_begin;			/* begin element */
  predicate_t on_end;			/* end element */
  predicate_t on_cdata;			/* cdata */
  predicate_t on_entity;		/* entity */
  predicate_t on_xmlns;			/* xmlns */
  predicate_t on_urlns;			/* url --> namespace */

  stopat      stopat;			/* Where to stop */
  int	      stopped;			/* Environment is complete */

  IOSTREAM*   source;			/* Where we are reading from */

  term_t      list;			/* output term (if any) */
  term_t      tail;			/* tail of the list */
  env 	     *stack;			/* environment stack */
  int	      free_on_close;		/* free parser on close */
} parser_data;


		 /*******************************
		 *	      CONSTANTS		*
		 *******************************/

static functor_t FUNCTOR_and2;
static functor_t FUNCTOR_bar2;
static functor_t FUNCTOR_comma2;
static functor_t FUNCTOR_default1;
static functor_t FUNCTOR_dialect1;
static functor_t FUNCTOR_document1;
static functor_t FUNCTOR_dtd1;
static functor_t FUNCTOR_dtd2;
static functor_t FUNCTOR_element1;
static functor_t FUNCTOR_element3;
static functor_t FUNCTOR_entity1;
static functor_t FUNCTOR_equal2;
static functor_t FUNCTOR_file1;
static functor_t FUNCTOR_fixed1;
static functor_t FUNCTOR_goal1;
static functor_t FUNCTOR_line1;
static functor_t FUNCTOR_list1;
static functor_t FUNCTOR_max_errors1;
static functor_t FUNCTOR_nameof1;
static functor_t FUNCTOR_omit2;
static functor_t FUNCTOR_opt1;
static functor_t FUNCTOR_plus1;
static functor_t FUNCTOR_rep1;
static functor_t FUNCTOR_sgml_parser1;
static functor_t FUNCTOR_parse1;
static functor_t FUNCTOR_source1;
static functor_t FUNCTOR_call2;
static functor_t FUNCTOR_charpos1;
static functor_t FUNCTOR_ns2;		/* :/2 */
static functor_t FUNCTOR_space1;

static atom_t ATOM_sgml;
static atom_t ATOM_dtd;
static atom_t ATOM_true;
static atom_t ATOM_false;
static atom_t ATOM_cdata;
static atom_t ATOM_pcdata;
static atom_t ATOM_empty;

#define mkfunctor(n, a) PL_new_functor(PL_new_atom(n), a)

static void
initConstants()
{ FUNCTOR_sgml_parser1 = mkfunctor("sgml_parser", 1);
  FUNCTOR_equal2       = mkfunctor("=", 2);
  FUNCTOR_dtd1	       = mkfunctor("dtd", 1);
  FUNCTOR_element1     = mkfunctor("element", 1);
  FUNCTOR_element3     = mkfunctor("element", 3);
  FUNCTOR_entity1      = mkfunctor("entity", 1);
  FUNCTOR_document1    = mkfunctor("document", 1);
  FUNCTOR_goal1	       = mkfunctor("goal", 1);
  FUNCTOR_dtd2	       = mkfunctor("dtd", 2);
  FUNCTOR_omit2	       = mkfunctor("omit", 2);
  FUNCTOR_and2	       = mkfunctor("&", 2);
  FUNCTOR_comma2       = mkfunctor(",", 2);
  FUNCTOR_bar2         = mkfunctor("|", 2);
  FUNCTOR_opt1         = mkfunctor("?", 1);
  FUNCTOR_rep1         = mkfunctor("*", 1);
  FUNCTOR_plus1	       = mkfunctor("+", 1);
  FUNCTOR_default1     = mkfunctor("default", 1);
  FUNCTOR_fixed1       = mkfunctor("fixed", 1);
  FUNCTOR_list1        = mkfunctor("list", 1);
  FUNCTOR_nameof1      = mkfunctor("nameof", 1);
  FUNCTOR_file1        = mkfunctor("file", 1);
  FUNCTOR_line1        = mkfunctor("line", 1);
  FUNCTOR_dialect1     = mkfunctor("dialect", 1);
  FUNCTOR_max_errors1  = mkfunctor("max_errors", 1);
  FUNCTOR_parse1       = mkfunctor("parse", 1);
  FUNCTOR_source1      = mkfunctor("source", 1);
  FUNCTOR_call2	       = mkfunctor("call", 2);
  FUNCTOR_charpos1     = mkfunctor("charpos", 1);
  FUNCTOR_ns2	       = mkfunctor(":", 2);
  FUNCTOR_space1       = mkfunctor("space", 1);

  ATOM_dtd  = PL_new_atom("dtd");
  ATOM_sgml = PL_new_atom("sgml");
  ATOM_true = PL_new_atom("true");
  ATOM_false = PL_new_atom("false");
  ATOM_cdata = PL_new_atom("cdata");
  ATOM_pcdata = PL_new_atom("#pcdata");
  ATOM_empty = PL_new_atom("empty");
}

		 /*******************************
		 *	       ACCESS		*
		 *******************************/

static int
unify_parser(term_t parser, dtd_parser *p)
{ return PL_unify_term(parser, PL_FUNCTOR, FUNCTOR_sgml_parser1,
		         PL_POINTER, p);
}


static int
get_parser(term_t parser, dtd_parser **p)
{ if ( PL_is_functor(parser, FUNCTOR_sgml_parser1) )
  { term_t a = PL_new_term_ref();
    dtd_parser *tmp;

    PL_get_arg(1, parser, a);
    if ( PL_get_pointer(a, (void **)&tmp) )
    { if ( tmp->magic == SGML_PARSER_MAGIC )
      { *p = tmp;

        return TRUE;
      }
      return sgml2pl_error(ERR_EXISTENCE, "sgml_parser", parser);
    }
  }

  return sgml2pl_error(ERR_TYPE, "sgml_parser", parser);
}


static int
unify_dtd(term_t t, dtd *dtd)
{ if ( dtd->doctype )
    return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_dtd2,
			 PL_POINTER, dtd,
			 PL_CHARS, dtd->doctype);
  else
    return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_dtd2,
			 PL_POINTER, dtd,
			 PL_VARIABLE);
}


static int
get_dtd(term_t t, dtd **dtdp)
{ if ( PL_is_functor(t, FUNCTOR_dtd2) )
  { term_t a = PL_new_term_ref();
    dtd *tmp;

    PL_get_arg(1, t, a);
    if ( PL_get_pointer(a, (void **)&tmp) )
    { if ( tmp->magic == SGML_DTD_MAGIC )
      { *dtdp = tmp;

        return TRUE;
      }
      return sgml2pl_error(ERR_EXISTENCE, "dtd", t);
    }
  }

  return sgml2pl_error(ERR_TYPE, "dtd", t);
}


		 /*******************************
		 *	      NEW/FREE		*
		 *******************************/

static foreign_t
pl_new_sgml_parser(term_t ref, term_t options)
{ term_t head = PL_new_term_ref();
  term_t tail = PL_copy_term_ref(options);
  term_t tmp  = PL_new_term_ref();

  dtd *dtd = NULL;
  dtd_parser *p;

  while ( PL_get_list(tail, head, tail) )
  { if ( PL_is_functor(head, FUNCTOR_dtd1) )
    { PL_get_arg(1, head, tmp);

      if ( PL_is_variable(tmp) )	/* dtd(X) */
      { dtd = new_dtd(NULL);		/* no known doctype */
	dtd->references++;
	unify_dtd(tmp, dtd);
      } else if ( !get_dtd(tmp, &dtd) )
	return FALSE;
    }
  }
  if ( !PL_get_nil(tail) )
    return sgml2pl_error(ERR_TYPE, "list", tail);

  p = new_dtd_parser(dtd);

  return unify_parser(ref, p);
}


static foreign_t
pl_free_sgml_parser(term_t parser)
{ dtd_parser *p;

  if ( get_parser(parser, &p) )
  { free_dtd_parser(p);
    return TRUE;
  }

  return FALSE;
}


static foreign_t
pl_new_dtd(term_t doctype, term_t ref)
{ char *dt;
  dtd *dtd;

  if ( !PL_get_atom_chars(doctype, &dt) )
    return sgml2pl_error(ERR_TYPE, "atom", doctype);

  if ( !(dtd=new_dtd(dt)) )
    return FALSE;

  dtd->references++;

  return unify_dtd(ref, dtd);
}


static foreign_t
pl_free_dtd(term_t t)
{ dtd *dtd;

  if ( get_dtd(t, &dtd) )
  { free_dtd(dtd);
    return TRUE;
  }

  return FALSE;
}


		 /*******************************
		 *	    PROPERTIES		*
		 *******************************/

static foreign_t
pl_set_sgml_parser(term_t parser, term_t option)
{ dtd_parser *p;

  if ( !get_parser(parser, &p) )
    return FALSE;

  if ( PL_is_functor(option, FUNCTOR_file1) )
  { term_t a = PL_new_term_ref();
    char *file;

    PL_get_arg(1, option, a);
    if ( !PL_get_atom_chars(a, &file) )
      return sgml2pl_error(ERR_TYPE, "atom", a);
    set_file_dtd_parser(p, file);
  } else if ( PL_is_functor(option, FUNCTOR_line1) )
  { term_t a = PL_new_term_ref();

    PL_get_arg(1, option, a);
    if ( !PL_get_integer(a, &p->location.line) )
      return sgml2pl_error(ERR_TYPE, "integer", a);
  } else if ( PL_is_functor(option, FUNCTOR_dialect1) )
  { term_t a = PL_new_term_ref();
    char *s;

    PL_get_arg(1, option, a);
    if ( !PL_get_atom_chars(a, &s) )
      return sgml2pl_error(ERR_TYPE, "atom", a);

    if ( streq(s, "xml") )
      set_dialect_dtd(p->dtd, DL_XML);
    else if ( streq(s, "xmlns") )
      set_dialect_dtd(p->dtd, DL_XMLNS);
    else if ( streq(s, "sgml") )
      set_dialect_dtd(p->dtd, DL_SGML);
    else
      return sgml2pl_error(ERR_DOMAIN, "sgml_dialect", a);
  } else if ( PL_is_functor(option, FUNCTOR_space1) )
  { term_t a = PL_new_term_ref();
    char *s;

    PL_get_arg(1, option, a);
    if ( !PL_get_atom_chars(a, &s) )
      return sgml2pl_error(ERR_TYPE, "atom", a);

    if ( streq(s, "preserve") )
      p->space_mode = SP_PRESERVE;
    else if ( streq(s, "default") )
      p->space_mode = SP_DEFAULT;
    else if ( streq(s, "remove") )
      p->space_mode = SP_REMOVE;

    else
      return sgml2pl_error(ERR_DOMAIN, "space", a);
  } else
    return sgml2pl_error(ERR_DOMAIN, "sgml_parser_option", option);

  return TRUE;
}


static foreign_t
pl_get_sgml_parser(term_t parser, term_t option)
{ dtd_parser *p;

  if ( !get_parser(parser, &p) )
    return FALSE;

  if ( PL_is_functor(option, FUNCTOR_charpos1) )
  { term_t a = PL_new_term_ref();

    PL_get_arg(1, option, a);
    return PL_unify_integer(a, p->startloc.charpos);
  } else if ( PL_is_functor(option, FUNCTOR_file1) )
  { if ( p->startloc.file )
    { term_t a = PL_new_term_ref();

      PL_get_arg(1, option, a);
      return PL_unify_atom_chars(a, p->startloc.file);
    }
  } else if ( PL_is_functor(option, FUNCTOR_source1) )
  { parser_data *pd = p->closure;

    if ( pd && pd->magic == PD_MAGIC && pd->source )
    { term_t a = PL_new_term_ref();

      PL_get_arg(1, option, a);
      return PL_unify_stream(a, pd->source);
    }
  } else if ( PL_is_functor(option, FUNCTOR_dialect1) )
  { term_t a = PL_new_term_ref();

    PL_get_arg(1, option, a);
    switch(p->dtd->dialect)
    { case DL_SGML:
	return PL_unify_atom_chars(a, "sgml");
      case DL_XML:
	return PL_unify_atom_chars(a, "xml");
      case DL_XMLNS:
	return PL_unify_atom_chars(a, "xmlns");
    }
  } else
    return sgml2pl_error(ERR_DOMAIN, "parser_option", option);

  return FALSE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
put_url(dtd_parser *p, term_t t, const ichar *url)
    Store the url-part of a name-space qualifier in term.  We call
    xml:xmlns(-Canonical, +Full) trying to resolve the specified
    namespace to an internal canonical namespace.

    We do a little caching as there will generally be only a very
    small pool of urls in use.  We assume the url-pointers we get
    life for the time of the parser.  It might be possible that
    multiple url pointers point to the same url, but this only clobbers
    the cache a little.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define URL_CACHE 4			/* # entries cached */

typedef struct
{ const ichar *url;			/* URL pointer */
  atom_t canonical;
} url_cache;

static url_cache cache[URL_CACHE];

static void
reset_url_cache()
{ int i;
  url_cache *c = cache;

  for(i=0; i<URL_CACHE; i++)
  { c[i].url = NULL;
    if ( c[i].canonical )
      PL_unregister_atom(c[i].canonical);
    c[i].canonical = 0;
  }
}


static void
put_url(dtd_parser *p, term_t t, const ichar *url)
{ parser_data *pd = p->closure;
  int i;

  if ( !pd->on_urlns )
  { PL_put_atom_chars(t, url);
    return;
  }

  for(i=0; i<URL_CACHE; i++)
  { if ( cache[i].url == url )		/* cache hit */
    { if ( cache[i].canonical )		/* and a canonical value */
	PL_put_atom(t, cache[i].canonical);
      else
	PL_put_atom_chars(t, url);

      return;
    }
  }
					/* shift the cache */
  i = URL_CACHE-1;
  if ( cache[i].canonical )
    PL_unregister_atom(cache[i].canonical);
  for(i=URL_CACHE-1; i>0; i--)
    cache[i] = cache[i-1];
  cache[0].url = url;
  cache[0].canonical = 0;

  { fid_t fid = PL_open_foreign_frame();
    term_t av = PL_new_term_refs(3);
    atom_t a;

    PL_put_atom_chars(av+0, url);
    unify_parser(av+2, p);
    if ( PL_call_predicate(NULL, PL_Q_NORMAL, pd->on_urlns, av) &&
	 PL_get_atom(av+1, &a) )
    { PL_register_atom(a);
      cache[0].canonical = a;
      PL_put_atom(t, a);
    } else
    { PL_put_atom_chars(t, url);
    }
    PL_discard_foreign_frame(fid);
  }
}


static void
put_name(dtd_parser *p, term_t t, dtd_symbol *nm)
{ const ichar *url, *local;

  if ( p->dtd->dialect == DL_XMLNS )
  { xmlns_resolve_attribute(p, nm, &local, &url);

    if ( url )
    { term_t av = PL_new_term_refs(2);
    
      put_url(p, av+0, url);
      PL_put_atom_chars(av+1, local);
      PL_cons_functor_v(t, FUNCTOR_ns2, av);
    } else
      PL_put_atom_chars(t, local);
  } else
    PL_put_atom_chars(t, nm->name);
}


static void
put_element_name(dtd_parser *p, term_t t, dtd_element *e)
{ const ichar *url, *local;

  if ( p->dtd->dialect == DL_XMLNS )
  { assert(p->environments->element == e);
    xmlns_resolve_element(p, &local, &url);

    if ( url )
    { term_t av = PL_new_term_refs(2);
    
      put_url(p, av+0, url);
      PL_put_atom_chars(av+1, local);
      PL_cons_functor_v(t, FUNCTOR_ns2, av);
    } else
      PL_put_atom_chars(t, local);
  } else
    PL_put_atom_chars(t, e->name->name);
}


static int
unify_attribute_list(dtd_parser *p, term_t alist,
		     int argc, sgml_attribute *argv)
{ int i;
  term_t tail = PL_copy_term_ref(alist);
  term_t h    = PL_new_term_ref();
  term_t a    = PL_new_term_refs(2);

  for(i=0; i<argc; i++)
  { put_name(p, a+0, argv[i].definition->name);

    switch(argv[i].definition->type)
    { case AT_CDATA:
	PL_put_atom_chars(a+1, argv[i].value.cdata);
        break;
      case AT_NUMBER:
	PL_put_integer(a+1, argv[i].value.number);
        break;
      default:
	PL_put_atom_chars(a+1, argv[i].value.text);
        break;
    }
    PL_cons_functor_v(a, FUNCTOR_equal2, a);
    if ( !PL_unify_list(tail, h, tail) ||
	 !PL_unify(h, a) )
      return FALSE;
  }

  if ( PL_unify_nil(tail) )
  { PL_reset_term_refs(tail);

    return TRUE;
  }

  return FALSE;
}



static int
on_begin(dtd_parser *p, dtd_element *e, int argc, sgml_attribute *argv)
{ parser_data *pd = p->closure;

  if ( pd->stopped )
    return TRUE;

  if ( pd->tail )
  { term_t content = PL_new_term_ref();	/* element content */
    term_t alist   = PL_new_term_ref();	/* attribute list */
    term_t et	   = PL_new_term_ref();	/* element structure */
    term_t h       = PL_new_term_ref();

    put_element_name(p, h, e);
    unify_attribute_list(p, alist, argc, argv);
    PL_unify_term(et, PL_FUNCTOR, FUNCTOR_element3,
		    PL_TERM, h,
		    PL_TERM, alist,
		    PL_TERM, content);
    if ( PL_unify_list(pd->tail, h, pd->tail) &&
	 PL_unify(h, et) )
    { env *env = calloc(1, sizeof(env));

      env->tail   = pd->tail;
      env->parent = pd->stack;
      pd->stack   = env;

      pd->tail = content;
      PL_reset_term_refs(alist);
    }

    return TRUE;
  }

  if ( pd->on_begin )
  { fid_t fid = PL_open_foreign_frame();
    term_t av = PL_new_term_refs(3);

    put_element_name(p, av+0, e);
    unify_attribute_list(p, av+1, argc, argv);
    unify_parser(av+2, p);

    PL_call_predicate(NULL, PL_Q_NORMAL, pd->on_begin, av);
    PL_discard_foreign_frame(fid);
  }

  return TRUE;
}


static int
on_end(dtd_parser *p, dtd_element *e)
{ parser_data *pd = p->closure;

  if ( pd->on_end )
  { fid_t fid = PL_open_foreign_frame();
    term_t av = PL_new_term_refs(2);

    PL_put_atom_chars(av+0, e->name->name);
    unify_parser(av+1, p);

    PL_call_predicate(NULL, PL_Q_NORMAL, pd->on_end, av);
    PL_discard_foreign_frame(fid);
  }

  if ( pd->tail && !pd->stopped )
  { PL_unify_nil(pd->tail);
    PL_reset_term_refs(pd->tail);	/* ? */

    if ( pd->stack )
    { env *parent = pd->stack->parent;

      pd->tail = pd->stack->tail;
      free(pd->stack);
      pd->stack = parent;

      if ( !parent && pd->stopat == SA_ELEMENT )
	pd->stopped = TRUE;
    } else
    { if ( pd->stopat == SA_CONTENT )
	pd->stopped = TRUE;
    }
  }

  return TRUE;
}


static int
on_entity(dtd_parser *p, dtd_entity *e, int chr)
{ parser_data *pd = p->closure;

  if ( pd->on_entity )
  { fid_t fid = PL_open_foreign_frame();
    term_t av = PL_new_term_refs(2);

    if ( e )
      PL_put_atom_chars(av+0, e->name->name);
    else
      PL_put_integer(av+0, chr);

    unify_parser(av+1, p);

    PL_call_predicate(NULL, PL_Q_NORMAL, pd->on_end, av);
    PL_discard_foreign_frame(fid);
  }

  if ( pd->tail && !pd->stopped )
  { term_t h = PL_new_term_ref();

    if ( !PL_unify_list(pd->tail, h, pd->tail) )
      return FALSE;

    if ( e )
      PL_unify_term(h,
		    PL_FUNCTOR, FUNCTOR_entity1,
		    PL_CHARS, e->name->name);
    else
      PL_unify_term(h,
		    PL_FUNCTOR, FUNCTOR_entity1,
		    PL_INTEGER, chr);
			 
    PL_reset_term_refs(h);
  }

  return TRUE;
}


static int
on_cdata(dtd_parser *p, int len, const ochar *data)
{ parser_data *pd = p->closure;

  if ( pd->on_cdata )
  { fid_t fid = PL_open_foreign_frame();
    term_t av = PL_new_term_refs(2);

    PL_put_atom_nchars(av+0, len, data);
    unify_parser(av+1, p);

    PL_call_predicate(NULL, PL_Q_NORMAL, pd->on_cdata, av);
    PL_discard_foreign_frame(fid);
  }

  if ( pd->tail && !pd->stopped )
  { term_t h = PL_new_term_ref();

    if ( PL_unify_list(pd->tail, h, pd->tail) &&
	 PL_unify_atom_nchars(h, len, data) )
    { PL_reset_term_refs(h);
      return TRUE;
    }
  }

  return FALSE;
}


static int
on_error(dtd_parser *p, dtd_error *error)
{ parser_data *pd = p->closure;
  const char *severity;

  if ( pd->stopped )
    return TRUE;

  switch(error->severity)
  { case ERS_WARNING:
      pd->warnings++;
      severity = "warning";
      break;
    case ERS_ERROR:
    default:				/* make compiler happy */
      pd->errors++;
      severity = "error";
      break;
  }

  { fid_t fid = PL_open_foreign_frame();
    predicate_t pred = PL_predicate("print_message", 2, "user");
    term_t av = PL_new_term_refs(2);
    term_t parser = PL_new_term_ref();

    unify_parser(parser, p);
    PL_put_atom_chars(av+0, severity);

    PL_unify_term(av+1,
		  PL_FUNCTOR_CHARS, "sgml", 4,
		    PL_TERM, parser,
		    PL_CHARS, error->file ? error->file : "[]",
		    PL_INTEGER, error->line,
		    PL_CHARS, error->plain_message);

    PL_call_predicate(NULL, PL_Q_NODEBUG, pred, av);

    PL_discard_foreign_frame(fid);
  }

  return TRUE;
}


static int
on_xmlns(dtd_parser *p, dtd_symbol *ns, dtd_symbol *url)
{ parser_data *pd = p->closure;

  if ( pd->stopped )
    return TRUE;

  if ( pd->on_xmlns )
  { fid_t fid = PL_open_foreign_frame();
    term_t av = PL_new_term_refs(3);

    if ( ns )
      PL_put_atom_chars(av+0, ns->name);
    else
      PL_put_nil(av+0);
    PL_put_atom_chars(av+1, url->name);
    unify_parser(av+2, p);

    PL_call_predicate(NULL, PL_Q_NORMAL, pd->on_xmlns, av);
    PL_discard_foreign_frame(fid);
  }

  return TRUE;
}


static int
write_parser(void *h, char *buf, int len)
{ parser_data *pd = h;
  unsigned char *s = (unsigned char *)buf;
  unsigned char *e = s+len;

  if ( !pd->parser || pd->parser->magic != SGML_PARSER_MAGIC )
  { errno = EINVAL;
    return -1;
  }
  
  if ( pd->errors > pd->max_errors || pd->stopped )
  { errno = EIO;
    return -1;
  }

  for(; s<e; s++)
    putchar_dtd_parser(pd->parser, *s);

  return len;
}


static int
close_parser(void *h)
{ parser_data *pd = h;
  dtd_parser *p;

  if ( !(p=pd->parser) || p->magic != SGML_PARSER_MAGIC )
  { errno = EINVAL;
    return -1;
  }

  if ( pd->tail )
    PL_unify_nil(pd->tail);

  if ( p->dmode == DM_DTD )
    p->dtd->implicit = FALSE;		/* assume we loaded a DTD */

  if ( pd->free_on_close )
    free_dtd_parser(p);
  else
    p->closure = NULL;

  free(pd);

  return 0;
}


static IOFUNCTIONS sgml_stream_functions =
{ (Sread_function)  NULL,
  (Swrite_function) write_parser,
  (Sseek_function)  NULL,
  (Sclose_function) close_parser,
		    NULL
};


static foreign_t
pl_open_dtd(term_t ref, term_t options, term_t stream)
{ dtd *dtd;
  dtd_parser *p;
  parser_data *pd;
  IOSTREAM *s;

  if ( !get_dtd(ref, &dtd) )
    return FALSE;
  p = new_dtd_parser(dtd);
  
  pd = calloc(1, sizeof(*pd));
  pd->parser = p;
  pd->free_on_close = TRUE;
  pd->max_errors = MAX_ERRORS;
  pd->max_warnings = MAX_WARNINGS;
  p->closure = pd;
  p->dmode = DM_DTD;

  s = Snew(pd, SIO_OUTPUT, &sgml_stream_functions);

  if ( !PL_open_stream(stream, s) )
    return FALSE;

  return TRUE;
}


static int
set_callback_predicates(parser_data *pd, term_t option)
{ term_t a = PL_new_term_ref();
  char *fname;
  atom_t pname;
  predicate_t *pp = NULL;		/* keep compiler happy */
  int arity;
  module_t m = NULL;

  PL_get_arg(2, option, a);
  PL_strip_module(a, &m, a);
  if ( !PL_get_atom(a, &pname) )
    return sgml2pl_error(ERR_TYPE, "atom", a);
  PL_get_arg(1, option, a);
  if ( !PL_get_atom_chars(a, &fname) )
    return sgml2pl_error(ERR_TYPE, "atom", a);
  
  if ( streq(fname, "begin") )
  { pp = &pd->on_begin;			/* tag, attributes, parser */
    arity = 3;
  } else if ( streq(fname, "end") )
  { pp = &pd->on_end;			/* tag, parser */
    arity = 2;
  } else if ( streq(fname, "cdata") )
  { pp = &pd->on_cdata;			/* cdata, parser */
    arity = 2;
  } else if ( streq(fname, "entity") )
  { pp = &pd->on_entity;		/* name, parser */
    arity = 2;
  } else if ( streq(fname, "xmlns") )
  { pp = &pd->on_xmlns;			/* ns, url, parser */
    arity = 3;
  } else if ( streq(fname, "urlns") )
  { pp = &pd->on_urlns;			/* url, ns, parser */
    arity = 3;
  } else
    return sgml2pl_error(ERR_DOMAIN, "sgml_callback", a);

  *pp = PL_pred(PL_new_functor(pname, arity), m);
  return TRUE;
}


static foreign_t
pl_sgml_parse(term_t parser, term_t options)
{ dtd_parser *p;
  parser_data *pd;
  parser_data *oldpd;
  term_t head = PL_new_term_ref();
  term_t tail = PL_copy_term_ref(options);
  term_t goal = 0;
  IOSTREAM *in = NULL, *s = NULL;
  int recursive;

  if ( !get_parser(parser, &p) )
    return FALSE;

  if ( p->closure )			/* recursive call */
  { recursive = TRUE;

    oldpd = p->closure;
    if ( oldpd->magic != PD_MAGIC || oldpd->parser != p )
      return sgml2pl_error(ERR_MISC, "sgml", "Parser associated with illegal data");
    
    pd = calloc(1, sizeof(*pd));
    *pd = *oldpd;
    p->closure = pd;

    in = pd->source;
  } else
  { recursive = FALSE;
    oldpd = NULL;			/* keep compiler happy */

    p->on_begin_element = on_begin;
    p->on_end_element   = on_end;
    p->on_entity	= on_entity;
    p->on_cdata         = on_cdata;
    p->on_error	        = on_error;
    p->on_xmlns		= on_xmlns;
    p->dmode	        = DM_SGML;
    p->state	        = S_PCDATA;
  
    pd = calloc(1, sizeof(*pd));
    pd->magic = PD_MAGIC;
    pd->parser = p;
    pd->max_errors = MAX_ERRORS;
    pd->max_warnings = MAX_WARNINGS;
    p->closure = pd;
  }

  while ( PL_get_list(tail, head, tail) )
  { if ( PL_is_functor(head, FUNCTOR_document1) )
    { pd->list  = PL_new_term_ref();
      PL_get_arg(1, head, pd->list);
      pd->tail  = PL_copy_term_ref(pd->list);
      pd->stack = NULL;
    } else if ( PL_is_functor(head, FUNCTOR_goal1) )
    { goal = PL_new_term_ref();

      PL_get_arg(1, head, goal);
    } else if ( PL_is_functor(head, FUNCTOR_source1) )
    { term_t a = PL_new_term_ref();

      PL_get_arg(1, head, a);
      if ( !PL_get_stream_handle(a, &in) )
	return FALSE;
    } else if ( PL_is_functor(head, FUNCTOR_call2) )
    { if ( !set_callback_predicates(pd, head) )
	return FALSE;
    } else if ( PL_is_functor(head, FUNCTOR_parse1) )
    { term_t a = PL_new_term_ref();
      char *s;

      PL_get_arg(1, head, a);
      if ( !PL_get_atom_chars(a, &s) )
	return sgml2pl_error(ERR_TYPE, "atom", a);
      if ( streq(s, "element") )
	pd->stopat = SA_ELEMENT;
      else if ( streq(s, "content") )
	pd->stopat = SA_CONTENT;
      else if ( streq(s, "file") )
	pd->stopat = SA_FILE;
      else
	return sgml2pl_error(ERR_DOMAIN, "parse", a);
    } else if ( PL_is_functor(head, FUNCTOR_max_errors1) )
    { term_t a = PL_new_term_ref();

      PL_get_arg(1, head, a);
      if ( !PL_get_integer(a, &pd->max_errors) )
	return sgml2pl_error(ERR_TYPE, "integer", a);
    } else
      return sgml2pl_error(ERR_DOMAIN, "sgml_option", head);
  }
  if ( !PL_get_nil(tail) )
    return sgml2pl_error(ERR_TYPE, "list", tail);

					/* Parsing input from a stream */
  if ( in )
  { int chr;

    if ( !recursive )
    { pd->source = in;
      begin_document_dtd_parser(p);
    }
    while( (chr = Sgetc(in)) != EOF )
    { putchar_dtd_parser(p, chr);

      if ( pd->errors > pd->max_errors )
	return sgml2pl_error(ERR_LIMIT, "max_errors", (long)pd->max_errors);
      if ( pd->stopped )
      { pd->stopped = FALSE;
	goto out;
      }
    }
    if ( !recursive )
      end_document_dtd_parser(p);

  out:
    reset_url_cache();
    if ( pd->tail )
      PL_unify_nil(pd->tail);

    if ( recursive )
    { p->closure = oldpd;
    } else
    { p->closure = NULL;
    }

    pd->magic = 0;			/* invalidate */
    free(pd);

    return TRUE;
  }

					/* Parsing data from a goal */
  if ( goal )
  { fid_t fid = PL_open_foreign_frame();
    term_t av = PL_new_term_refs(2);
    predicate_t pred = PL_predicate("call", 2, "user");
    qid_t qid;
    int rval;

    PL_put_term(av+0, goal);
    s = Snew(pd, SIO_OUTPUT, &sgml_stream_functions);
    PL_open_stream(av+1, s);

    begin_document_dtd_parser(p);
    qid = PL_open_query(NULL, PL_Q_CATCH_EXCEPTION, pred, av);
    rval = PL_next_solution(qid);
    PL_cut_query(qid);
    if ( rval &&
	 Sflush(s) == 0 &&
	 end_document_dtd_parser(p) )
    { Sclose(s);
      PL_close_foreign_frame(fid);
      return TRUE;
    }
    PL_discard_foreign_frame(fid);
    Sclose(s);

    if ( pd->errors > pd->max_errors )
      return sgml2pl_error(ERR_LIMIT, "max_errors", (long)pd->max_errors);

    return sgml2pl_error(ERR_FAIL, goal);
  }
  reset_url_cache();

  return TRUE;
}


		 /*******************************
		 *	  DTD PROPERTIES	*
		 *******************************/

static void	put_model(term_t t, dtd_model *m);

/* doctype(DocType) */

static int
dtd_prop_doctype(dtd *dtd, term_t prop)
{ if ( dtd->doctype )
    return PL_unify_atom_chars(prop, dtd->doctype);
  return FALSE;
}


/* elements(ListOfElements) */

static void
make_model_list(term_t t, dtd_model *m, functor_t f)
{ if ( !m->next )
  { put_model(t, m);
  } else
  { term_t av = PL_new_term_refs(2);

    put_model(av+0, m);
    make_model_list(av+1, m->next, f);
    PL_cons_functor_v(t, f, av);
    PL_reset_term_refs(av);
  }
}


static void
put_model(term_t t, dtd_model *m)
{ functor_t f;

  switch(m->type)
  { case MT_PCDATA:
      PL_put_atom(t, ATOM_pcdata);
      goto card;
    case MT_ELEMENT:
      PL_put_atom_chars(t, m->content.element->name->name);
      goto card;
    case MT_AND:
      f = FUNCTOR_and2;
      break;
    case MT_SEQ:
      f = FUNCTOR_comma2;
      break;
    case MT_OR:
      f = FUNCTOR_bar2;
      break;
    case MT_UNDEF:
    default:
      assert(0);
      f = 0;
      break;
  }

  if ( !m->content.group )
    PL_put_atom(t, ATOM_empty);
  else
    make_model_list(t, m->content.group, f);
  
card:
  switch(m->cardinality)
  { case MC_ONE:
      break;
    case MC_OPT:
      PL_cons_functor_v(t, FUNCTOR_opt1, t);
      break;
    case MC_REP:
      PL_cons_functor_v(t, FUNCTOR_rep1, t);
      break;
    case MC_PLUS:
      PL_cons_functor_v(t, FUNCTOR_plus1, t);
      break;
  }
}


static void
put_content(term_t t, dtd_edef *def)
{ switch(def->type)
  { case C_EMPTY:
      PL_put_atom(t, ATOM_empty);
      return;
    case C_CDATA:
      PL_put_atom(t, ATOM_cdata);
      return;
    default:
      if ( def->content )
	put_model(t, def->content);
  }
}


static int
dtd_prop_elements(dtd *dtd, term_t prop)
{ term_t tail = PL_copy_term_ref(prop);
  term_t head = PL_new_term_ref();
  term_t et   = PL_new_term_ref();
  dtd_element *e;
  
  for( e=dtd->elements; e; e=e->next )
  { PL_put_atom_chars(et, e->name->name);
    if ( !PL_unify_list(tail, head, tail) ||
	 !PL_unify(head, et) )
      return FALSE;
  }

  return PL_unify_nil(tail);
}


static int
get_element(dtd *dtd, term_t name, dtd_element **elem)
{ char *s;
  dtd_element *e;
  dtd_symbol *id;

  if ( !PL_get_atom_chars(name, &s) )
    return sgml2pl_error(ERR_TYPE, "atom", name);

  if ( !(id=dtd_find_symbol(dtd, s)) ||
       !(e=id->element) )
    return FALSE;

  *elem = e;
  return TRUE;
}




static int
dtd_prop_element(dtd *dtd, term_t name, term_t omit, term_t content)
{ dtd_element *e;
  dtd_edef *def;
  term_t model = PL_new_term_ref();

  if ( !get_element(dtd, name, &e) || !(def=e->structure) )
    return FALSE;
  
  if ( !PL_unify_term(omit, PL_FUNCTOR, FUNCTOR_omit2,
		        PL_ATOM, def->omit_open ?  ATOM_true : ATOM_false,
		        PL_ATOM, def->omit_close ? ATOM_true : ATOM_false) )
    return FALSE;

  put_content(model, def);
  return PL_unify(content, model);
}


static int
dtd_prop_attributes(dtd *dtd, term_t ename, term_t atts)
{ dtd_element *e;
  term_t tail = PL_copy_term_ref(atts);
  term_t head = PL_new_term_ref();
  term_t elem = PL_new_term_ref();
  dtd_attr_list *al;

  if ( !get_element(dtd, ename, &e) )
    return FALSE;
  
  for(al=e->attributes; al; al=al->next)
  { PL_put_atom_chars(elem, al->attribute->name->name);

    if ( !PL_unify_list(tail, head, tail) ||
	 !PL_unify(head, elem) )
      return FALSE;
  }

  return PL_unify_nil(tail);
}


typedef struct _plattrdef
{ attrtype	type;			/* AT_* */
  const char *	name;			/* name */
  int	       islist;			/* list-type */
  atom_t	atom;			/* name as atom */
} plattrdef;

static plattrdef plattrs[] = 
{
  { AT_CDATA,	 "cdata",    FALSE },
  { AT_ENTITY,	 "entity",   FALSE },
  { AT_ENTITIES, "entity",   TRUE },
  { AT_ID,	 "id",	     FALSE },
  { AT_IDREF,	 "idref",    FALSE },
  { AT_IDREFS,	 "idref",    TRUE },
  { AT_NAME,	 "name",     FALSE },
  { AT_NAMES,	 "name",     TRUE },
/*{ AT_NAMEOF,	 "nameof",   FALSE },*/
  { AT_NMTOKEN,	 "nmtoken",  FALSE },
  { AT_NMTOKENS, "nmtoken",  TRUE },
  { AT_NOTATION, "notation", FALSE },
  { AT_NUMBER,	 "number",   FALSE },
  { AT_NUMBERS,	 "number",   TRUE },
  { AT_NUTOKEN,	 "nutoken",  FALSE },
  { AT_NUTOKENS, "nutoken",  TRUE },

  { 0, NULL }
};


static int
unify_attribute_type(term_t type, dtd_attr *a)
{ plattrdef *ad = plattrs;

  for( ; ad->name; ad++ )
  { if ( ad->type == a->type )
    { if ( !ad->atom )
	ad->atom = PL_new_atom(ad->name);

      if ( ad->islist )
	return PL_unify_term(type, PL_FUNCTOR, FUNCTOR_list1,
			     PL_ATOM, ad->atom);
      else
	return PL_unify_atom(type, ad->atom);
    }
  }

  if ( a->type == AT_NAMEOF )
  { dtd_name_list *nl;
    term_t tail = PL_new_term_ref();
    term_t head = PL_new_term_ref();
    term_t elem = PL_new_term_ref();

    if ( !PL_unify_functor(type, FUNCTOR_nameof1) )
      return FALSE;
    PL_get_arg(1, type, tail);

    for(nl = a->typeex.nameof; nl; nl = nl->next)
    { PL_put_atom_chars(elem, nl->value->name);
      
      if ( !PL_unify_list(tail, head, tail) ||
	   !PL_unify(head, elem) )
	return FALSE;
    }
    return PL_unify_nil(tail);
  }

  assert(0);
  return FALSE;
}



static int
unify_attribute_default(term_t defval, dtd_attr *a)
{ int v;

  switch(a->def)
  { case AT_REQUIRED:
      return PL_unify_atom_chars(defval, "required");
    case AT_CURRENT:
      return PL_unify_atom_chars(defval, "current");
    case AT_CONREF:
      return PL_unify_atom_chars(defval, "conref");
    case AT_IMPLIED:
      return PL_unify_atom_chars(defval, "implied");
    case AT_DEFAULT:
      v = PL_unify_functor(defval, FUNCTOR_default1);
      goto common;
    case AT_FIXED:
      v = PL_unify_functor(defval, FUNCTOR_fixed1);
    common:
      if ( v )
      { term_t tmp = PL_new_term_ref();

	PL_get_arg(1, defval, tmp);
	switch( a->type )
	{ case AT_CDATA:
	    return PL_unify_atom_chars(tmp, a->att_def.cdata);
	  case AT_NAME:
	  case AT_NMTOKEN:
	  case AT_NAMEOF:
	    return PL_unify_atom_chars(tmp, a->att_def.name->name);
	  case AT_NUMBER:
	    return PL_unify_integer(tmp, a->att_def.number);
	  default:
	    assert(0);
	}
      }
    default:
      assert(0);
      return FALSE;
  }
}


static int
dtd_prop_attribute(dtd *dtd, term_t ename, term_t aname,
		   term_t type, term_t def_value)
{ dtd_element *e;
  char *achars;
  dtd_symbol *asym;
  dtd_attr_list *al;


  if ( !get_element(dtd, ename, &e) )
    return FALSE;
  if ( !PL_get_atom_chars(aname, &achars) )
    return sgml2pl_error(ERR_TYPE, "atom", aname);
  if ( !(asym=dtd_find_symbol(dtd, achars)) )
    return FALSE;

  for(al=e->attributes; al; al=al->next)
  { if ( al->attribute->name == asym )
    { if ( unify_attribute_type(type, al->attribute) &&
	   unify_attribute_default(def_value, al->attribute) )
	return TRUE;

      return FALSE;
    }
  } 
  
  return FALSE;
}


static int
dtd_prop_entities(dtd *dtd, term_t list)
{ term_t tail = PL_copy_term_ref(list);
  term_t head = PL_new_term_ref();
  term_t et   = PL_new_term_ref();
  dtd_entity *e;
  
  for( e=dtd->entities; e; e=e->next )
  { PL_put_atom_chars(et, e->name->name);
    if ( !PL_unify_list(tail, head, tail) ||
	 !PL_unify(head, et) )
      return FALSE;
  }

  return PL_unify_nil(tail);

  return FALSE;
}


static int
dtd_prop_entity(dtd *dtd, term_t ename, term_t value)
{ char *s;
  dtd_entity *e;
  dtd_symbol *id;

  if ( !PL_get_atom_chars(ename, &s) )
    return sgml2pl_error(ERR_TYPE, "atom", ename);

  if ( !(id=dtd_find_symbol(dtd, s)) ||
       !(e=id->entity)  )
    return FALSE;

  switch(e->type)
  { case ET_SYSTEM:
      return PL_unify_term(value, PL_FUNCTOR_CHARS, "system", 1,
			   PL_CHARS, e->exturl);
    case ET_PUBLIC:
      if ( e->exturl )
	return PL_unify_term(value, PL_FUNCTOR_CHARS, "public", 2,
			     PL_CHARS, e->extid,
			     PL_CHARS, e->exturl);
      else
	return PL_unify_term(value, PL_FUNCTOR_CHARS, "public", 2,
			     PL_CHARS, e->extid,
			     PL_VARIABLE);

    case ET_LITERAL:
    default:
      if ( e->value )
	return PL_unify_atom_chars(value, e->value);
  }

  assert(0);
  return FALSE;
}



typedef struct _prop
{ int (*func)();
  const char *name;
  int arity;
  functor_t functor;
} prop;


static prop dtd_props[] =
{ { dtd_prop_doctype,    "doctype",    1 },
  { dtd_prop_elements,	 "elements",   1 },
  { dtd_prop_element,	 "element",    3 },
  { dtd_prop_attributes, "attributes", 2, },
  { dtd_prop_attribute,	 "attribute",  4, },
  { dtd_prop_entities,	 "entities",   1, },
  { dtd_prop_entity,	 "entity",     2, },
  { NULL }
};


static void
initprops()
{ static int done = FALSE;

  if ( !done )
  { prop *p;

    done = TRUE;
    for(p=dtd_props; p->func; p++)
      p->functor = PL_new_functor(PL_new_atom(p->name), p->arity);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
dtd_property(DTD, doctype(DocType))
dtd_property(DTD, elements(ListOfNames))
dtd_property(DTD, element(Name, Omit, Model))
dtd_property(DTD, attributes(ElementName, ListOfAttributes)),
dtd_property(DTD, attribute(ElementName, AttributeName, Type, Default))
dtd_property(DTD, entities(ListOfEntityNames))
dtd_property(DTD, entity(Name, Type))
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static foreign_t
pl_dtd_property(term_t ref, term_t property)
{ dtd *dtd;
  const prop *p;

  initprops();

  if ( !get_dtd(ref, &dtd) )
    return FALSE;

  for(p=dtd_props; p->func; p++)
  { if ( PL_is_functor(property, p->functor) )
    { term_t a = PL_new_term_refs(p->arity);
      int i;

      for(i=0; i<p->arity; i++)
	PL_get_arg(i+1, property, a+i);

      switch(p->arity)
      { case 1:
	  return (*p->func)(dtd, a+0);
	case 2:
	  return (*p->func)(dtd, a+0, a+1);
	case 3:
	  return (*p->func)(dtd, a+0, a+1, a+2);
	case 4:
	  return (*p->func)(dtd, a+0, a+1, a+2, a+3);
	default:
	  assert(0);
	  return FALSE;
      }
    }
  }

  return sgml2pl_error(ERR_DOMAIN, "dtd_property", property);
}

		 /*******************************
		 *	     CATALOG		*
		 *******************************/

static foreign_t
pl_sgml_register_catalog_file(term_t file, term_t where)
{ char *fn, *w;
  catalog_location loc;

  if ( !PL_get_atom_chars(file, &fn) )
    return sgml2pl_error(ERR_TYPE, "atom", file);
  if ( !PL_get_atom_chars(where, &w) )
    return sgml2pl_error(ERR_TYPE, "atom", where);

  if ( streq(w, "start") )
    loc = CTL_START;
  else if ( streq(w, "end") )
    loc = CTL_END;
  else
    return sgml2pl_error(ERR_DOMAIN, "location", where);
  
  return register_catalog_file(fn, loc);
}


		 /*******************************
		 *	      INSTALL		*
		 *******************************/
install_t
install()
{ initConstants();

  PL_register_foreign("new_dtd",	  2, pl_new_dtd,	  0);
  PL_register_foreign("free_dtd",	  1, pl_free_dtd,	  0);
  PL_register_foreign("new_sgml_parser",  2, pl_new_sgml_parser,  0);
  PL_register_foreign("free_sgml_parser", 1, pl_free_sgml_parser, 0);
  PL_register_foreign("set_sgml_parser",  2, pl_set_sgml_parser,  0);
  PL_register_foreign("get_sgml_parser",  2, pl_get_sgml_parser,  0);
  PL_register_foreign("open_dtd",         3, pl_open_dtd,	  0);
  PL_register_foreign("sgml_parse",       2, pl_sgml_parse,
		      PL_FA_TRANSPARENT);
  PL_register_foreign("_sgml_register_catalog_file", 2,
		      pl_sgml_register_catalog_file, 0);

  PL_register_foreign("$dtd_property",	  2, pl_dtd_property,
		      PL_FA_NONDETERMINISTIC);
}

