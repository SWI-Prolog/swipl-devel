/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#include "dtd.h"
#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include <errno.h>
#include "error.h"
#include <stdlib.h>
#include <assert.h>

#define streq(s1, s2) (strcmp(s1, s2) == 0)

#define MAX_ERRORS	50
#define MAX_WARNINGS	50

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
      return pl_error(ERR_EXISTENCE, "sgml_parser", parser);
    }
  }

  return pl_error(ERR_TYPE, "sgml_parser", parser);
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
      return pl_error(ERR_EXISTENCE, "dtd", t);
    }
  }

  return pl_error(ERR_TYPE, "dtd", t);
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
	unify_dtd(tmp, dtd);
      } else if ( !get_dtd(tmp, &dtd) )
	return FALSE;
    }
  }
  if ( !PL_get_nil(tail) )
    return pl_error(ERR_TYPE, "list", tail);

  if ( !dtd )
    dtd = new_dtd(NULL);
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
    return pl_error(ERR_TYPE, "atom", doctype);

  if ( !(dtd=new_dtd(dt)) )
    return FALSE;

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

    PL_get_arg(1, option, a);
    if ( !PL_get_atom_chars(a, (char **)&p->file) )
      return pl_error(ERR_TYPE, "atom", a);
    p->line = 1;
  } else if ( PL_is_functor(option, FUNCTOR_line1) )
  { term_t a = PL_new_term_ref();

    PL_get_arg(1, option, a);
    if ( !PL_get_integer(a, &p->line) )
      return pl_error(ERR_TYPE, "integer", a);
  } else if ( PL_is_functor(option, FUNCTOR_dialect1) )
  { term_t a = PL_new_term_ref();
    char *s;

    PL_get_arg(1, option, a);
    if ( !PL_get_atom_chars(a, &s) )
      return pl_error(ERR_TYPE, "atom", a);

    if ( streq(s, "xml") )
      set_dialect_dtd(p->dtd, DL_XML);
    else if ( streq(s, "sgml") )
      set_dialect_dtd(p->dtd, DL_SGML);
    else
      return pl_error(ERR_DOMAIN, "sgml_dialect", a);
  } else
    return pl_error(ERR_DOMAIN, "sgml_parser_option", option);

  return TRUE;
}

		 /*******************************
		 *	       STREAM		*
		 *******************************/

typedef struct _env
{ term_t	tail;
  struct _env *parent;
} env;

typedef struct _parser_data
{ dtd_parser *parser;			/* parser itself */

  int	      warnings;			/* #warnings seen */
  int	      errors;			/* #errors seen */
  int	      max_errors;		/* error limit */
  int	      max_warnings;		/* warning limit */

  term_t      list;			/* output term (if any) */
  term_t      tail;			/* tail of the list */
  env 	     *stack;			/* environment stack */
  int	      free_on_close;		/* free parser on close */
} parser_data;


static int
unify_attribute_list(term_t alist, int argc, sgml_attribute *argv)
{ int i;
  term_t tail = PL_copy_term_ref(alist);
  term_t h    = PL_new_term_ref();
  term_t a    = PL_new_term_refs(2);

  for(i=0; i<argc; i++)
  { PL_put_atom_chars(a+0, argv[i].definition->name->name);

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
print_open(dtd_parser *p, dtd_element *e, int argc, sgml_attribute *argv)
{ parser_data *pd = p->closure;

  if ( pd->tail )
  { term_t content = PL_new_term_ref();	/* element content */
    term_t alist   = PL_new_term_ref();	/* attribute list */
    term_t et	   = PL_new_term_ref();	/* element structure */
    term_t h       = PL_new_term_ref();

    unify_attribute_list(alist, argc, argv);
    PL_unify_term(et, PL_FUNCTOR, FUNCTOR_element3,
		    PL_CHARS, e->name->name,
		    PL_TERM,  alist,
		    PL_TERM,  content);
    if ( PL_unify_list(pd->tail, h, pd->tail) &&
	 PL_unify(h, et) )
    { env *env = calloc(1, sizeof(env));

      env->tail   = pd->tail;
      env->parent = pd->stack;
      pd->stack   = env;

      pd->tail = content;
      PL_reset_term_refs(alist);

      return TRUE;
    }
  }

  return FALSE;
}


static int
print_close(dtd_parser *p, dtd_element *e)
{ parser_data *pd = p->closure;
  int rval = FALSE;

  if ( pd->tail )
  { rval = PL_unify_nil(pd->tail);
    PL_reset_term_refs(pd->tail);	/* ? */

    if ( pd->stack )
    { env *parent = pd->stack->parent;

      pd->tail = pd->stack->tail;
      free(pd->stack);
      pd->stack = parent;
    }
  }

  return rval;
}


static int
print_entity(dtd_parser *p, dtd_entity *e, int chr)
{ parser_data *pd = p->closure;

  if ( pd->tail )
  { term_t h = PL_new_term_ref();
    int ok;

    if ( !PL_unify_list(pd->tail, h, pd->tail) )
      return FALSE;

    if ( e )
      ok = PL_unify_term(h,
			 PL_FUNCTOR, FUNCTOR_entity1,
			 PL_CHARS, e->name->name);
    else
      ok = PL_unify_term(h,
			 PL_FUNCTOR, FUNCTOR_entity1,
			 PL_INTEGER, chr);
			 
    if ( ok )
    { PL_reset_term_refs(h);
      return TRUE;
    }
  }

  return FALSE;
}


static int
print_cdata(dtd_parser *p, int len, const ochar *data)
{ parser_data *pd = p->closure;

  if ( pd->tail )
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
sgml_error(dtd_parser *p, dtd_error *error)
{ parser_data *pd = p->closure;

  switch(error->severity)
  { case ERS_WARNING:
      pd->warnings++;

      if ( pd->warnings <= pd->max_warnings )
	Sfprintf(Serror, "SGML2PL: %s\n", error->message);

      break;
    case ERS_ERROR:
      pd->errors++;

      if ( pd->errors <= pd->max_errors )
	Sfprintf(Serror, "SGML2PL: %s\n", error->message);

      break;
  }


  return TRUE;
}


static int
write_parser(void *h, char *buf, int len)
{ parser_data *pd = h;
  int i;

  if ( !pd->parser || pd->parser->magic != SGML_PARSER_MAGIC )
  { errno = EINVAL;
    return -1;
  }
  
  if ( pd->errors > pd->max_errors )
  { errno = EIO;
    return -1;
  }

  for(i=0; i<len; i++)
    putchar_dtd_parser(pd->parser, buf[i]);

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



static foreign_t
pl_sgml_open(term_t parser, term_t options, term_t stream)
{ dtd_parser *p;
  parser_data *pd;
  term_t head = PL_new_term_ref();
  term_t tail = PL_copy_term_ref(options);
  IOSTREAM *s;

  if ( PL_is_functor(parser, FUNCTOR_dtd2) )
    return pl_open_dtd(parser, options, stream);

  if ( !get_parser(parser, &p) )
    return FALSE;
  p->on_begin_element = print_open;
  p->on_end_element   = print_close;
  p->on_entity	      = print_entity;
  p->on_cdata         = print_cdata;
  p->on_error	      = sgml_error;
  p->dmode	      = DM_SGML;
  p->state	      = S_PCDATA;
  p->blank_cdata      = TRUE;
  
  pd = calloc(1, sizeof(*pd));
  pd->parser = p;
  pd->max_errors = MAX_ERRORS;
  pd->max_warnings = MAX_WARNINGS;
  p->closure = pd;

  s = Snew(pd, SIO_OUTPUT, &sgml_stream_functions);
  if ( !PL_open_stream(stream, s) )
    return FALSE;

  while ( PL_get_list(tail, head, tail) )
  { if ( PL_is_functor(head, FUNCTOR_document1) )
    { pd->list  = PL_new_term_ref();
      PL_get_arg(1, head, pd->list);
      pd->tail  = PL_copy_term_ref(pd->list);
      pd->stack = NULL;
    } else if ( PL_is_functor(head, FUNCTOR_goal1) )
    { term_t g = PL_new_term_ref();
      int rval;

      PL_get_arg(1, head, g);
      rval = PL_call(g, NULL);
      Sflush(s);

      if ( !rval )
	return FALSE;
    } else if ( PL_is_functor(head, FUNCTOR_max_errors1) )
    { term_t a = PL_new_term_ref();

      PL_get_arg(1, head, a);
      if ( !PL_get_integer(a, &pd->max_errors) )
	return pl_error(ERR_TYPE, "integer", a);
    } else
      return pl_error(ERR_DOMAIN, "sgml_option", head);
  }
  if ( !PL_get_nil(tail) )
    return pl_error(ERR_TYPE, "list", tail);

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
      return PL_put_atom(t, ATOM_empty);
    case C_CDATA:
      return PL_put_atom(t, ATOM_cdata);
    default:
      if ( def->content )
	return put_model(t, def->content);
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
    return pl_error(ERR_TYPE, "atom", name);

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
    return pl_error(ERR_TYPE, "atom", aname);
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
    return pl_error(ERR_TYPE, "atom", ename);

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

  return pl_error(ERR_DOMAIN, "dtd_property", property);
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
  PL_register_foreign("sgml_open",        3, pl_sgml_open,
		      PL_FA_TRANSPARENT);
  PL_register_foreign("$dtd_property",	  2, pl_dtd_property,
		      PL_FA_NONDETERMINISTIC);
}

