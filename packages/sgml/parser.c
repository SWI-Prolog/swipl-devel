/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#define DTD_IMPLEMENTATION 1
#include "dtd.h"
#include "model.h"
#include "util.h"
#include "catalog.h"
#include "parser.h"
#include <stdlib.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "utf8.h"

#define DEBUG(g) ((void)0)

static int		gripe(dtd_error_id e, ...);
static const ichar *	itake_name(dtd *dtd, const ichar *in, dtd_symbol **id);
static const char *	isee_func(dtd *dtd, const ichar *in, charfunc func);
static const ichar *	isee_text(dtd *dtd, const ichar *in, char *id);
static const ichar *	iskip_layout(dtd *dtd, const ichar *in);
static int		process_chars(dtd_parser *p, const ichar *name, const ichar *s);
static dtd_parser *	clone_dtd_parser(dtd_parser *p);
static void		free_model(dtd_model *m);
static int		process_entity_declaraction(dtd *dtd, const ichar *decl);

void			putchar_dtd_parser(dtd_parser *p, int chr);
int			load_dtd_from_file(dtd_parser *p, const char *file);
void			free_dtd_parser(dtd_parser *p);


		 /*******************************
		 *	   CHARACTER CLASS	*
		 *******************************/

#define HasClass(dtd, chr, mask) \
	(dtd->charclass->class[(chr)] & (mask))

		 /*******************************
		 *	      SYMBOLS		*
		 *******************************/

static dtd_symbol_table *
new_symbol_table()
{ dtd_symbol_table *t = calloc(1, sizeof(*t));
  t->size    = SYMBOLHASHSIZE;
  t->entries = calloc(t->size, sizeof(dtd_symbol*));

  return t;
}


static void
free_symbol_table(dtd_symbol_table *t)
{ int i;

  for(i=0; i<t->size; i++)
  { dtd_symbol *s, *next;

    for(s=t->entries[i]; s; s=next)
    { next = s->next;

      free((char *)s->name);
      free(s);
    }
  }

  free(t->entries);
  free(t);
}


dtd_symbol *
dtd_find_symbol(dtd *dtd, const ichar *name)
{ dtd_symbol_table *t = dtd->symbols;

  if ( dtd->case_sensitive )
  { int k = istrhash(name, t->size);
    dtd_symbol *s;

    for(s=t->entries[k]; s; s = s->next)
    { if ( istreq(s->name, name) )
	return s;
    }
  } else
  { int k = istrcasehash(name, t->size);
    dtd_symbol *s;

    for(s=t->entries[k]; s; s = s->next)
    { if ( istrcaseeq(s->name, name) )
	return s;
    }
  }

  return NULL;
}


static dtd_symbol *
add_symbol(dtd_symbol_table *t, const ichar *name)
{ int k = istrhash(name, t->size);
  dtd_symbol *s;

  for(s=t->entries[k]; s; s = s->next)
  { if ( istreq(s->name, name) )
      return s;
  }

  s = calloc(1, sizeof(*s));
  s->name = istrdup(name);
  s->next = t->entries[k];
  t->entries[k] = s;

  return s;
}


		 /*******************************
		 *	    ENTITIES		*
		 *******************************/

static void
free_entity_list(dtd_entity *e)
{ dtd_entity *next;

  for( ; e; e=next)
  { next = e->next;

    if ( e->value ) free(e->value);
    if ( e->extid ) free(e->extid);
    if ( e->exturl ) free(e->exturl);

    free(e);
  }
}


static dtd_entity *
find_pentity(dtd *dtd, dtd_symbol *id)
{ dtd_entity *e;

  for(e = dtd->pentities; e; e=e->next)
  { if ( e->name == id )
      return e;
  }

  return NULL;
}


static const char *
entity_file(dtd *dtd, dtd_entity *e)
{ switch(e->type)
  { case ET_SYSTEM:
      return (char *)isee_text(dtd, e->exturl, "file:");
    case ET_PUBLIC:
      return find_in_catalog("PUBLIC", e->extid);
    default:
      return NULL;
  }
}


static const ichar *
entity_value(dtd *dtd, dtd_entity *e)
{ const char *file;

  if ( e->value )
    return e->value;
  
  if ( (file=entity_file(dtd, e)) )
    e->value = load_file_to_charp(file);

  return e->value;
}


static int
expand_pentities(dtd *dtd, const ichar *in, ichar *out, int len)
{ const ichar *s;

  while(*in)
  { if ( (s = isee_func(dtd, in, CF_PERO)) )
    { dtd_symbol *id;

      in = s;
      if ( (in = itake_name(dtd, in, &id)) )
      { dtd_entity *e = find_pentity(dtd, id);
	const ichar *eval;
	int l;

	in = iskip_layout(dtd, in);
	if ( (s=isee_func(dtd, in, CF_ERC)) ) /* ; is not obligatory? */
	  in = s;

	if ( !e )
	  return gripe(ERC_EXISTENCE, "parameter entity", id->name);

	if ( !(eval = entity_value(dtd, e)) )
	  return FALSE;

	if ( !expand_pentities(dtd, eval, out, len) )
	  return FALSE;
	l = strlen(out);		/* could be better */
	out += l;
	len -= l;

	continue;
      } else
	return gripe(ERC_SYNTAX_ERROR, "Illegal parameter entity reference");
    }

    if ( --len <= 0 )
    { gripe(ERC_REPRESENTATION, "Declaration too long");
      return FALSE;
    }
    *out++ = *in++;
  }

  *out = '\0';

  return TRUE;
}


static const ichar *
isee_character_entity(dtd *dtd, const ichar *in, int *chr)
{ const ichar *s;

  if ( (s=isee_func(dtd, in, CF_ERO)) && *s == '#' )
  { long v = strtol((char *)&s[1], (char **)&s, 10);

    if ( (s=isee_func(dtd, s, CF_ERC)) )
    { *chr = (int)v;
      return s;
    }
  }

  return NULL;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Expand entities in a string.  Used to expand CDATA attribute values.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
expand_entities(dtd_parser *p, const ichar *in, ochar *out, int len)
{ const ichar *s;
  dtd *dtd = p->dtd;

  while(*in)
  { if ( (s = isee_func(dtd, in, CF_ERO)) ) /* & */
    { const ichar *estart = in;		/* for recovery */
      int chr;

      if ( (in=isee_character_entity(dtd, in, &chr)) )
      { if ( chr <= 0 || chr >= OUTPUT_CHARSET_SIZE )
	  gripe(ERC_REPRESENTATION, "character");
	if ( --len <= 0 )
	  return gripe(ERC_REPRESENTATION, "CDATA string too long");
	*out++ = chr;
	continue;
      }

      if ( HasClass(dtd, *s, CH_NMSTART) )
      { dtd_symbol *id;
	dtd_entity *e;
	const ichar *eval;
	int l;
	
	in = itake_name(dtd, s, &id);
	if ( !(in=isee_func(dtd, in, CF_ERC)) )
	{ in = estart;
	  goto recover;
	}
  
	if ( !(e = id->entity) )
	{ gripe(ERC_EXISTENCE, "entity", id->name);
	  in = estart;
	  goto recover;
	}
  
	if ( !(eval = entity_value(dtd, e)) )
	{ gripe(ERC_NO_VALUE, e->name->name);
	  in = estart;
	  goto recover;
	}

	if ( !expand_entities(p, eval, out, len) )
	  return FALSE;
	l = ostrlen(out);		/* could be better */
	out += l;
	len -= l;

	continue;
      }
    }

  recover:
    if ( --len <= 0 )
      return gripe(ERC_REPRESENTATION, "CDATA string too long");

#ifdef UTF8
    if ( p->utf8_decode && ISUTF8_MB(*in) )
    { int chr;

      in = __utf8_get_char(in, &chr);
      *out++ = chr;
    }
#endif
    *out++ = dtd->charmap->map[*in++];
  }

  *out = 0;

  return TRUE;
}



		 /*******************************
		 *	      ELEMENTS		*
		 *******************************/

static dtd_element *
find_element(dtd *dtd, dtd_symbol *id)
{ dtd_element *e;

  if ( id->element )
    return id->element;			/* must check */

  e = calloc(1, sizeof(*e));
  e->name = id;
  id->element = e;
  
  e->next = dtd->elements;
  dtd->elements = e;

  return e;
}


static dtd_element *
def_element(dtd *dtd, dtd_symbol *id)
{ dtd_element *e = find_element(dtd, id);

  if ( !e->structure )
  { e->structure = calloc(1, sizeof(*e->structure));
    e->structure->references = 1;
  }

  return e;
}


static void
free_name_list(dtd_name_list *nl)
{ dtd_name_list *next;

  for( ; nl; nl=next)
  { next = nl->next;

    free(nl);
  }
}


static void
free_attribute(dtd_attr *a)
{ if ( --a->references == 0 )
  { switch(a->type)
    { case AT_NAMEOF:
	free_name_list(a->typeex.nameof);
      default:
	;
    }
    switch(a->def)
    { case AT_DEFAULT:
      { if ( a->type == AT_CDATA )
	  free(a->att_def.cdata);
      }
      default:
	;
    }

    free(a);
  }
}


static void
free_attribute_list(dtd_attr_list *l)
{ dtd_attr_list *next;

  for(; l; l=next)
  { next = l->next;

    free_attribute(l->attribute);
    free(l);
  } 
}


static void
free_element_list(dtd_element_list *l)
{ dtd_element_list *next;

  for( ; l; l=next)
  { next = l->next;

    free(l);
  }
}


static void
free_element_definition(dtd_edef *def)
{ if ( --def->references == 0 )
  { if ( def->content )
      free_model(def->content);
    free_element_list(def->included);
    free_element_list(def->excluded);
    free_state_engine(def->initial_state);

    free(def);
  }
}


static void
free_elements(dtd_element *e)
{ dtd_element *next;

  for( ; e; e=next)
  { next = e->next;

    if ( e->structure )
      free_element_definition(e->structure);
    free_attribute_list(e->attributes);

    free(e);
  }
}


		 /*******************************
		 *	    ATTRIBUTES		*
		 *******************************/

static dtd_attr *
find_attribute(dtd_element *e, dtd_symbol *name)
{ dtd_attr_list *a;

  for(a=e->attributes; a; a=a->next)
  { if ( a->attribute->name == name )
      return a->attribute;
  }

  return NULL;
}


		 /*******************************
		 *	  PARSE PRIMITIVES	*
		 *******************************/

static const ichar *
iskip_layout(dtd *dtd, const ichar *in)
{ ichar cmt = dtd->charfunc->func[CF_CMT];

  for( ; *in; in++ )
  { if ( HasClass(dtd, *in, CH_BLANK) )
      continue;

    if ( in[0] == cmt && in[1] == cmt )
    { in += 2;

      for( ; *in; in++ )
      { if ( in[0] == cmt && in[1] == cmt )
	  break;
      }
      in++;
      continue;
    }

    return in;
  }

  return in;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
See whether we are looking at identifier   "id". "id" must be lowercase!
This is only used for reserved words,  and parsed case-insentive in both
XML and SGML modes.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static const ichar *
isee_identifier(dtd *dtd, const ichar *in, char *id)
{ in = iskip_layout(dtd, in);

					/* match */
  while (*id && *id == tolower(*in) )
    id++, in++;
  if ( *id == 0 && !HasClass(dtd, *in, CH_NAME) )
    return iskip_layout(dtd, in);

  return NULL;
}


static const ichar *
isee_text(dtd *dtd, const ichar *in, char *id)
{ while (*id && *id == tolower(*in) )
    id++, in++;

  if ( *id == 0 )
    return in;

  return NULL;
}


static const ichar *
itake_name(dtd *dtd, const ichar *in, dtd_symbol **id)
{ ichar buf[MAXNMLEN];
  ichar *o = buf;

  in = iskip_layout(dtd, in);
  if ( !HasClass(dtd, *in, CH_NMSTART) )
    return NULL;
  if ( dtd->case_sensitive )
  { while( HasClass(dtd, *in, CH_NAME) )
      *o++ = *in++;
  } else
  { while( HasClass(dtd, *in, CH_NAME) )
      *o++ = tolower(*in++);
  }
  *o++ = '\0';

  *id = add_symbol(dtd->symbols, buf);

  return iskip_layout(dtd, in);
}


static const ichar *
itake_nmtoken(dtd *dtd, const ichar *in, dtd_symbol **id)
{ ichar buf[MAXNMLEN];
  ichar *o = buf;

  in = iskip_layout(dtd, in);
  if ( !HasClass(dtd, *in, CH_NAME) )
    return NULL;
  if ( dtd->case_sensitive )
  { while( HasClass(dtd, *in, CH_NAME) )
      *o++ = *in++;
  } else
  { while( HasClass(dtd, *in, CH_NAME) )
      *o++ = tolower(*in++);
  }
  *o++ = '\0';

  *id = add_symbol(dtd->symbols, buf);

  return iskip_layout(dtd, in);
}


static const ichar *
itake_number(dtd *dtd, const ichar *in, long *v)
{ char *end;

  *v = strtol((const char *)in, &end, 10);
  if ( end > (char *)in )
    return (const ichar *)end;

  return NULL;
}


static const ichar *
itake_string(dtd *dtd, const ichar *in, ichar *out, int len)
{ in = iskip_layout(dtd, in);

  if ( isee_func(dtd, in, CF_LIT) ||
       isee_func(dtd, in, CF_LITA) )
  { ichar q = *in++;

    while( *in && *in != q )
    { *out++ = *in++;
      if ( --len == 0 )
      { gripe(ERC_REPRESENTATION, "String too long");
	return NULL;
      }
    }
    if ( *in )
    { *out = '\0';
      return iskip_layout(dtd, ++in);
    }
  }

  return NULL;
}


static const ichar *
itake_dubbed_string(dtd *dtd, const ichar *in, ichar **out)
{ ichar buf[MAXSTRINGLEN];
  const ichar *end;

  if ( (end=itake_string(dtd, in, buf, sizeof(buf))) )
    *out = istrdup(buf);

  return end;
}


static const ichar *
itake_nmtoken_chars(dtd *dtd, const ichar *in, ichar *out, int len)
{ in = iskip_layout(dtd, in);
  if ( !HasClass(dtd, *in, CH_NAME) )
    return NULL;
  while( HasClass(dtd, *in, CH_NAME) )
  { if ( --len <= 0 )
      gripe(ERC_REPRESENTATION, "Name token too long");
    *out++ = (dtd->case_sensitive ? *in++ : tolower(*in++));
  }
  *out++ = '\0';

  return iskip_layout(dtd, in);
}


static const char *
isee_func(dtd *dtd, const ichar *in, charfunc func)
{ if ( dtd->charfunc->func[func] == *in )
    return ++in;

  return NULL;
}



		 /*******************************
		 *		DTD		*
		 *******************************/

dtd *
new_dtd(const ichar *doctype)
{ dtd *dtd = calloc(1, sizeof(*dtd));

  dtd->magic	 = SGML_DTD_MAGIC;
  dtd->implicit  = TRUE;
  dtd->dialect   = DL_SGML;
  if ( doctype )
    dtd->doctype = istrdup(doctype);
  dtd->symbols	 = new_symbol_table();
  dtd->charclass = new_charclass();
  dtd->charfunc	 = new_charfunc();
  dtd->charmap	 = new_charmap();

  return dtd;
}


void
free_dtd(dtd *dtd)
{ if ( --dtd->references == 0 )
  { if ( dtd->doctype )
      free(dtd->doctype);
  
    free_entity_list(dtd->entities);
    free_entity_list(dtd->pentities);
    free_elements(dtd->elements);
    free_symbol_table(dtd->symbols);
    free(dtd->charfunc);
    free(dtd->charclass);
    free(dtd->charmap);
    dtd->magic = 0;
  
    free(dtd);
  }
}


static char *xml_entities[] =
{ "lt CDATA \"&#60;\"",			/* < */
  "gt CDATA \"&#62;\"",			/* > */
  "amp CDATA \"&#38;\"",		/* & */
  "apos CDATA \"&#39;\"",		/* ' */
  "quot CDATA \"&#34;\"",		/* " */
  NULL
};


int
set_dialect_dtd(dtd *dtd, dtd_dialect dialect)
{ dtd->dialect = dialect;

  switch(dialect)
  { case DL_SGML:
    { dtd->case_sensitive = FALSE;
      break;
    }
    case DL_XML:
    { char **el;

      dtd->case_sensitive = TRUE;
      dtd->charclass->class['_'] |= CH_LCNMSTRT;
      dtd->charclass->class[':'] |= CH_LCNMSTRT;
      dtd->encoding = ENC_UTF8;

      for(el = xml_entities; *el; el++)
	process_entity_declaraction(dtd, *el);

      break;
    }
  }

  return TRUE;
}


static const ichar *
process_entity_value_declaration(dtd *dtd, const ichar *decl, dtd_entity *e)
{ const ichar *s;

  switch ( e->type )
  { case ET_SYSTEM:
    { ichar buf[MAXSTRINGLEN];
      ichar *o;

      strcpy(buf, "file:");
      o = buf + 5;
      if ( !(s = itake_string(dtd, decl, o, sizeof(buf)-5)) )
	goto string_expected;
      e->exturl = istrdup(buf);
      return s;
    }
    case ET_PUBLIC:
    { if ( !(s = itake_dubbed_string(dtd, decl, &e->extid)) )
	goto string_expected;
      decl = s;
      if ( isee_func(dtd, decl, CF_LIT) ||
	   isee_func(dtd, decl, CF_LITA) )
      { if ( !(s = itake_dubbed_string(dtd, decl, &e->exturl)) )
	{ gripe(ERC_SYNTAX_ERROR, "String expected", decl);
	  return NULL;
	}
	decl = s;
      }
      return decl;
    }
    case ET_LITERAL:
    { if ( !(s = itake_dubbed_string(dtd, decl, &e->value)) )
	goto string_expected;
      decl=s;
      return decl;
    }
    default:
      assert(0);
      return NULL;
  }

string_expected:
  gripe(ERC_SYNTAX_ERROR, "String expected", decl);
  return NULL;
}


static int
process_entity_declaraction(dtd *dtd, const ichar *decl)
{ const ichar *s;
  dtd_symbol *id;
  dtd_entity *e = calloc(1, sizeof(*e));
  int isparam;
					/* parameter entity */
  if ( (s=isee_func(dtd, decl, CF_PERO)) )
  { isparam = TRUE;
    decl = s;
  } else
    isparam = FALSE;

  if ( !(s = itake_name(dtd, decl, &id)) )
    return gripe(ERC_SYNTAX_ERROR, "Name expected", decl);
  decl = s;
  e->name = id;

  if ( (s = isee_identifier(dtd, decl, "system")) )
  { e->type = ET_SYSTEM;
    decl = s;
  } else if ( (s = isee_identifier(dtd, decl, "public")) )
  { e->type = ET_PUBLIC;
    decl = s;
  } else
  { if ( isparam )
      e->type = ET_LITERAL;
    else
    { if ( (s=isee_identifier(dtd, decl, "cdata")) )
      { decl = s;
	e->type = ET_LITERAL;
      } else
	return gripe(ERC_SYNTAX_ERROR, "Illegal entity value", decl);
    }
  }

  if ( (decl=process_entity_value_declaration(dtd, decl, e)) )
  { if ( *decl )
      return gripe(ERC_SYNTAX_ERROR, "Unexpected end of declaraction", decl);
  }

  if ( isparam )
  { e->next = dtd->pentities;
    dtd->pentities = e;
  } else
  { e->name->entity = e;
    e->next = dtd->entities;
    dtd->entities = e;
  }
  
  return TRUE;
}


static void
add_submodel(dtd_model *m, dtd_model *sub)
{ dtd_model **d;

  for( d = &m->content.group; *d; d = &(*d)->next )
    ;
  *d = sub;
}


static void
free_model(dtd_model *m)
{ switch(m->type)
  { case MT_SEQ:
    case MT_AND:
    case MT_OR:
    { dtd_model *sub = m->content.group;
      dtd_model *next;

      for(; sub; sub = next)
      { next = sub->next;

	free_model(sub);
      }
    }
    default:
      ;
  }

  free(m);
}


static dtd_model *
make_model(dtd *dtd, const ichar *decl, const ichar **end)
{ const ichar *s;
  dtd_model *m = calloc(1, sizeof(*m));
  dtd_symbol *id;

  if ( (s=isee_identifier(dtd, decl, "#pcdata")) )
  { m->type = MT_PCDATA;
    m->cardinality = MC_ONE;		/* actually don't care */
    *end = s;
    return m;
  }

  if ( (s=itake_name(dtd, decl, &id)) )
  { m->type = MT_ELEMENT;
    m->content.element = find_element(dtd, id);
    decl = s;
  } else
  { if ( !(s=isee_func(dtd, decl, CF_GRPO)) )
    { gripe(ERC_SYNTAX_ERROR, "Name group expected", decl);
      free_model(m);
      return NULL;
    }
    decl = s;

    for(;;)
    { dtd_model *sub;
      modeltype mt;

      if ( !(sub = make_model(dtd, decl, &s)) )
	return NULL;
      decl = s;
      add_submodel(m, sub);
      
      if ( (s = isee_func(dtd, decl, CF_OR)) )
      { decl = s;
	mt = MT_OR;
      } else if ( (s = isee_func(dtd, decl, CF_SEQ)) )
      { decl = s;
	mt = MT_SEQ;
      } else if ( (s = isee_func(dtd, decl, CF_AND)) )
      { decl = s;
	mt = MT_AND;
      } else if ( (s = isee_func(dtd, decl, CF_GRPC)) )
      { decl = s;
	break;
      } else
      { gripe(ERC_SYNTAX_ERROR, "Connector ('|', ',' or '&') expected", decl);
	free_model(m);
	return NULL;
      }
      decl = iskip_layout(dtd, decl);

      if ( m->type != mt )
      { if ( !m->type )
	  m->type = mt;
	else
	{ gripe(ERC_SYNTAX_ERROR, "Different connector types in model", decl);
	  free_model(m);
	  return NULL;
	}
      }
    }
  }

  if ( (s = isee_func(dtd, decl, CF_OPT)) )
  { decl = s;
    m->cardinality = MC_OPT;
  } else if ( (s=isee_func(dtd, decl, CF_REP)) )
  { decl = s;
    m->cardinality = MC_REP;
  } else if ( (s=isee_func(dtd, decl, CF_PLUS)) )
  { decl = s;
    m->cardinality = MC_PLUS;
  } else
    m->cardinality = MC_ONE;
    
  if ( m->type == MT_UNDEF )		/* simplify (e+), etc. */
  { dtd_model *sub = m->content.group;
    modelcard card;

    assert(!sub->next);
    if ( sub->cardinality == MC_ONE )
      card = m->cardinality;
    else if ( m->cardinality == MC_ONE )
      card = sub->cardinality;
    else
    { m->type = MT_OR;
      goto out;
    }
      
    *m = *sub;
    m->cardinality = card;
    free_model(sub);
  }

out:
  *end = iskip_layout(dtd, decl);
  return m;
}


static const ichar *
process_model(dtd *dtd, dtd_edef *e, const ichar *decl)
{ const ichar *s;

  decl = iskip_layout(dtd, decl);
  if ( (s = isee_identifier(dtd, decl, "empty")) )
  { e->type = C_EMPTY;
    return s;
  }
  if ( (s = isee_identifier(dtd, decl, "cdata")) )
  { e->type = C_CDATA;
    return s;
  }
  
  e->type = C_PCDATA;
  if ( !(e->content = make_model(dtd, decl, &decl)) )
    return FALSE;

  return decl;
}


static const ichar *
itake_namegroup(dtd *dtd, const ichar *decl, dtd_symbol **names, int *n)
{ const ichar *s;
  int en = 0;

  if ( (s=isee_func(dtd, decl, CF_GRPO)) )
  { for(;;)
    { if ( !(decl=itake_name(dtd, s, &names[en++])) )
      { gripe(ERC_SYNTAX_ERROR, "Name expected", s);
	return NULL;
      }
      if ( (s=isee_func(dtd, decl, CF_OR)) )
      { decl = iskip_layout(dtd, s);
	continue;
      }
      if ( (s=isee_func(dtd, decl, CF_GRPC)) )
      { *n = en;
        decl = s;
	return iskip_layout(dtd, decl);
      }
      gripe(ERC_SYNTAX_ERROR, "Bad name-group", decl);
      return NULL;
    }
  }

  return NULL;
}


static const ichar *
itake_id_or_idlist(dtd *dtd, const ichar *decl, dtd_symbol **names, int *n)
{ const ichar *s;

  if ( isee_func(dtd, decl, CF_GRPO) )
  { return itake_namegroup(dtd, decl, names, n);
  } else
  { if ( !(s = itake_name(dtd, decl, &names[0])) )
    { gripe(ERC_SYNTAX_ERROR, "Name expected", decl);
      return NULL;
    }
    *n = 1;
    return s;
  }
}


static void
add_element_list(dtd_element_list **l, dtd_element *e)
{ dtd_element_list *n = calloc(1, sizeof(*n));

  n->value = e;

  for( ; *l; l = &(*l)->next )
    ;
  *l = n;
}


static int
process_element_declaraction(dtd *dtd, const ichar *decl)
{ ichar buf[MAXDECL];
  const ichar *s;
  dtd_symbol *eid[MAXATTELEM];
  dtd_edef *def;
  int en;
  int i;

					/* expand parameter entities */
  if ( !expand_pentities(dtd, decl, buf, sizeof(buf)) )
    return FALSE;
  decl = buf;

  if ( !(s=itake_id_or_idlist(dtd, decl, eid, &en)) )
    return gripe(ERC_SYNTAX_ERROR, "Name or name-group expected", decl);
  decl = s;
  if ( en == 0 )
    return TRUE;			/* 0 elements */

  def = calloc(1, sizeof(*def));
  for(i=0; i<en; i++)
  { find_element(dtd, eid[i]);
    eid[i]->element->structure = def;
  }
  def->references = en;			/* for GC */


					/* omitted tag declarations (optional) */
  if ( (s = isee_identifier(dtd, decl, "-")) )
  { def->omit_close = FALSE;
    goto seeclose;
  } else if ( (s = isee_identifier(dtd, decl, "o")) )
  { def->omit_open = TRUE;

  seeclose:
    decl = s;
    if ( (s = isee_identifier(dtd, decl, "-")) )
    { def->omit_close = FALSE;
    } else if ( (s = isee_identifier(dtd, decl, "o")) )
    { for(i=0; i<en; i++)
	def->omit_close = TRUE;
    } else
      gripe(ERC_SYNTAX_ERROR, "Bad omit-tag declaration", decl);

    decl = s;
  }
      
					/* content model */
  if ( !(decl=process_model(dtd, def, decl)) )
    return FALSE;

					/* in/excluded elements */
  if ( decl[0] == '-' || decl[0] == '+' )
  { dtd_symbol *ng[MAXNAMEGROUP];
    int ns;
    dtd_element_list **l;
    
    if ( decl[0] == '-' )
      l = &def->excluded;
    else
      l = &def->included;

    decl++;
    if ( (s=itake_namegroup(dtd, decl, ng, &ns)) )
    { int i;

      decl = s;

      for(i=0; i<ns; i++)
	add_element_list(l, find_element(dtd, ng[i]));
    } else
    { return gripe(ERC_SYNTAX_ERROR, "Name group expected", decl);
    }
  }

  if (*decl)
    return gripe(ERC_SYNTAX_ERROR, "Unexpected end of declaration", decl);

  return TRUE;
}


static void
add_name_list(dtd_name_list **nl, dtd_symbol *s)
{ dtd_name_list *n = calloc(1, sizeof(*n));
  
  n->value = s;

  for( ; *nl; nl = &(*nl)->next )
    ;

  *nl = n;
}


static void
add_attribute(dtd *dtd, dtd_element *e, dtd_attr *a)
{ dtd_attr_list **l;
  dtd_attr_list *n;

  for(l = &e->attributes; *l; l = &(*l)->next)
  { if ( (*l)->attribute->name == a->name )
    { gripe(ERC_REDEFINED, "attribute", a->name->name);
      free_attribute((*l)->attribute);
      (*l)->attribute = a;
      a->references++;
      return;
    }
  }

  n = calloc(1, sizeof(*n));

  n->attribute = a;
  a->references++;
  *l = n;
}


static int
process_attlist_declaraction(dtd *dtd, const ichar *decl)
{ dtd_symbol *eid[MAXATTELEM];
  int i, en;
  ichar buf[MAXDECL];
  const ichar *s;

					/* expand parameter entities */
  if ( !expand_pentities(dtd, decl, buf, sizeof(buf)) )
    return FALSE;
  decl = iskip_layout(dtd, buf);
  DEBUG(printf("Expanded to %s\n", decl));

  if ( !(decl=itake_id_or_idlist(dtd, decl, eid, &en)) )
    return FALSE;

					/* fetch attributes */
  while(*decl)
  { dtd_attr *at = calloc(1, sizeof(*at));

					/* name of attribute */
    if ( !(s = itake_name(dtd, decl, &at->name)) )
      return gripe(ERC_SYNTAX_ERROR, "Name expected", decl);
    decl = s;

					/* (name1|name2|...) type */
    if ( (s=isee_func(dtd, decl, CF_GRPO)) )
    { at->type = AT_NAMEOF;

      decl=s;
      for(;;)
      { dtd_symbol *nm;

	if ( !(s = itake_nmtoken(dtd, decl, &nm)) )
	  return gripe(ERC_SYNTAX_ERROR, "Name expected", decl);
	decl = s;
	add_name_list(&at->typeex.nameof, nm);
	if ( (s=isee_func(dtd, decl, CF_OR)) )
	{ decl = s;
	  continue;
	}
	if ( (s = isee_func(dtd, decl, CF_GRPC)) )
	{ decl=s;
	  decl = iskip_layout(dtd, decl);
	  break;
	}
	return gripe(ERC_SYNTAX_ERROR, "Illegal name-group", decl);
      }
    } else if ( (s=isee_identifier(dtd, decl, "cdata")) )
    { decl = s;
      at->type = AT_CDATA;
    } else if ( (s=isee_identifier(dtd, decl, "entity")) )
    { decl = s;
      at->type = AT_ENTITY;
    } else if ( (s=isee_identifier(dtd, decl, "entities")) )
    { decl = s;
      at->type = AT_ENTITIES;
    } else if ( (s=isee_identifier(dtd, decl, "id")) )
    { decl = s;
      at->type = AT_ID;
    } else if ( (s=isee_identifier(dtd, decl, "idref")) )
    { decl = s;
      at->type = AT_IDREF;
    } else if ( (s=isee_identifier(dtd, decl, "idrefs")) )
    { decl = s;
      at->type = AT_IDREFS;
    } else if ( (s=isee_identifier(dtd, decl, "name")) )
    { decl = s;
      at->type = AT_NAME;
    } else if ( (s=isee_identifier(dtd, decl, "names")) )
    { decl = s;
      at->type = AT_NAMES;
    } else if ( (s=isee_identifier(dtd, decl, "nmtoken")) )
    { decl = s;
      at->type = AT_NMTOKEN;
    } else if ( (s=isee_identifier(dtd, decl, "nmtokens")) )
    { decl = s;
      at->type = AT_NMTOKENS;
    } else if ( (s=isee_identifier(dtd, decl, "number")) )
    { decl = s;
      at->type = AT_NUMBER;
    } else if ( (s=isee_identifier(dtd, decl, "numbers")) )
    { decl = s;
      at->type = AT_NUMBERS;
    } else if ( (s=isee_identifier(dtd, decl, "nutoken")) )
    { decl = s;
      at->type = AT_NMTOKEN;
    } else if ( (s=isee_identifier(dtd, decl, "nutokens")) )
    { decl = s;
      at->type = AT_NUTOKENS;
    } else
      return gripe(ERC_SYNTAX_ERROR, "Attribute-type expected", decl);

					/* Attribute Defaults */
    if ( (s=isee_identifier(dtd, decl, "#fixed")) )
    { decl = s;
      at->def = AT_FIXED;
    } else if ( (s=isee_identifier(dtd, decl, "#required")) )
    { decl = s;
      at->def = AT_REQUIRED;
    } else if ( (s=isee_identifier(dtd, decl, "#current")) )
    { decl = s;
      at->def = AT_CURRENT;
    } else if ( (s=isee_identifier(dtd, decl, "#conref")) )
    { decl = s;
      at->def = AT_CONREF;
    } else if ( (s=isee_identifier(dtd, decl, "#implied")) )
    { decl = s;
      at->def = AT_IMPLIED;
    } else				/* real default */
      at->def = AT_DEFAULT;

    if ( at->def == AT_DEFAULT || at->def == AT_FIXED )
    { switch(at->type)
      { case AT_CDATA:
	{ dtd_symbol *value;

	  if ( (s=itake_nmtoken(dtd, decl, &value)) )
	  { decl = s;
	    at->att_def.cdata = (ichar *)value->name;
	  } else if ( (s=itake_dubbed_string(dtd, decl, &at->att_def.cdata)) )
	  { decl = s;
	  } else
	    return gripe(ERC_DOMAIN, "cdata", decl);
	  break;
	}
	case AT_ENTITY:
	case AT_NAME:
	{ if ( (s=itake_name(dtd, decl, &at->att_def.name)) )
	  { decl = s;
	  } else
	    return gripe(ERC_DOMAIN, "name", decl);
	  break;
	}
	case AT_NMTOKEN:
	case AT_NAMEOF:
	{ if ( (s=itake_nmtoken(dtd, decl, &at->att_def.name)) )
	  { decl = s;
	  } else
	    return gripe(ERC_DOMAIN, "nmtoken", decl);
	  break;
	}
	case AT_NUMBER:
	{ if ( !(decl = itake_number(dtd, decl, &at->att_def.number)) )
	     return gripe(ERC_DOMAIN, "number", decl);
	  break;
	}
	default:
	  return gripe(ERC_REPRESENTATION, "No default for type");
      }
    }

					/* add to list */
    for(i=0; i<en; i++)
    { dtd_element *e = def_element(dtd, eid[i]);

      add_attribute(dtd, e, at);
    }
  }

  return TRUE;
}

		 /*******************************
		 *    GENERIC TAG PROCESSING	*
		 *******************************/

typedef enum
{ IE_NORMAL,
  IE_INCLUDED,				/* is included */
  IE_EXCLUDED				/* is excluded */
} includetype;


static includetype
in_or_excluded(sgml_environment *env, dtd_element *e)
{ for(; env; env=env->parent)
  { if ( env->element->structure )
    { dtd_edef *def = env->element->structure;
      dtd_element_list *el;

      for(el=def->excluded; el; el=el->next)
      { if ( el->value == e )
	  return IE_EXCLUDED;
      }
      for(el=def->included; el; el=el->next)
      { if ( el->value == e )
	  return IE_INCLUDED;
      }
    }
  }

  return IE_NORMAL;
}


static int
complete(sgml_environment *env)
{ if ( env->element->structure && !env->element->undefined )
  { dtd_edef *def = env->element->structure;

    if ( !same_state(def->final_state, env->state) )
      return FALSE;
  }

  return TRUE;
}


static void
validate_completeness(sgml_environment *env)
{ if ( !complete(env) )
  { char buf[256];

    sprintf(buf, "Incomplete <%s> element", env->element->name->name);

    gripe(ERC_VALIDATE, buf);		/* TBD: expected */
  }
}


static sgml_environment *
push_element(dtd_parser *p, dtd_element *e, int callback)
{ if ( e != CDATA_ELEMENT )
  { sgml_environment *env = calloc(1, sizeof(*env));

    make_state_engine(e);
    env->element = e;
    env->state = make_state_engine(e);
    env->parent = p->environments;
    p->environments = env;
    if ( callback && p->on_begin_element )
      (*p->on_begin_element)(p, e, 0, NULL);
    if ( e->structure && e->structure->type == C_CDATA )
    { p->state = S_CDATA;
      p->etag = e->name->name;
      p->etaglen = istrlen(p->etag);
    }
  }

  return p->environments;
}


static int
pop_to(dtd_parser *p, sgml_environment *to)
{ sgml_environment *env, *parent;
  
  for(env = p->environments; env != to; env=parent)
  { dtd_element *e = env->element;

    validate_completeness(env);
    parent = env->parent;
	
    if ( e->structure && !e->structure->omit_close )
      gripe(ERC_OMITTED_CLOSE, e->name->name);

    if ( p->on_end_element )
      (*p->on_end_element)(p, e);
    free(env);
  }
  
  p->environments = to;

  return TRUE;
}


static void
allow_for(dtd_element *in, dtd_element *e)
{ dtd_edef *def = in->structure;
  dtd_model *g;

  if ( def->type == C_EMPTY )
  { def->type = C_PCDATA;
    def->content = calloc(1, sizeof(*def->content));
    def->content->type = MT_OR;
    def->content->cardinality = MC_REP;
  }
  assert(def->content->type == MT_OR);

  g = def->content->content.group;

  if ( e == CDATA_ELEMENT )
  { dtd_model *m;

    for(; g; g = g->next)
    { if ( g->type == MT_PCDATA )
	return;
    }
    m = calloc(1, sizeof(*m));
    m->type	   = MT_PCDATA;
    m->cardinality = MC_ONE;		/* ignored */
    add_submodel(def->content, m);
  } else
  { dtd_model *m;

    for(; g; g = g->next)
    { if ( g->type == MT_ELEMENT && g->content.element == e )
	return;
    }
    m = calloc(1, sizeof(*m));
    m->type	   = MT_ELEMENT;
    m->cardinality = MC_ONE;		/* ignored */
    m->content.element = e;
    add_submodel(def->content, m);
  }
}



static int
open_element(dtd_parser *p, dtd_element *e)
{ if ( p->environments )
  { sgml_environment *env = p->environments;

    if ( env->element->undefined )
    { allow_for(env->element, e);
      push_element(p, e, FALSE);
      return TRUE;
    }

    switch(in_or_excluded(env, e))
    { case IE_EXCLUDED:
	gripe(ERC_NOT_ALLOWED, e->name->name);
	/*FALLTHROUGH*/
      case IE_INCLUDED:
        push_element(p, e, FALSE);
	return TRUE;
      case IE_NORMAL:
	for(; env; env=env->parent)
	{ dtd_state *new;
    
	  if ( (new = make_dtd_transition(env->state, e)) )
	  { env->state = new;
	    pop_to(p, env);
	    push_element(p, e, FALSE);
	    return TRUE;
	  } else
	  { dtd_element *oe[MAXOMITTED];
	    int olen;
	    int i;
    
	    if ( (olen=find_omitted_path(env->state, e, oe)) > 0 )
	    { pop_to(p, env);
	      for(i=0; i<olen; i++)
	      { env->state = make_dtd_transition(env->state, oe[i]);
		env = push_element(p, oe[i], TRUE);
	      }
	      env->state = make_dtd_transition(env->state, e);
	      push_element(p, e, FALSE);
	      return TRUE;
	    }
	  }
	}
    }
    if ( e == CDATA_ELEMENT )
      gripe(ERC_VALIDATE, "CDATA not allowed here");
    else
      gripe(ERC_NOT_ALLOWED, e->name->name);
  }

  push_element(p, e, FALSE);
  return TRUE;
}


static int
close_element(dtd_parser *p, dtd_element *e)
{ sgml_environment *env;

  for(env = p->environments; env; env=env->parent)
  { if ( env->element == e )
    { sgml_environment *parent;

      for(env = p->environments; ; env=parent)
      {	dtd_element *ce	= env->element;

	validate_completeness(env);
	parent = env->parent;
	
	if ( p->on_end_element )
	  (*p->on_end_element)(p, env->element);
	free(env);
	if ( ce == e )
	{ p->environments = parent;
	  return TRUE;
	} else
	{ if ( ce->structure && !ce->structure->omit_close )
	    gripe(ERC_OMITTED_CLOSE, ce->name->name);
	}
      }
    }
  }

  return gripe(ERC_NOT_OPEN, e->name->name);
}


static const ichar *
get_attribute_value(dtd_parser *p, const ichar *decl, sgml_attribute *att)
{ ichar buf[MAXSTRINGLEN];
  ochar cdata[MAXSTRINGLEN];
  dtd *dtd = p->dtd;
  const ichar *s;
  const ichar *end;

  if ( !(end=itake_string(dtd, decl, buf, sizeof(buf))) )
    end=itake_nmtoken_chars(dtd, decl, buf, sizeof(buf));
  if ( !end )
    return NULL;

  switch(att->definition->type)
  { case AT_NUMBER:			/* number */
      if ( (s=itake_number(dtd, decl, &att->value.number)) )
	return s;
      return NULL;
    case AT_CDATA:			/* CDATA attribute */
      expand_entities(p, buf, cdata, MAXSTRINGLEN);
      att->value.cdata = ostrdup(cdata);
      return end;
    case AT_ENTITY:			/* entity-name */
    case AT_ID:				/* identifier */
    case AT_IDREF:			/* identifier reference */
    case AT_NAME:			/* name token */
    case AT_NAMEOF:			/* one of these names */
    case AT_NMTOKEN:			/* name-token */
    case AT_NOTATION:			/* notation-name */
    case AT_NUTOKEN:			/* number token */
      att->value.text = istrdup(buf);	/* TBD: more validation */
      return end;
    case AT_NAMES:			/* list of names */
    case AT_NMTOKENS:			/* name-token list */
    case AT_NUMBERS:			/* number list */
    case AT_NUTOKENS:
    case AT_IDREFS:			/* list of identifier references */
    case AT_ENTITIES:			/* entity-name list */
      att->value.text = istrdup(buf);	/* TBD: break-up */
      return end;
  }

  assert(0);
  return NULL;
}


static const ichar *
process_attributes(dtd_parser *p, dtd_element *e, const ichar *decl,
		   sgml_attribute *atts, int *argc)
{ int attn = 0;
  dtd *dtd = p->dtd;

  decl = iskip_layout(dtd, decl);
  while(decl && *decl)
  { dtd_symbol *nm;
    const ichar *s;

    if ( (s=itake_name(dtd, decl, &nm)) )
    { decl = s;

#ifdef XMLNS
					/* XMLNS declarations */
      if ( dtd->dialect == DL_XML )
      { dtd_symbol *snm;		/* scoped name */
	xmlns *ns;

	if ( streq(s->name, "xmlns") )	/* xmlns[:ns]=url */
	{ if ( (s=isee_func(dtd, s. CF_NS)) )
	  { dtd_symbol *ns;
	    ichar url[MAXSTRINGLEN];

	    if ( (s=itake_name(dtd, s, &ns)) &&
		 (s=isee_func(dtd, decl, CF_VI)) &&
		 (s=itake_string(dtd, s, url, sizeof(url))) )
	    { decl = s;

	      xmlns_push(p, ns, add_symbol(dtd->symbols, url));
	      continue;
	    } else if ( (s=isee_func(dtd, decl, CF_VI)) &&
			(s=itake_string(dtd, s, url, sizeof(url))) )
	    { decl = s;

	      xmlns_push(p, NULL, add_symbol(dtd->symbols, url));
	      continue;
	    }
	  }
	}
	
	if ( (s=isee_func(dtd, s, CF_NS)) &&
	     (s=itake_name(dtd, s, &snm)) &&
	     (ns = xmlns_find(p->environments, nm))
	{ decl = s;
	}
      }
#endif

      if ( (s=isee_func(dtd, decl, CF_VI)) ) /* name= */
      { dtd_attr *a;

	decl = s;
	if ( !(a=find_attribute(e, nm)) )
	{ a = calloc(1, sizeof(*a));

	  a->name = nm;
	  a->type = AT_CDATA;
	  a->def  = AT_IMPLIED;
	  add_attribute(dtd, e, a);

	  if ( !e->undefined )
	    gripe(ERC_NO_ATTRIBUTE, e->name->name, nm->name);
	}
	atts[attn].definition = a;
	if ( (decl=get_attribute_value(p, decl, atts+attn)) )
	{ attn++;
	  continue;
	}
      } else if ( e->structure )	/* value shorthand */
      { dtd_attr_list *al;

	for(al=e->attributes; al; al=al->next)
	{ dtd_attr *a = al->attribute;

	  if ( a->type == AT_NAMEOF )
	  { dtd_name_list *nl;

	    for(nl=a->typeex.nameof; nl; nl = nl->next)
	    { if ( nl->value == nm )
	      { atts[attn].definition = a;
		atts[attn].value.text = istrdup(nm->name);
		attn++;
		goto next;
	      }
	    }
	  }
	}
	gripe(ERC_NO_ATTRIBUTE_VALUE, e->name->name, nm->name);
      }
    } else
    { *argc = attn;
      return decl;
    }
    
  next:
    ;
  }

  *argc = attn;
  return decl;
}


static void
free_attribute_values(int argc, sgml_attribute *argv)
{ int i;

  for(i=0; i<argc; i++, argv++)
  { switch(argv->definition->type)
    { case AT_CDATA:
	free(argv->value.cdata);
        break;
      case AT_NUMBER:
        break;
      default:
	free(argv->value.text);
        break;
    }
  }
}


static int
process_begin_element(dtd_parser *p, const ichar *decl)
{ dtd *dtd = p->dtd;
  dtd_symbol *id;
  const ichar *s;

  if ( (s=itake_name(dtd, decl, &id)) )
  { sgml_attribute atts[MAXATTRIBUTES];
    int natts;
    dtd_element *e = find_element(dtd, id);
    int empty = FALSE;

    if ( !e->structure )
    { dtd_edef *def;
      if ( !dtd->implicit )
	gripe(ERC_EXISTENCE, "element", e->name->name);

      e->undefined = TRUE;
      def_element(dtd, id);
      def = e->structure;
      def->type = C_EMPTY;
    }

    decl=s;
    if ( !(s=process_attributes(p, e, decl, atts, &natts)) )
      return gripe(ERC_SYNTAX_ERROR, "Bad attribute list", decl);
    decl=s;
    if ( dtd->dialect == DL_XML && (s=isee_func(dtd, decl, CF_ETAGO2)) )
    { empty = TRUE;			/* XML <tag/> */
      decl = s;
    }
    if ( *decl )
      gripe(ERC_SYNTAX_ERROR, "Bad attribute list", decl);

    open_element(p, e);
    if ( p->on_begin_element )
      (*p->on_begin_element)(p, e, natts, atts);
    free_attribute_values(natts, atts);
    if ( empty ||
	 (e->structure &&
	  e->structure->type == C_EMPTY &&
	  !e->undefined) )
      close_element(p, e);
    return TRUE;
  }

  return gripe(ERC_SYNTAX_ERROR, "Bad open-element tag", decl);
}


static int
process_end_element(dtd_parser *p, const ichar *decl)
{ dtd *dtd = p->dtd;
  dtd_symbol *id;
  const ichar *s;
  
  if ( (s=itake_name(dtd, decl, &id)) && *s == '\0' )
    return close_element(p, find_element(dtd, id));

  return gripe(ERC_SYNTAX_ERROR, "Bad close-element tag", decl);
}


static int				/* <!DOCTYPE ...> */
process_doctype(dtd_parser *p, const ichar *decl)
{ dtd *dtd = p->dtd;
  dtd_symbol *id;
  const ichar *s;
  dtd_entity *et = calloc(1, sizeof(*et));

  if ( !(s=itake_name(dtd, decl, &id)) )
    return gripe(ERC_SYNTAX_ERROR, "Name expected", decl);
  decl = s;
  et->name = id;

  if ( (s=isee_identifier(dtd, decl, "system")) )
    et->type = ET_SYSTEM;
  else if ( (s=isee_identifier(dtd, decl, "public")) )
    et->type = ET_PUBLIC;
  else
    return gripe(ERC_SYNTAX_ERROR, "SYSTEM or PUBLIC expected", decl);
  decl = s;

  if ( !(s=process_entity_value_declaration(dtd, decl, et)) )
    return FALSE;
  decl = s;

  if ( !dtd->doctype )			/* i.e. anonymous DTD */
  { const char *file;
    dtd_parser *clone;

    dtd->doctype = istrdup(id->name);	/* Fill it */
    if ( !(file=find_in_catalog("DOCTYPE", dtd->doctype)) && 
	 !(file=entity_file(dtd, et)) )
    { gripe(ERC_EXISTENCE, "DTD", dtd->doctype);
    } else
    { clone = clone_dtd_parser(p);
      if ( !load_dtd_from_file(clone, file) )
	gripe(ERC_EXISTENCE, "file", file);
      free_dtd_parser(clone);
    }
  }

  free_entity_list(et);

  if ( (s=isee_func(dtd, decl, CF_DSO)) ) /* [...] */
  { int grouplevel = 1;
    data_mode oldmode  = p->dmode;
    dtdstate  oldstate = p->state;

    p->dmode = DM_DTD;
    p->state = S_PCDATA;
    empty_icharbuf(p->buffer);		/* dubious */

    for( ; *s; s++ )
    { if ( isee_func(dtd, s, CF_DSC) && --grouplevel == 0 )
	break;
      putchar_dtd_parser(p, *s);
    }

    p->state = oldstate;
    p->dmode = oldmode;
  }

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Process <? ... ?>

Should deal with character encoding for XML documents.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
process_pi(dtd_parser *p, const ichar *decl)
{ const ichar *s;
  dtd *dtd = p->dtd;

  if ( (s=isee_identifier(dtd, decl, "xml")) ) /* <?xml version="1.0"?> */
  { set_dialect_dtd(dtd, DL_XML);
    return TRUE;
  }

  return FALSE;				/* Warn? */
}


static int
process_declaration(dtd_parser *p, const ichar *decl)
{ const ichar *s;
  dtd *dtd = p->dtd;

  if ( (s=isee_func(dtd, decl, CF_MDO2)) ) /* <! ... >*/
  { decl = s;

    if ( (s = isee_identifier(dtd, decl, "entity")) )
      process_entity_declaraction(dtd, s);
    else if ( (s = isee_identifier(dtd, decl, "element")) )
      process_element_declaraction(dtd, s);
    else if ( (s = isee_identifier(dtd, decl, "attlist")) )
      process_attlist_declaraction(dtd, s);
    else if ( (s = isee_identifier(dtd, decl, "doctype")) )
    { if ( p->dmode != DM_DTD )
	process_doctype(p, s);
    } else
    { s = iskip_layout(dtd, decl);
  
      if ( *s )
	gripe(ERC_SYNTAX_ERROR, "Invalid declaration", s);
    }

    return TRUE;
  }

  if ( p->dmode != DM_DTD )
  { if ( (s=isee_func(dtd, decl, CF_ETAGO2)) ) /* </ ... > */
      return process_end_element(p, s);
    else if ( (s=isee_func(dtd, decl, CF_PRO2)) ) /* <? */
      return process_pi(p, s);
    else
      return process_begin_element(p, decl);
  }


  return gripe(ERC_SYNTAX_ERROR, "Invalid declaration", decl);
}

		 /*******************************
		 *	  STREAM BINDING	*
		 *******************************/

static dtd_parser *current_parser;	/* For gripes */

void
set_file_dtd_parser(dtd_parser *p, const char *file)
{ p->location.file    = file;
  p->location.line    = 1;
  p->location.linepos = 1;
  p->location.charpos = 0;
}

dtd_parser *
new_dtd_parser(dtd *dtd)
{ dtd_parser *p = calloc(1, sizeof(*p));
  
  if ( !dtd )
    dtd = new_dtd(NULL);
  dtd->references++;

  p->magic      = SGML_PARSER_MAGIC;
  p->dtd	= dtd;
  p->state	= S_PCDATA;
  p->mark_state	= MS_INCLUDE;
  p->dmode      = DM_DTD;
  p->encoding	= ENC_ISO_LATIN1;
  p->buffer	= new_icharbuf();
  p->cdata	= new_ocharbuf();
  set_file_dtd_parser(p, NULL);

  return p;
}


static dtd_parser *
clone_dtd_parser(dtd_parser *p)
{ dtd_parser *clone = calloc(1, sizeof(*p));
  
  *clone = *p;
  clone->dtd->references++;
  clone->environments =	NULL;
  clone->marked	      =	NULL;
  clone->etag	      =	NULL;
  clone->grouplevel   =	0;
  clone->state	      =	S_PCDATA;
  clone->mark_state   =	MS_INCLUDE;
  clone->dmode	      =	DM_DTD;
  clone->buffer	      =	new_icharbuf();
  clone->cdata	      =	new_ocharbuf();

  return clone;
}


void
free_dtd_parser(dtd_parser *p)
{ free_icharbuf(p->buffer);
  free_ocharbuf(p->cdata);

  free_dtd(p->dtd);

  free(p);
}


static int
process_include(dtd_parser *p, const ichar *entity_name)
{ dtd_symbol *id;
  dtd_entity *pe;

  if ( (id=dtd_find_symbol(p->dtd, entity_name)) &&
       (pe=find_pentity(p->dtd, id)) )
  { const ichar *text = entity_value(p->dtd, pe);

    if ( !text )
      return gripe(ERC_NO_VALUE, pe->name->name);

    return process_chars(p, entity_name, text);
  }
  
  return gripe(ERC_EXISTENCE, "parameter entity", entity_name);
}


static int
process_chars(dtd_parser *p, const ichar *name, const ichar *s)
{ dtd_srcloc old = p->location;
  
  set_file_dtd_parser(p, (char *)name);
  empty_icharbuf(p->buffer);		/* dubious */
  for(; *s; s++)
    putchar_dtd_parser(p, *s);
  p->location = old;

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Process <![ KEYWORD [

Switches ->mark_state according to KEYWORD. Processes the rest in normal
S_PCDATA style, which pops the mark-stack on seeing ]]>

For the purpose of <!DOCTYPE spec [additions]> we switch to S_GROUP if
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static void
process_marked_section(dtd_parser *p)
{ char buf[MAXDECL];
  dtd *dtd = p->dtd;
  const ichar *decl = p->buffer->data;
  const ichar *s;

  if ( (decl=isee_func(dtd, decl, CF_MDO2)) && /* ! */
       (decl=isee_func(dtd, decl, CF_DSO)) && /* [ */
       expand_pentities(dtd, decl, buf, sizeof(buf)) )
  { dtd_symbol *kwd;

    decl = buf;
    if ( (s=itake_name(dtd, decl, &kwd)) &&
	 isee_func(dtd, s, CF_DSO) )	/* [ */
    { dtd_marked *m = calloc(1, sizeof(*m));

      m->keyword = kwd;			/* push on the stack */
      m->parent = p->marked;
      p->marked = m;

      if ( istrcaseeq(kwd->name, "IGNORE") )
	m->type = MS_IGNORE;
      else if ( istrcaseeq(kwd->name, "INCLUDE") )
	m->type = MS_INCLUDE;
      else if ( istrcaseeq(kwd->name, "TEMP") )
	m->type = MS_INCLUDE;
      else if ( istrcaseeq(kwd->name, "CDATA") )
	m->type = MS_CDATA;
      else if ( istrcaseeq(kwd->name, "RCDATA") )
	m->type = MS_RCDATA;
      else
	m->type = MS_INCLUDE;		/* default */

      empty_icharbuf(p->buffer);
      if ( m->type == MS_CDATA )
	p->state = S_MSCDATA;
      else
	p->state = S_PCDATA;
      if ( p->mark_state != MS_IGNORE )
	p->mark_state = m->type;
    }
  } else
  { decl = p->buffer->data;

    if ( (decl=isee_func(dtd, decl, CF_MDO2)) && /* ! */
	 !isee_func(dtd, decl, CF_DSO) ) /* [ */
    { p->state = S_GROUP;
      p->grouplevel = 1;
    }
  }
}


static void
pop_marked_section(dtd_parser *p)
{ dtd_marked *m = p->marked;

  if ( m )
  { p->marked = m->parent;
    free(m);
    p->mark_state = (p->marked ? p->marked->type : MS_INCLUDE);
  }
} 


static int
process_cdata(dtd_parser *p)
{ if ( p->cdata->size == 0 )
    return TRUE;

  terminate_ocharbuf(p->cdata);

  if ( p->mark_state == MS_INCLUDE )
  { dtd *dtd = p->dtd;
    const ichar *s, *data = p->cdata->data;
    int len = p->cdata->size;
    int blank = TRUE;

    for(s=data; *s; s++)
    { if ( !HasClass(dtd, *s, CH_BLANK) )
      { blank = FALSE;
	break;
      }
    }   

    if ( !blank )
    { open_element(p, CDATA_ELEMENT);
      if ( p->on_cdata )
	(*p->on_cdata)(p, len, data);
    } else if ( p->environments )
    { sgml_environment *env = p->environments;
      dtd_state *new;
  
      if ( (new=make_dtd_transition(env->state, CDATA_ELEMENT)) )
      { env->state = new;
	if ( p->on_cdata )
	  (*p->on_cdata)(p, len, data);
      }
    }
  }
  
  empty_ocharbuf(p->cdata);

  return TRUE;
}


static int
process_entity(dtd_parser *p, const ichar *name)
{ if ( name[0] == '#' )			/* #charcode: character entity */
  { char *end;
    int v = strtol((char *)&name[1], &end, 10);

    if ( *end == '\0' )
    { if ( v <= 0 || v >= OUTPUT_CHARSET_SIZE )
      { if ( p->on_entity )
	{ process_cdata(p);
	  (*p->on_entity)(p, NULL, v);
	} else
	  return gripe(ERC_REPRESENTATION, "character");
      } else
	add_ocharbuf(p->cdata, v);

      return TRUE;
    } else
      return gripe(ERC_SYNTAX_ERROR, "Bad entity value", name);
  } else
  { dtd_symbol *id;
    dtd_entity *e;
    dtd *dtd = p->dtd;

    if ( (id=dtd_find_symbol(dtd, name)) && (e=id->entity) )
    { const ichar *text = entity_value(dtd, e);
      const ichar *s;
      int   chr;

      if ( !text )
	return gripe(ERC_NO_VALUE, e->name->name);

      if ( (s=isee_character_entity(dtd, text, &chr)) && *s == '\0' )
      { if ( chr > 0 && chr < OUTPUT_CHARSET_SIZE )
	{ add_ocharbuf(p->cdata, chr);
	  return TRUE;
	} else
	{ if ( p->on_entity )
	  { process_cdata(p);
	    (*p->on_entity)(p, e, chr);
	  } else
	    return gripe(ERC_REPRESENTATION, "character");
	}
      } else
      { dtd_srcloc oldloc = p->location;

	set_file_dtd_parser(p, (char *)name);
	empty_icharbuf(p->buffer);		/* dubious */
	for(s=text; *s; s++)
	  putchar_dtd_parser(p, *s);
	p->location = oldloc;
      }

      return TRUE;
    } else
      return gripe(ERC_EXISTENCE, "entity", name);
  }
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Deal with end of input.  We should give a proper error message depending
on the state and the start-location of the error.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
end_document_dtd_parser(dtd_parser *p)
{ switch(p->state)
  { case S_PCDATA:
    { if ( p->dmode == DM_SGML )
      { sgml_environment *env;

	if ( (env=p->environments) )
	{ dtd_element *e;

	  while(env->parent)
	    env = env->parent;

	  pop_to(p, env);
	  e = env->element;
	  if ( e->structure && !e->structure->omit_close )
	    gripe(ERC_OMITTED_CLOSE, e->name->name);
	  close_element(p, e);
	}
      }
      return TRUE;
    }
    case S_CDATA:
    case S_ECDATA1:
    case S_ECDATA2:
    case S_DECL:
    case S_STRING:
    case S_COMMENT:
    case S_GROUP:
    case S_CLOSEMARK:
    case S_PENT:
    case S_ENT:
      return gripe(ERC_SYNTAX_ERROR,
		   "Unexpected end-of-file", "");
#ifdef UTF8
    case S_UTF8:
      return gripe(ERC_SYNTAX_ERROR,
		   "Unexpected end-of-file in UTF-8 sequence", "");
#endif
    case S_MSCDATA:
    case S_EMSCDATA1:
    case S_EMSCDATA2:
      return gripe(ERC_SYNTAX_ERROR,
		   "Unexpected end-of-file in CDATA marked section", "");
  }

  return FALSE;				/* ?? */
}


int
begin_document_dtd_parser(dtd_parser *p)
{
#ifdef UTF8
  dtd *dtd = p->dtd;

  if ( dtd->encoding == ENC_UTF8 &&
       p->encoding   == ENC_ISO_LATIN1 )
  { p->utf8_decode = TRUE;
    fprintf(stderr, "Converting UTF-8 to ISO-Latin-1\n");
  } else
    p->utf8_decode = FALSE;
#endif

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Set the UTF-8 state
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#ifdef UTF8
static void
process_utf8(dtd_parser *p, int chr)
{ int bytes;
  int mask;

  for( bytes=1, mask=0x20; chr&mask; bytes++, mask >>= 1 )
    ;
  mask--;				/* 0x20 --> 0x1f */

  p->saved_state = p->state;		/* state to return to */
  p->state = S_UTF8;
  p->utf8_char = chr & mask;
  p->utf8_left = bytes;
}
#endif


/* We discovered illegal markup and now process it as normal CDATA
*/

static void
recover_parser(dtd_parser *p)
{ const ichar *s;
  dtd *dtd = p->dtd;

  terminate_icharbuf(p->buffer);
  add_ocharbuf(p->cdata, dtd->charmap->map[p->saved]);
  for(s=p->buffer->data; *s; s++)
    add_ocharbuf(p->cdata, dtd->charmap->map[*s]);
  p->state = S_PCDATA;
}


#define WITH_PUBLIC_PARSER(p, g) \
	{ dtd_parser *_old = p; \
	  current_parser = p; \
	  g; \
	  current_parser = _old; \
	}

void
putchar_dtd_parser(dtd_parser *p, int chr)
{ dtd *dtd = p->dtd;
  const ichar *f = dtd->charfunc->func;
  ichar prev = p->previous_char;
  dtd_srcloc old = p->location;

  p->previous_char = chr;

  if ( f[CF_RS] == chr )
  { p->location.line++;
    p->location.linepos = 0;
  }
  p->location.linepos++;
  p->location.charpos++;

  switch(p->state)
  { case S_PCDATA:
    { if ( f[CF_MDO1] == chr )		/* < */
      { p->startloc = old;
	p->state = S_DECL;
	empty_icharbuf(p->buffer);	/* make sure */
	return;
      }
      if ( p->dmode == DM_DTD )
      { if ( f[CF_PERO] == chr )	/* % */
	{ p->startloc = old;
	  p->state = S_PENT;
	  return;
	}
      } else
      { if ( f[CF_ERO] == chr )		/* & */
	{ p->startloc = old;
	  p->state = S_ENT;
	  p->saved = chr;		/* for recovery */
	  return;
	}
      }
      if ( f[CF_DSC] == chr )		/* ] */
      { if ( prev == chr )
	{ pop_marked_section(p);
	  p->state = S_CLOSEMARK;
	}
	return;				/* TBD: error if only one ] */
      }
					/* Real character data */
      switch(p->dmode)
      { case DM_DTD:
	  if ( !HasClass(dtd, chr, CH_BLANK) )
	    gripe(ERC_SYNTAX_ERROR, "Character data in DTD", "");
	  return;
	case DM_SGML:
#ifdef UTF8
	  if ( p->utf8_decode && ISUTF8_MB(chr) )
	  { process_utf8(p, chr);
	    return;
	  }
#endif
	  add_ocharbuf(p->cdata, dtd->charmap->map[chr]);
	  return;
      }
    }
    case S_ECDATA2:			/* Seen </ in CDATA */
    { if ( p->etaglen == p->buffer->size &&
	   istrncaseeq(p->buffer->data, p->etag, p->etaglen) &&
	   f[CF_MDC] == chr )
      { p->cdata->size -= p->etaglen+2;	/* 2 for </ */
	terminate_ocharbuf(p->cdata);
	if ( p->mark_state == MS_INCLUDE )
	{ WITH_PUBLIC_PARSER(p,
			     process_cdata(p);
			     process_end_element(p, p->buffer->data));
	}
	empty_icharbuf(p->buffer);
	empty_ocharbuf(p->cdata);
	p->state = S_PCDATA;
      } else
      { add_ocharbuf(p->cdata, dtd->charmap->map[chr]);
	if ( p->etaglen < p->buffer->size || !HasClass(dtd, chr, CH_NAME))
	{ empty_icharbuf(p->buffer);	/* mismatch */
	  p->state = S_CDATA;
	} else
	  add_icharbuf(p->buffer, chr);
      }
      return;
    }
    case S_ECDATA1:			/* seen < in CDATA */
    { add_ocharbuf(p->cdata, dtd->charmap->map[chr]);
      if ( f[CF_ETAGO2] == chr )	/* / */
      { empty_icharbuf(p->buffer);
	p->state = S_ECDATA2;
      } else
	p->state = S_CDATA;
      return;
    }
    case S_CDATA:
    { add_ocharbuf(p->cdata, dtd->charmap->map[chr]);
      if ( f[CF_MDO1] == chr )		/* < */
	p->state = S_ECDATA1;
      return;
    }
    case S_MSCDATA:
    { add_ocharbuf(p->cdata, dtd->charmap->map[chr]);
      if ( f[CF_DSC] == chr )		/* ] */
        p->state = S_EMSCDATA1;
      return;
    }
    case S_EMSCDATA1:
    { add_ocharbuf(p->cdata, dtd->charmap->map[chr]);
      if ( f[CF_DSC] == chr )		/* ]] */
        p->state = S_EMSCDATA2;
      else
        p->state = S_MSCDATA;
      return;
    }
    case S_EMSCDATA2:
    { add_ocharbuf(p->cdata, dtd->charmap->map[chr]);
      if ( f[CF_MDC] == chr )		/* ]]> */
      { p->cdata->size -= 3;		/* Delete chars for ]] */
	pop_marked_section(p);
	p->state = S_PCDATA;
      } else
        p->state = S_MSCDATA;
      return;
    }
    case S_CLOSEMARK:
    { if ( f[CF_MDC] == chr )
      { p->state = S_PCDATA;
	return;
      }
      if ( !HasClass(dtd, chr, CH_BLANK) )
	gripe(ERC_SYNTAX_ERROR, "Character data in DTD", "");
      return;
    }
    case S_PENT:			/* %parameter entity; */
    { if ( f[CF_ERC] == chr )
      { p->state = S_PCDATA;
	terminate_icharbuf(p->buffer);
	if ( p->mark_state == MS_INCLUDE )
	{ WITH_PUBLIC_PARSER(p, process_include(p, p->buffer->data));
	}
	empty_icharbuf(p->buffer);
	return;
      }
      if ( HasClass(dtd, chr, CH_NAME) )
      { add_icharbuf(p->buffer, chr);
	return;
      }

      terminate_icharbuf(p->buffer);
      gripe(ERC_SYNTAX_ERROR, "Illegal parameter entity", p->buffer->data);
      break;
    }
    case S_ENT:				/* &entity; */
    { if ( f[CF_ERC] == chr )
      { p->state = S_PCDATA;
	terminate_icharbuf(p->buffer);
	if ( p->mark_state == MS_INCLUDE )
	{ WITH_PUBLIC_PARSER(p, process_entity(p, p->buffer->data));
	}
	empty_icharbuf(p->buffer);
	return;
      }
      if ( HasClass(dtd, chr, CH_NAME) || chr == '#' ) /* TBD: abstract name */
      { add_icharbuf(p->buffer, chr);
	return;
      }

      recover_parser(p);
      gripe(ERC_SYNTAX_WARNING, "Illegal entity", p->buffer->data);
      empty_icharbuf(p->buffer);
      break;
    }
    case S_DECL:			/* <...> */
    { if ( f[CF_MDC] == chr )
      { process_cdata(p);

	p->state = S_PCDATA;
	terminate_icharbuf(p->buffer);
	if ( p->mark_state == MS_INCLUDE )
	{ ichar *data = p->buffer->data;

	  WITH_PUBLIC_PARSER(p, process_declaration(p, data));
	}
	empty_icharbuf(p->buffer);
	return;
      }
      if ( f[CF_LIT] == chr )
      { add_icharbuf(p->buffer, chr);
	p->state = S_STRING;
	p->saved = chr;
	return;
      }
      if ( f[CF_LITA] == chr )
      { add_icharbuf(p->buffer, chr);
	p->state = S_STRING;
	p->saved = chr;
	return;
      }
      if ( f[CF_CMT] == chr && prev == chr )
      { del_icharbuf(p->buffer);
	p->state = S_COMMENT;
	return;
      }
      if ( f[CF_DSO] == chr )		/* [: marked section */
      { add_icharbuf(p->buffer, chr);
	terminate_icharbuf(p->buffer);

	process_marked_section(p);
	return;
      }
      add_icharbuf(p->buffer, chr);
      return;
    }
    case S_STRING:
    { add_icharbuf(p->buffer, chr);
      if ( chr == p->saved )
	p->state = S_DECL;
      break;
    }
    case S_COMMENT:
    { if ( f[CF_CMT] == chr && prev == chr )
	p->state = S_DECL;
      break;
    }
    case S_GROUP:
    { add_icharbuf(p->buffer, chr);
      if ( f[CF_DSO] == chr )
	p->grouplevel++;
      else if ( f[CF_DSC] == chr )
      { if ( --p->grouplevel == 0 )
	  p->state = S_DECL;
      }
      break;
    }
#ifdef UTF8
    case S_UTF8:
    { if ( (chr & 0xc0) != 0x80 )	/* TBD: recover */
	gripe(ERC_SYNTAX_ERROR, "Bad UTF-8 sequence", "");
      p->utf8_char <<= 6;
      p->utf8_char |= (chr & 0xc0);
      if ( --p->utf8_left == 0 )
      { add_ocharbuf(p->cdata, p->utf8_char);
	p->state = p->saved_state;
      }
    }
#endif
  }
}


		 /*******************************
		 *	     TOPLEVEL		*
		 *******************************/

int
load_dtd_from_file(dtd_parser *p, const char *file)
{ FILE *fd;
  int rval;
  data_mode   oldmode  = p->dmode;
  dtdstate    oldstate = p->state;
  dtd_srcloc  oldloc   = p->location;

  p->dmode = DM_DTD;
  p->state = S_PCDATA;
  empty_icharbuf(p->buffer);		/* dubious */
  set_file_dtd_parser(p, file);

  if ( (fd = fopen(file, "rb")) )
  { int chr;

    while( (chr = getc(fd)) != EOF )
      putchar_dtd_parser(p, chr);

    p->dtd->implicit = FALSE;
    rval = TRUE;
  } else
    rval = FALSE;

  p->location = oldloc;
  p->dmode = oldmode;
  p->state = oldstate;

  return rval;
}


dtd *
file_to_dtd(const char *file, const char *doctype)
{ dtd_parser *p = new_dtd_parser(new_dtd(doctype));

  if ( load_dtd_from_file(p, file) )
  { dtd *dtd = p->dtd;

    free_dtd_parser(p);
    return dtd;
  } else
  { free_dtd_parser(p);

    return NULL;
  }
}


int
sgml_process_file(dtd_parser *p, const char *file)
{ FILE *fd;
  int rval;
  dtd_srcloc oldloc = p->location;

  set_file_dtd_parser(p, file);
  p->dmode = DM_SGML;
  p->state = S_PCDATA;

  if ( (fd = fopen(file, "rb")) )
  { int chr;

    while( (chr = getc(fd)) != EOF )
      putchar_dtd_parser(p, chr);

    rval = TRUE;
  } else
    rval = FALSE;

  p->location = oldloc;

  return rval;
}



		 /*******************************
		 *	       ERRORS		*
		 *******************************/

static char *
format_message(dtd_error *e)
{ char buf[1024];
  char *s;

  switch(e->severity)
  { case ERS_ERROR:
      strcpy(buf, "Error: ");
      break;
    case ERS_WARNING:
      strcpy(buf, "Warning: ");
      break;
    default:
      buf[0] = '\0';
  }
  s = buf+strlen(buf);

  if ( e->file )
  { sprintf(s, "%s:%ld: ", e->file, e->line);
    s += strlen(s);
  }

  switch(e->id)
  { case ERC_REPRESENTATION:
      sprintf(s, "Cannot represent due to %s", e->argv[0]);
      break;
    case ERC_RESOURCE:
      sprintf(s, "Insufficient %s resources", e->argv[0]);
      break;
    case ERC_VALIDATE:
      sprintf(s, "Document does not match DTD: %s", e->argv[0]);
      break;
    case ERC_SYNTAX_ERROR:
      sprintf(s, "Syntax error: %s", e->argv[0]);
      break;
    case ERC_EXISTENCE:
      sprintf(s, "%s \"%s\" does not exist", e->argv[0], e->argv[1]);
      break;
    case ERC_REDEFINED:
      sprintf(s, "Redefined %s \"%s\"", e->argv[0], e->argv[1]);
      break;
    default:
      ;
  }

  return str2ring(buf);
}


int
gripe(dtd_error_id e, ...)
{ va_list args;
  char buf[1024];
  dtd_error error;
  int dtdmode = FALSE;

  va_start(args, e);

  memset(&error, 0, sizeof(error));

  if ( current_parser )
  { error.file = current_parser->startloc.file;
    error.line = current_parser->startloc.line;
    if ( current_parser->dmode == DM_DTD )
      dtdmode = TRUE;
  } else
  { error.file = NULL;
    error.line = -1;
  }

  switch(e)
  { case ERC_REPRESENTATION:
    case ERC_RESOURCE:
      error.severity = ERS_ERROR;
      error.argv[0]  = va_arg(args, char *);
      break;
    case ERC_SYNTAX_ERROR:
    case ERC_SYNTAX_WARNING:
    { char *m = va_arg(args, char *);
      const char *s = va_arg(args, const char *);

      if ( s && *s )
      { sprintf(buf, "%s, found \"%s\"", m, str_summary(s, 25));
	error.argv[0] = buf;
      } else
	error.argv[0] = m;
      
      error.severity = (e == ERC_SYNTAX_WARNING ? ERS_WARNING : ERS_ERROR);
      e = ERC_SYNTAX_ERROR;
      break;
    }
    case ERC_DOMAIN:
    { const char *expected = va_arg(args, const char *);
      const char *found    = str_summary(va_arg(args, const char *), 25);

      sprintf(buf, "Expected type %s, found \"%s\"", expected, found);
      error.argv[0] = buf;
      error.severity = ERS_ERROR;
      e = (dtdmode ? ERC_SYNTAX_ERROR : ERC_VALIDATE);
      break;
    }
    case ERC_REDEFINED:
    { error.argv[0] = va_arg(args, char *); /* type */
      error.argv[1] = va_arg(args, char *); /* name */
      error.severity = ERS_WARNING;
      break;
    }
    case ERC_EXISTENCE:
    { error.argv[0] = va_arg(args, char *); /* type */
      error.argv[1] = va_arg(args, char *); /* name */
      error.severity = ERS_ERROR;
      break;
    }
    case ERC_VALIDATE:
    { error.argv[0] = va_arg(args, char *); /* message */
      error.severity = ERS_WARNING;
      break;
    }
    case ERC_OMITTED_CLOSE:
    { const char *element = va_arg(args, const char *); 

      sprintf(buf, "Inserted omitted close tag for <%s>", element);
      error.argv[0] = buf;
      error.severity = ERS_WARNING;
      e = ERC_VALIDATE;
      break;
    }
    case ERC_NOT_OPEN:
    { const char *element = va_arg(args, const char *); 

      sprintf(buf, "Ignored close tag for </%s> which is not open", element);
      error.argv[0] = buf;
      error.severity = ERS_WARNING;
      e = ERC_VALIDATE;
      break;
    }
    case ERC_NOT_ALLOWED:
    { const char *element = va_arg(args, const char *); 

      sprintf(buf, "Element <%s> not allowed here", element);
      error.argv[0] = buf;
      error.severity = ERS_WARNING;
      e = ERC_VALIDATE;
      break;
    }
    case ERC_NO_ATTRIBUTE:
    { const char *elem = va_arg(args, char *); /* element */
      const char *attr = va_arg(args, char *); /* attribute */

      sprintf(buf, "Element <%s> does has no attribute %s", elem, attr);
      error.argv[0] = buf;
      error.severity = ERS_WARNING;

      e = ERC_VALIDATE;
      break;
    }
    case ERC_NO_ATTRIBUTE_VALUE:
    { const char *elem  = va_arg(args, char *); /* element */
      const char *value = va_arg(args, char *); /* attribute value */

      sprintf(buf, "Element <%s> has no attribute with value %s", elem, value);
      error.argv[0] = buf;
      error.severity = ERS_WARNING;

      e = ERC_VALIDATE;
      break;
    }
    case ERC_NO_VALUE:
    { error.argv[0] = "entity value";
      error.argv[1] = va_arg(args, char *); /* entity */

      error.severity = ERS_ERROR;
      e = ERC_EXISTENCE;
      break;
    }
  } 

  error.id      = e;
  error.message = format_message(&error);

  if ( current_parser && current_parser->on_error )
    (*current_parser->on_error)(current_parser, &error);
  else
    fprintf(stderr, "SGML: %s\n", error.message);

  va_end(args);

  return FALSE;
}
