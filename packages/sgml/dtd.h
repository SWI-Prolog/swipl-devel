/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#ifndef DTD_H_INCLUDED
#define DTD_H_INCLUDED
#include "sgmldefs.h"

#define CH_BLANK	0x0001
#define CH_LCLETTER	0x0002
#define CH_UCLETTER	0x0004
#define CH_LCNMSTRT	0x0008
#define CH_UCNMSTRT	0x0010
#define CH_DIGIT	0x0020

#define CH_NMSTART	(CH_LCLETTER|CH_UCLETTER|CH_LCNMSTRT|CH_UCNMSTRT)
#define CH_NAME		(CH_NMSTART|CH_DIGIT)

#define SGML_DTD_MAGIC	0x7364573

typedef enum
{ CF_STAGO = 0,				/* < */
  CF_STAGC,				/* > */
  CF_ETAGO1,				/* < */
  CF_ETAGO2,				/* / */
  CF_VI,				/* = */
  CF_NS,				/* : (XMLNS) */
  CF_LIT,				/* " */
  CF_LITA,				/* ' */
  CF_PERO,				/* % */
  CF_ERO,				/* & */
  CF_ERC,				/* ; */
  CF_MDO1,				/* < */
  CF_MDO2,				/* ! (MDO=<!) */
  CF_MDC,				/* > */
  CF_PRO1,				/* < */
  CF_PRO2,				/* ? (PRO=<?) */
  CF_PRC,				/* > */
  CF_GRPO,				/* ( */
  CF_GRPC,				/* ) */
  CF_SEQ,				/* , */
  CF_AND,				/* & */
  CF_OR,				/* | */
  CF_OPT,				/* ? */
  CF_PLUS,				/* + */
  CF_DSO,				/* [ */
  CF_DSC,				/* ] */
  CF_REP,				/* * */
  CF_RS,				/* \n */
  CF_RE,				/* \r */
  CF_CMT,				/* - */
  CF_ENDTABLE				/* to find size */
} charfunc;				/* function of characters */

typedef enum
{ ENC_ISO_LATIN1,			/* ISO-Latin-1 */
  ENC_UTF8				/* Multi-byte UTF-8 encoding */
} dtd_char_encoding;

typedef enum
{ C_CDATA,				/* pure cdata */
  C_PCDATA,				/* parsed character data */
  C_EMPTY				/* empy element */
} contenttype;

typedef enum
{ MC_ONE,				/* one time */
  MC_OPT,				/* optional element (?) */
  MC_REP,				/* any times (*) */
  MC_PLUS				/* one-or-more (+) */
} modelcard;

typedef enum
{ MT_UNDEF = 0,				/* undefined */
  MT_PCDATA,				/* Contains PCDATA */
  MT_ELEMENT,				/* refers to element */
  MT_SEQ,				/* Sequence (,) */
  MT_AND,				/* Ony order (&) */
  MT_OR					/* Disjunction (|) */
} modeltype;

typedef enum
{ AT_CDATA,				/* CDATA attribute */
  AT_ENTITY,				/* entity-name */
  AT_ENTITIES,				/* entity-name list */
  AT_ID,				/* identifier */
  AT_IDREF,				/* identifier reference */
  AT_IDREFS,				/* list of identifier references */
  AT_NAME,				/* name token */
  AT_NAMES,				/* list of names */
  AT_NAMEOF,				/* one of these names */
  AT_NMTOKEN,				/* name-token */
  AT_NMTOKENS,				/* name-token list */
  AT_NOTATION,				/* notation-name */
  AT_NUMBER,				/* number */
  AT_NUMBERS,				/* number list */
  AT_NUTOKEN,				/* number token */
  AT_NUTOKENS				/* number token list */
} attrtype;

typedef enum
{ AT_FIXED,				/* fixed value */
  AT_REQUIRED,				/* Required attribute */
  AT_CURRENT,				/* most recent value */
  AT_CONREF,				/* cross-reference */
  AT_IMPLIED,				/* Implied attribute */
  AT_DEFAULT				/* has default */
} attrdef;


typedef enum
{ ET_SYSTEM,				/* System (file) entity */
  ET_PUBLIC,				/* Public (external) entity */
  ET_LITERAL				/* Literal text */
} entity_type;


typedef enum
{ DL_SGML,				/* Use SGML */
  DL_XML,				/* Use XML */
  DL_XMLNS				/* Use XML + Namespaces */
} dtd_dialect;


		 /*******************************
		 *	      ERRORS		*
		 *******************************/

typedef enum
{ ERS_WARNING,
  ERS_ERROR
} dtd_error_severity;


typedef enum
{ ERC_REPRESENTATION,			/* Internal limit */
	/* id */
  ERC_RESOURCE,				/* external limit */
	/* id */
  ERC_VALIDATE,				/* DTD Validation */
	/* Message */
  ERC_SYNTAX_ERROR,			/* Syntax error */
	/* Message, found */
  ERC_EXISTENCE,			/* Existence error */
	/* Type, name */
  ERC_REDEFINED				/* Redefined object */
	/* Type, name */
#ifdef DTD_IMPLEMENTATION
  ,					/* reopen list */
  ERC_SYNTAX_WARNING,			/* Syntax warning (i.e. fixed) */
	/* Message, found */
  ERC_DOMAIN,				/* Relative to declared type */
	/* Type, found */
  ERC_OMITTED_CLOSE,
	/* Element */
  ERC_NOT_OPEN,
	/* Element */
  ERC_NOT_ALLOWED,
	/* Element */
  ERC_NO_ATTRIBUTE,
	/* Element, Attribute */
  ERC_NO_ATTRIBUTE_VALUE,
	/* Element, Value */
  ERC_NO_VALUE
	/* Entity */
#endif
} dtd_error_id;


typedef struct _dtd_error
{ dtd_error_id id;			/* ERC_* identifier */
  dtd_error_severity severity;		/* ERS_* severity */
  const char *file;			/* file (or NULL) */
  long line;				/* Related line */
  char *message;			/* complete message */
  char *argv[2];			/* context earguments */
} dtd_error;


		 /*******************************
		 *	     DTD TYPES		*
		 *******************************/

typedef struct _dtd_symbol
{ const ichar *name;			/* name of the atom */
  struct _dtd_symbol *next;		/* next in atom list */
  struct _dtd_element *element;		/* connected element (if any) */
  struct _dtd_entity  *entity;		/* connected entity (if any) */
} dtd_symbol;


typedef struct _dtd_symbol_table
{ int		size;			/* Allocated size */
  dtd_symbol  **entries;		/* Entries */
} dtd_symbol_table;


typedef struct _dtd_entity
{ dtd_symbol *name;			/* its name */
  entity_type type;			/* ET_* */
  ichar *value;				/* literal value */
  ichar *extid;				/* external identifier */
  ichar *exturl;			/* url to fetch from */
  struct _dtd_entity *next;		/* list-link */
} dtd_entity;


typedef struct _dtd_element_list
{ struct _dtd_element *value;		/* element */
  struct _dtd_element_list *next;	/* next in list */
} dtd_element_list;


typedef struct _dtd_name_list
{ dtd_symbol	*value;
  struct _dtd_name_list *next;
} dtd_name_list;


typedef struct _dtd_attr
{ dtd_symbol  *name;			/* name of attribute */
  attrtype type;			/* type (AT_*) */
  attrdef  def;				/* AT_REQUIRED/AT_IMPLIED */
  union
  { dtd_name_list *nameof;		/* (name1|name2|...) */
  } typeex;
  union
  { ochar *cdata;			/* default for CDATA */
    dtd_symbol *name;			/* AT_NAME or AT_NAMEOF */
    long number;			/* AT_NUMBER */
  } att_def;
  int references;			/* reference count */
} dtd_attr;


typedef struct _dtd_attr_list
{ dtd_attr	*attribute;
  struct _dtd_attr_list *next;
} dtd_attr_list;


typedef struct _dtd_model
{ modeltype type;			/* MT_* */
  modelcard cardinality;		/* MC_* */

  union
  { struct _dtd_model *group;		/* ,/|/& group */
    struct _dtd_element *element;	/* element */
  } content;
  struct _dtd_model *next;		/* next in list (for groups) */
} dtd_model;


typedef struct _dtd_edef
{ contenttype	type;			/* EMPTY, MIXED, ... */
  int		omit_open;		/* allow omitted open tag? */
  int		omit_close;		/* allow omitted close tag? */
  dtd_model	*content;		/* the content model */
  dtd_element_list *included;		/* +(namegroup) */
  dtd_element_list *excluded;		/* -(namegroup) */
  struct _dtd_state *initial_state;	/* Initial state in state engine */
  struct _dtd_state *final_state;	/* Final state in state engine */
  int		references;		/* #elements using this def */
} dtd_edef;


typedef struct _dtd_element
{ dtd_symbol	*name;			/* its name */
  dtd_edef	*structure;		/* content structure of the element */
  dtd_attr_list *attributes;		/* defined attributes */
  int		undefined;		/* Only implicitely defined */
  struct _dtd_element *next;		/* in DTD'e element list */
} dtd_element;


typedef struct _dtd_charmap
{ ochar		map[INPUT_CHARSET_SIZE]; /* ichar --> ochar */
} dtd_charmap;


typedef struct _dtd_charclass
{ unsigned char	class[INPUT_CHARSET_SIZE]; /* ichar --> class-mask */
} dtd_charclass;


typedef struct _dtd_charfunc
{ ichar func[(int)CF_ENDTABLE];		/* CF_ --> ichar */
} dtd_charfunc;


typedef struct _dtd
{ int		        magic;		/* SGML_DTD_MAGIC */
  int			implicit;	/* There is no DTD */
  dtd_dialect		dialect;	/* DL_* */
  int			case_sensitive;	/* Tags are case-sensitive */
  ichar		       *doctype;	/* defined document type */
  dtd_symbol_table     *symbols;	/* symbol-table */
  dtd_entity           *pentities;	/* defined parameter entities */
  dtd_entity	       *entities;	/* defined entities */
  dtd_element          *elements;	/* defined elements */
  dtd_charfunc	       *charfunc;	/* CF_ --> ichar */
  dtd_charclass	       *charclass;	/* ichar -> CH_-mask */
  dtd_charmap	       *charmap;	/* ichar ->ochar */
  dtd_char_encoding	encoding;	/* document encoding */
  int			references;	/* destruction reference count */
} dtd;

extern dtd_charfunc *new_charfunc(void);   /* default classification */
extern dtd_charclass *new_charclass(void); /* default classification */
extern dtd_charmap  *new_charmap(void);	   /* identity map */

extern dtd_symbol*	dtd_find_symbol(dtd *dtd, const ichar *name);
extern dtd_symbol*	dtd_add_symbol(dtd *dtd, const ichar *name);


		 /*******************************
		 *	       PUBLIC		*
		 *******************************/

#ifndef DTD_IMPLEMENTATION
#if 0
typedef void *	dtd_parser;		/* abstract parser handle */
#else
#include "parser.h"
#endif

dtd *		file_to_dtd(const char *file, const char *doctype);
int		sgml_process_file(dtd_parser *p, const char *file);

dtd_parser *	new_dtd_parser(dtd *dtd);
void		free_dtd_parser(dtd_parser *p);

void		free_dtd(dtd *dtd);
int		load_dtd_from_file(dtd_parser *p, const char *file);
dtd *		new_dtd(const ichar *doctype);
int		set_dialect_dtd(dtd *dtd, dtd_dialect dialect);

void		putchar_dtd_parser(dtd_parser *p, int chr);
int		begin_document_dtd_parser(dtd_parser *p);
int		end_document_dtd_parser(dtd_parser *p);
void		set_file_dtd_parser(dtd_parser *p, const char *file);
#endif /*DTD_IMPLEMENTATION*/


#endif /*DTD_H_INCLUDED*/


