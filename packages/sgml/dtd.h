/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2002, University of Amsterdam

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#ifndef DTD_H_INCLUDED
#define DTD_H_INCLUDED
#include "sgmldefs.h"

#define CH_WHITE	0x0001
#define CH_LCLETTER	0x0002
#define CH_UCLETTER	0x0004
#define CH_CNMSTRT	0x0008		/* may start a name */
#define CH_CNM		0x0010		/* may be in a name */
#define CH_DIGIT	0x0020
#define CH_RE		0x0040
#define CH_RS		0x0080

#define CH_LETTER	(CH_LCLETTER|CH_UCLETTER)
#define CH_NMSTART	(CH_LCLETTER|CH_UCLETTER|CH_CNMSTRT)
#define CH_NAME		(CH_NMSTART|CH_DIGIT|CH_CNM)
#define CH_BLANK	(CH_WHITE|CH_RE|CH_RS)

#define CHR_BLANK	0x1		/* SHORTREF 'B' */
#define CHR_DBLANK	0x2		/* SHORTREF 'BB' */

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
  CF_NG,				/* , or & or | */
  CF_ENDTABLE				/* to find size */
} charfunc;				/* function of characters */

typedef enum
{ ENC_ISO_LATIN1,			/* ISO-Latin-1 */
  ENC_UTF8				/* Multi-byte UTF-8 encoding */
} dtd_char_encoding;

typedef enum
{ C_CDATA,				/* pure cdata */
  C_PCDATA,				/* parsed character data */
  C_RCDATA,				/* pure cdata + entities */
  C_EMPTY,				/* empy element */
  C_ANY					/* element may contain anything */
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
{ EC_SGML,				/* SGML data */
  EC_STARTTAG,				/* SGML start-tag */
  EC_ENDTAG,				/* SGML end-tag */
  EC_CDATA,				/* CDATA entity */
  EC_SDATA,				/* SDATA entity */
  EC_NDATA,				/* non-sgml data */
  EC_PI					/* Programming instruction */
} data_type;


typedef enum
{ DL_SGML,				/* Use SGML */
  DL_XML,				/* Use XML */
  DL_XMLNS				/* Use XML + Namespaces */
} dtd_dialect;


typedef enum
{ OPT_SHORTTAG				/* do/don't accept shorttag */
} dtd_option;


typedef enum
{ SP_PRESERVE = 0,			/* Preserve all white-space */
  SP_DEFAULT,				/* Default space handling */
  SP_REMOVE,				/* Remove all blank CDATA elements */
  SP_SGML,				/* Compliant SGML mode */
  SP_INHERIT				/* DTD: inherit from environment */
} dtd_space_mode;


typedef enum
{ NU_TOKEN,				/* Treat numbers as tokens */
  NU_INTEGER				/* Convert to integer */
} dtd_number_mode;


		 /*******************************
		 *	      ERRORS		*
		 *******************************/

#ifdef DTD_IMPLEMENTATION
#define DTD_MINOR_ERRORS 1
#endif

typedef enum
{ ERS_WARNING,				/* probably correct result */
  ERS_ERROR,				/* probably incrorrect result */
  ERS_STYLE				/* dubious/bad style; correct result */
} dtd_error_severity;


typedef enum
{ ERC_REPRESENTATION,			/* Internal limit */
	/* id */
  ERC_RESOURCE,				/* external limit */
	/* id */
  ERC_LIMIT,				/* Exceeded SGML limit */
	/* id */
  ERC_VALIDATE,				/* DTD Validation */
	/* Message */
  ERC_SYNTAX_ERROR,			/* Syntax error */
	/* Message, found */
  ERC_EXISTENCE,			/* Existence error */
	/* Type, name */
  ERC_REDEFINED				/* Redefined object */
	/* Type, name */
#ifdef DTD_MINOR_ERRORS
  ,					/* reopen list */
  ERC_SYNTAX_WARNING,			/* Syntax warning (i.e. fixed) */
	/* Message, found */
  ERC_DOMAIN,				/* Relative to declared type */
	/* Type, found */
  ERC_OMITTED_CLOSE,
	/* Element */
  ERC_OMITTED_OPEN,
	/* Element */
  ERC_NOT_OPEN,
	/* Element */
  ERC_NOT_ALLOWED,
	/* Element */
  ERC_NO_ATTRIBUTE,
	/* Element, Attribute */
  ERC_NO_ATTRIBUTE_VALUE,
	/* Element, Value */
  ERC_NO_VALUE,
	/* Entity */
  ERC_NO_DOCTYPE
        /* Implicit, file */
#endif
} dtd_error_id;


typedef enum
{ IN_NONE,				/* unspecified input */
  IN_FILE,				/* input from file */
  IN_ENTITY				/* input from entity */
} input_type;


typedef struct _dtd_srcloc
{ input_type  type;			/* type of input */
  const char *name;			/* name of the file */
  int	      line;			/* 1-based Line no */
  int	      linepos;			/* 1-based char  */
  long	      charpos;			/* 0-based file char  */
  struct _dtd_srcloc *parent;		/* parent location */
} dtd_srcloc;


typedef struct _dtd_error
{ dtd_error_id id;			/* ERC_* identifier */
  dtd_error_id minor;			/* Minor code */
  dtd_error_severity severity;		/* ERS_* severity */
  dtd_srcloc *location;			/* location of the error */
  char *plain_message;			/* Clean message */
  char *message;			/* complete message */
					/* (Warning: file:line: <plain>) */
  char *argv[2];			/* context arguments */
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
  data_type content;			/* EC_* */
  int catalog_location;			/* what catalog to use for lookup */
  int length;				/* size of literal value */
  ichar *value;				/* literal value */
  ichar *extid;				/* external identifier */
  ichar *exturl;			/* url to fetch from */
  ichar *baseurl;			/* base url for exturl */
  struct _dtd_entity *next;		/* list-link */
} dtd_entity;


typedef struct _dtd_notation
{ dtd_symbol *name;			/* name of the notation */
  entity_type type;			/* ET_{PUBLIC|SYSTEM} */
  ichar *public;			/* public id */
  ichar *system;			/* file with info */
  struct _dtd_notation *next;		/* list-link */
} dtd_notation;


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
  int islist;				/* attribute is a list */
  union
  { dtd_name_list *nameof;		/* (name1|name2|...) */
  } typeex;
  union
  { ochar *cdata;			/* default for CDATA */
    ichar *list;			/* text for list-data */
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


typedef struct _dtd_map
{ ichar	       *from;			/* mapped text */
  int		len;			/* length of mapped text */
  dtd_symbol   *to;			/* name of symbol mapped onto */
  struct _dtd_map *next;		/* next in shortref map */
} dtd_map;


typedef struct _dtd_shortref
{ dtd_symbol	*name;			/* name of SHORTREF map */
  dtd_map	*map;			/* implemented map */
  char		ends[ICHARSET_SIZE];	/* ending-characters in map */
  int		defined;		/* has been defined */
  struct _dtd_shortref *next;		/* next declared shortref */
} dtd_shortref;


typedef struct _dtd_element
{ dtd_symbol	*name;			/* its name */
  dtd_edef	*structure;		/* content structure of the element */
  dtd_attr_list *attributes;		/* defined attributes */
  dtd_space_mode space_mode;		/* How to handle white-space (SP_*) */
  dtd_shortref	*map;			/* SHORTREF map */
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
  int			ent_case_sensitive; /* Entities are case-sensitive */
  ichar		       *doctype;	/* defined document type */
  dtd_symbol_table     *symbols;	/* symbol-table */
  dtd_entity           *pentities;	/* defined parameter entities */
  dtd_entity	       *entities;	/* defined entities */
  dtd_entity	       *default_entity;	/* default-entity (if any) */
  dtd_notation	       *notations;	/* Declared notations */
  dtd_shortref	       *shortrefs;	/* SHORTREF declarations */
  dtd_element          *elements;	/* defined elements */
  dtd_charfunc	       *charfunc;	/* CF_ --> ichar */
  dtd_charclass	       *charclass;	/* ichar -> CH_-mask */
  dtd_charmap	       *charmap;	/* ichar ->ochar */
  dtd_char_encoding	encoding;	/* document encoding */
  dtd_space_mode	space_mode;	/* Default for handling white-space */
  dtd_number_mode	number_mode;	/* How to treat number attributes */
  int			shorttag;	/* support SHORTTAG */
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

#include "parser.h"

dtd *		file_to_dtd(const char *file, const char *doctype,
			    dtd_dialect dialect);
int		sgml_process_file(dtd_parser *p,
				  const char *file, unsigned flags);
int		sgml_process_stream(dtd_parser *p, FILE *in,
				    unsigned flags);
dtd_parser *	new_dtd_parser(dtd *dtd);
void		free_dtd_parser(dtd_parser *p);

void		free_dtd(dtd *dtd);
int		load_dtd_from_file(dtd_parser *p, const char *file);
dtd *		new_dtd(const ichar *doctype);
int		set_dialect_dtd(dtd *dtd, dtd_dialect dialect);
int		set_option_dtd(dtd *dtd, dtd_option option, int set);

void		putchar_dtd_parser(dtd_parser *p, int chr);
int		begin_document_dtd_parser(dtd_parser *p);
int		end_document_dtd_parser(dtd_parser *p);
void		reset_document_dtd_parser(dtd_parser *p);
void		set_src_dtd_parser(dtd_parser *p,
				   input_type in, const char *file);
void		set_mode_dtd_parser(dtd_parser *p, data_mode mode);
void		sgml_cplocation(dtd_srcloc *dst, dtd_srcloc *src);

#endif /*DTD_H_INCLUDED*/


