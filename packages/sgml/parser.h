/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#ifndef SGML_PARSER_H_INCLUDED
#define SGML_PARSER_H_INCLUDED
#include "util.h"

		 /*******************************
		 *	      CALL-BACK		*
		 *******************************/

					/* sgml_attribute->flags */
#define SGML_AT_DEFAULT		0x1

typedef struct _sgml_attribute
{ struct				/* so we can free members */
  { ochar *cdata;			/* CDATA value */
    ichar *text;			/* other textual value */
    long   number;			/* numeric value */
  } value;
  dtd_attr *definition;			/* DTD definition */
  unsigned flags;			/* additional flags */
} sgml_attribute;

typedef struct _dtd_parser *dtd_parser_p;

typedef int (*sgml_begin_element_f)(dtd_parser_p parser,
				    dtd_element *e,
				    int argc,
				    sgml_attribute *argv);
typedef int (*sgml_end_element_f)(dtd_parser_p parser,
				  dtd_element *e);
typedef int (*sgml_data_f)(dtd_parser_p parser,
			   data_type type, int len, const ochar *text);
typedef int (*sgml_entity_f)(dtd_parser_p parser,
			     dtd_entity *entity,
			     int chr);
typedef int (*sgml_pi_f)(dtd_parser_p parser, const ichar *pi);
typedef int (*sgml_error_f)(dtd_parser_p parser,
			    dtd_error *error);
typedef int (*sgml_decl_f)(dtd_parser_p parser, const ichar *decl);
#ifdef XMLNS
typedef int (*xmlns_f)(dtd_parser_p parser,
		       dtd_symbol *ns, dtd_symbol *url);
#endif


		 /*******************************
		 *	 PARSER AND STATES	*
		 *******************************/

#define SGML_PARSER_MAGIC	(0x834ab663)

typedef enum
{ S_PCDATA,				/* between declarations */
#ifdef UTF8
  S_UTF8,				/* Loading UTF-8 character */
#endif
  S_CDATA,				/* non-parsed data */
  S_RCDATA,				/* CDATA+entities */
  S_MSCDATA,				/* <![CDATA[...]]> */
  S_EMSCDATA1,				/* Seen ] in S_MSCDATA */
  S_EMSCDATA2,				/* Seen ]] in S_MSCDATA */
  S_ECDATA1,				/* Seen < in CDATA */
  S_ECDATA2,				/* Seen </ in CDATA */
  S_EMSC1,				/* Seen ] in marked section */
  S_EMSC2,				/* Seen ]] in marked section */
  S_PI,					/* Seen <? */
  S_PI2,				/* Seen <?...? */
  S_DECL0,				/* Seen < */
  S_DECL,				/* inside a declaration */
  S_MDECL0,				/* Seen <! */
  S_STRING,				/* inside a "string" or 'string' */
  S_DECLCMT0,				/* Seen <...- */
  S_DECLCMT,				/* Seen <...-- */
  S_DECLCMTE0,				/* Seen <...--..- */
  S_CMTO,				/* Seen <!- */
  S_CMT,				/* Seen <!--... */
  S_CMTE0,				/* Seem <!--...- */
  S_CMTE1,				/* Seem <!--...-- */
  S_GROUP,				/* inside [...] */
  S_PENT,				/* Seen % */
  S_ENT0,				/* Seen & */
  S_ENT,				/* Seen &(#|\w) */
  S_SHORTTAG_CDATA			/* Seen <tag/ */
} dtdstate;


typedef enum
{ DCL_DTD,				/* DTD Declaration */
  DCL_BEGIN,				/* begin-tag */
  DCL_END,				/* end-tag */
} dcl_type;


typedef enum
{ MS_IGNORE,				/* ignore this data */
  MS_INCLUDE,				/* process normally */
  MS_CDATA,				/* pass literally */
  MS_RCDATA				/* replace entities */
} marktype;


typedef enum
{ EV_EXPLICIT,				/* Explicit event */
  EV_OMITTED,				/* Omitted tag event */
  EV_SHORTTAG,				/* SHORTTAG event: <tag/value/ */
  EV_SHORTREF				/* SHORTREF event */
} sgml_event_class;


typedef struct _dtd_marked
{ dtd_symbol *keyword;			/* keyword of the marked section */
  marktype	type;			/* processing type */
  struct _dtd_marked *parent;		/* parent marked section */
} dtd_marked;


typedef enum
{ DM_DTD,				/* DTD mode: no data allowed (?) */
  DM_DATA				/* Environment has only elements */
} data_mode;


typedef struct _sgml_environment
{ dtd_element *element;			/* element that opened the env */
  struct _dtd_state *state;		/* State we are in */
#ifdef XMLNS
  struct _xmlns *xmlns;			/* XML namespace */
  struct _xmlns *thisns;		/* Name space of element */
#endif
  dtd_space_mode space_mode;		/* How to handle blanks */
  dtd_shortref *map;			/* SHORTREF map */
  struct _sgml_environment *parent;	/* Parent environment */
} sgml_environment;

					/* parser->flags */
#define SGML_PARSER_NODEFS	0x1	/* don't handle default atts */

typedef struct _dtd_parser
{ long     magic;			/* SGML_PARSER_MAGIC */
  dtd     *dtd;				/* DTD we are building */
  dtdstate state;			/* current state */
  dtdstate cdata_state;			/* S_CDATA/S_RCDATA */
  dtd_marked *marked;			/* marked section stack */
  marktype mark_state;			/* processing mode */
  sgml_environment *environments;	/* Open environments */
  data_mode dmode;			/* How to handle characters */
  int	   first;			/* Just seen <tag> */
  icharbuf *buffer;			/* buffer for temp data */
  ocharbuf *cdata;			/* collected character data */
  int	   blank_cdata;			/* CDATA is all blank */
  int	   cdata_must_be_empty;		/* Only shortrefs allowed here */
  const ichar *etag;			/* name of end-tag in CDATA */
  int	   etaglen;			/* length of end-tag */
  int	   grouplevel;			/* [..] level in declaration */
  int	   saved;			/* saved character */
  dtd_char_encoding encoding;		/* CDATA output character-set */
  dtd_shortref *map;			/* SHORTREF map */
#ifdef UTF8
  int	   utf8_decode;			/* decode UTF-8 sequences? */
  int      utf8_char;			/* building character */
  int	   utf8_left;			/* bytes left */
  dtdstate saved_state;			/* state from which we come */
#endif
  dtd_srcloc	location;		/* Current location */
  dtd_srcloc	startloc;		/* Start of last markup */
  dtd_srcloc	startcdata;		/* Start of last cdata */
  dtd_symbol   *enforce_outer_element;	/* Outer element to look for */
  sgml_event_class event_class;		/* EV_* */

  void *closure;			/* client handle */
  sgml_begin_element_f	on_begin_element; /* start an element */
  sgml_end_element_f	on_end_element;	/* end an element */
  sgml_data_f		on_data;	/* process cdata */
  sgml_entity_f		on_entity;	/* unprocessed entity */
  sgml_pi_f		on_pi;		/* processing instruction */
  sgml_error_f		on_error;	/* handle error */
  sgml_decl_f		on_decl;	/* handle declarations */
#ifdef XMLNS
  xmlns_f		on_xmlns;	/* handle new namespace */
#endif
  unsigned		flags;		/* misc flags */
} dtd_parser;


#ifdef XMLNS
#include "xmlns.h"
#endif

extern int		gripe(dtd_error_id e, ...);

#endif /*SGML_PARSER_H_INCLUDED*/

