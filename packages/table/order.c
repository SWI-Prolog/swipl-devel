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

#include <SWI-Prolog.h>
#ifdef HAVE_MALLOC_H
#include <malloc.h>
#endif
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "order.h"
#include "error.h"

#ifndef TRUE
#define TRUE 1
#define FALSE 0
#endif

static void	exact_table(OrdTable ot);
static void	case_insensitive_table(OrdTable ot);
static void	iso_latin_1_table(OrdTable ot);
static void	iso_latin_1_case_table(OrdTable ot);
static int	register_table(OrdTable ot);
static int	get_order_table(term_t handle, OrdTable *t);

		 /*******************************
		 *       PROLOG CONSTANTS	*
		 *******************************/

static atom_t ATOM_lt;
static atom_t ATOM_eq;
static atom_t ATOM_gt;
static atom_t ATOM_ignore;
static atom_t ATOM_tag;
static atom_t ATOM_break;
static atom_t ATOM_case_insensitive;
static atom_t ATOM_iso_latin_1;
static atom_t ATOM_iso_latin_1_case_insensitive;
static atom_t ATOM_exact;
static atom_t ATOM_copy;

static void
standard_table(atom_t name, void (*func)(OrdTable))
{ OrdTable t = malloc(sizeof(ordtable));
  
  if ( t )
  { exact_table(t);
    t->name = name;
    if ( func )
      (*func)(t);
    register_table(t);
  } else
    PL_warning("Could not allocate table");
}


static void
init_constants()
{ ATOM_lt			= PL_new_atom("<");
  ATOM_eq			= PL_new_atom("=");
  ATOM_gt			= PL_new_atom(">");
  ATOM_ignore			= PL_new_atom("ignore");
  ATOM_tag			= PL_new_atom("tag");
  ATOM_break			= PL_new_atom("break");
  ATOM_copy			= PL_new_atom("copy");
  ATOM_iso_latin_1		= PL_new_atom("iso_latin_1");
  ATOM_iso_latin_1_case_insensitive
				= PL_new_atom("iso_latin_1_case_insensitive");
  ATOM_break			= PL_new_atom("break");
  ATOM_case_insensitive		= PL_new_atom("case_insensitive");
  ATOM_exact			= PL_new_atom("exact");

  standard_table(ATOM_exact, 			    NULL);
  standard_table(ATOM_case_insensitive, 	    case_insensitive_table);
  standard_table(ATOM_iso_latin_1, 		    iso_latin_1_table);
  standard_table(ATOM_iso_latin_1_case_insensitive, iso_latin_1_case_table);
}


		 /*******************************
		 *         REGISTRATION		*
		 *******************************/

#define MAXTABLES 100

static OrdTable tables[MAXTABLES];

static int
register_table(OrdTable ot)
{ int i;
  int slot = -1;

  for(i=0; i<MAXTABLES; i++)
  { if ( tables[i] && tables[i]->name == ot->name )
    { free(tables[i]);
      tables[i] = ot;
      return TRUE;
    } else if ( slot < 0 && !tables[i] )
      slot = i;
  }

  if ( slot >= 0 )
  { tables[slot] = ot;
    return TRUE;
  }

  return FALSE;
}


OrdTable
findOrdTable(atom_t name)
{ int i;

  for(i=0; i<MAXTABLES; i++)
  { if ( tables[i] && tables[i]->name == name )
      return tables[i];
  }

  return NULL;
}


		 /*******************************
		 *          ORDER TABLE		*
		 *******************************/

static int
get_char(term_t t, int *chr)
{ int i;

  if ( PL_get_integer(t, &i) && i >= 0 && i <= 255 )
  { *chr = i;
    return TRUE;
  }

  return FALSE;
}


static int
parse_set(OrdTable ot, atom_t name, term_t set)
{ term_t c = PL_new_term_ref();
  int type;

  if ( name == ATOM_break )
    type = ORD_BREAK;
  else if ( name == ATOM_ignore )
    type = ORD_IGNORE;
  else if ( name == ATOM_tag )
    type = ORD_TAG;
  else
    return FALSE;

  while(PL_get_list(set, c, set))
  { int i;

    if ( !get_char(c, &i) )
      return FALSE;

    ORD(ot, i) = type;
  }

  return PL_get_nil(set);
}


static void
copy_table(OrdTable to, OrdTable from)
{ int i;

  to->magic = ORD_MAGIC;

  for(i=0; i<256; i++)
    ORD(to, i) = ORD(from, i);
}


static void
exact_table(OrdTable ot)
{ int i;

  ot->magic = ORD_MAGIC;

  for(i=0; i<256; i++)
    ORD(ot, i) = i;
}


static void
case_insensitive_table(OrdTable ot)
{ int i;

  ot->magic = ORD_MAGIC;

  for(i='A'; i<='Z'; i++)
    ORD(ot, i) = i + ('a' - 'A');
}


static void
iso_latin_1_table(OrdTable ot)
{ int i;

  exact_table(ot);

  ORD(ot, 192) = 'A';
  ORD(ot, 193) = 'A';
  ORD(ot, 194) = 'A';
  ORD(ot, 195) = 'A';
  ORD(ot, 196) = 'A';
  ORD(ot, 197) = 'A';

  ORD(ot, 199) = 'C';

  ORD(ot, 200) = 'E';
  ORD(ot, 201) = 'E';
  ORD(ot, 202) = 'E';
  ORD(ot, 203) = 'E';

  ORD(ot, 204) = 'I';
  ORD(ot, 205) = 'I';
  ORD(ot, 206) = 'I';
  ORD(ot, 207) = 'I';

  ORD(ot, 208) = 'D';
  ORD(ot, 209) = 'N';

  ORD(ot, 210) = 'O';
  ORD(ot, 211) = 'O';
  ORD(ot, 212) = 'O';
  ORD(ot, 213) = 'O';
  ORD(ot, 214) = 'O';
  ORD(ot, 216) = 'O';

  ORD(ot, 217) = 'U';
  ORD(ot, 218) = 'U';
  ORD(ot, 219) = 'U';
  ORD(ot, 220) = 'U';

  ORD(ot, 221) = 'Y';

  ORD(ot, 223) = 'S';			/* german SS */

  for(i=224; i<=253; i++)
  { if ( i == 230 ||			/* ae */
	 i == 247 )			/* x and / */
      continue;

    ORD(ot, i) = ORD(ot, i-32) + 'a' - 'A';
  }
}


static void
iso_latin_1_case_table(OrdTable ot)
{ int i;

  iso_latin_1_table(ot);

  for(i=0; i<=255; i++)
  { int o = ORD(ot, i);

    if ( o >= 'A' && o <= 'Z' )
      ORD(ot, i) = o + ('a' - 'A');
  }
}


static foreign_t
pl_new_order_table(term_t name, term_t options)
{ OrdTable t = malloc(sizeof(ordtable));
  term_t tail = PL_copy_term_ref(options);
  term_t head = PL_new_term_ref();

  exact_table(t);

  if ( !PL_get_atom(name, &t->name) )
    return error(ERR_INSTANTIATION, "new_order_table/2", 1, name);

  while(PL_get_list(tail, head, tail))
  { atom_t name;
    int arity;

    if ( PL_get_name_arity(head, &name, &arity) )
    { if ( name == ATOM_case_insensitive )
      { case_insensitive_table(t);
      } else if ( name == ATOM_iso_latin_1 )
      { iso_latin_1_table(t);
      } else if ( name == ATOM_iso_latin_1_case_insensitive )
      { iso_latin_1_case_table(t);
      } else if ( name == ATOM_copy && arity == 1 )
      { term_t a = PL_new_term_ref();
	OrdTable from;

	PL_get_arg(1, head, a);
	if ( get_order_table(a, &from) )
	{ copy_table(t, from);
	} else
	  return FALSE;
      } else if ( arity == 1 )
      { fid_t fid = PL_open_foreign_frame();
	term_t a = PL_new_term_ref();

	PL_get_arg(1, head, a);
	if ( !parse_set(t, name, a) )
	  goto err1;

	PL_close_foreign_frame(fid);
      } else if ( name == ATOM_eq && arity == 2 )
      { fid_t fid = PL_open_foreign_frame();
	term_t c = PL_new_term_ref();
	int from, to;

	if ( !PL_get_arg(1, head, c) || !get_char(c, &from) ||
	     !PL_get_arg(2, head, c) || !get_char(c, &to) )
	  return FALSE;

	ORD(t, from) = to;

	PL_close_foreign_frame(fid);
      } else
	goto err1;
    } else
    { err1:
      return error(ERR_INSTANTIATION, "new_order_table/2", 2, options);
    }
  }
  if ( !PL_get_nil(tail) )
    goto err1;

  register_table(t);
  
  PL_succeed;
}


static int
unify_mapped_code(term_t to, int ti)
{ switch(ti)
  { case ORD_BREAK:
      return PL_unify_atom(to, ATOM_break);
    case ORD_IGNORE:
      return PL_unify_atom(to, ATOM_ignore);
    case ORD_TAG:
      return PL_unify_atom(to, ATOM_tag);
    default:
      return PL_unify_integer(to, ti);
  }
}


static foreign_t
pl_order_table_mapping(term_t handle, term_t from, term_t to, control_t ctrl)
{ OrdTable t;
  int f;

  if ( !get_order_table(handle, &t) )
    return FALSE;

  if ( PL_get_integer(from, &f) && f >= 0 && f <= 255 )
    return unify_mapped_code(to, ORD(t, f));

  if ( PL_is_variable(from) )
  { switch(PL_foreign_control(ctrl))
    { case PL_FIRST_CALL:
	f = 0;
        break;
      case PL_REDO:
	f = PL_foreign_context(ctrl);
        break;
      case PL_CUTTED:
	return TRUE;
    }
    while( f <= 255 && !unify_mapped_code(to, ORD(t, f)) )
      f++;
    if ( f <= 255 )
    { PL_unify_integer(from, f);
      PL_retry(f+1);
    }
    return FALSE;
  }

  return FALSE;
}


		 /*******************************
		 *        DEFAULT TABLES	*
		 *******************************/

static int
get_order_table(term_t handle, OrdTable *t)
{ atom_t name;
  OrdTable ot;

  if ( PL_get_atom(handle, &name) &&
       (ot = findOrdTable(name)) )
  { *t = ot;
    return TRUE;
  }

  return FALSE;
}


static int
compare_strings_(const char *s1, const char **S2, int n, OrdTable ot)
{ const char *e1 = s1 + n;
  const char *s2 = *S2;

  for(;;)
  { int o1, o2;
    
    if ( s1 == e1 )
    { *S2 = s2;
      return 0;				/* equal */
    }

    o1 = ORD(ot, *s1);
    o2 = ORD(ot, *s2);

    if ( o1 == o2 )
    { if ( o1 == ORD_END )
      { *S2 = s2;
	return 0;			/* equal */
      }

      if ( o1 == ORD_BREAK )		/* a break, loop on both */
      { while(ORD(ot, *s1) == ORD_BREAK)
	  s1++;
	while(ORD(ot, *s2) == ORD_BREAK)
	  s2++;

	continue;
      }

      s1++;
      s2++;
      continue;
    }

					/* ignore stuff */
    if ( o1 == ORD_IGNORE )
    { s1++;
      continue;
    }
    if ( o2 == ORD_IGNORE )
    { s2++;
      continue;
    }

    return o1 < o2 ? -1 : 1;
  }
}


int
compare_strings(const char *s1, const char *s2, int n, OrdTable ot)
{ return compare_strings_(s1, &s2, n, ot);
}


static foreign_t
pl_compare_strings(term_t ord, term_t t1, term_t t2, term_t result)
{ OrdTable ot;
  char *s1, *s2;
  int rval;

  if ( !get_order_table(ord, &ot) )
    return error(ERR_INSTANTIATION, "compare_strings/4", 1, ord);
  if ( !PL_get_chars(t1, &s1, CVT_ATOM|CVT_STRING|CVT_LIST|BUF_RING) )
    return error(ERR_INSTANTIATION, "compare_strings/4", 2, t1);
  if ( !PL_get_chars(t2, &s2, CVT_ATOM|CVT_STRING|CVT_LIST|BUF_RING) )
    return error(ERR_INSTANTIATION, "compare_strings/4", 3, t2);

  rval = compare_strings(s1, s2, -1, ot);

  return PL_unify_atom(result,
		       rval == 0 ? ATOM_eq :
		       rval <  0 ? ATOM_lt :
		       	           ATOM_gt);
}


static foreign_t
pl_prefix_string(term_t ord, term_t pre, term_t t2)
{ OrdTable ot;
  char *s1, *s2;
  unsigned int l1;

  if ( !get_order_table(ord, &ot) )
    return error(ERR_INSTANTIATION, "prefix_string/3", 1, ord);
  if ( !PL_get_chars(pre, &s1, CVT_ATOM|CVT_STRING|CVT_LIST|BUF_RING) )
    return error(ERR_INSTANTIATION, "prefix_string/3", 2, pre);
  if ( !PL_get_chars(t2, &s2, CVT_ATOM|CVT_STRING|CVT_LIST|BUF_RING) )
    return error(ERR_INSTANTIATION, "prefix_string/3", 3, t2);

  if ( (l1=strlen(s1)) <= strlen(s2) &&
       compare_strings(s1, s2, l1, ot) == 0 )
    return TRUE;

  return FALSE;
}


static foreign_t
pl_prefix_string4(term_t ord, term_t pre, term_t post, term_t t2)
{ OrdTable ot;
  char *s1;
  const char *s2;
  unsigned int l1;

  if ( !get_order_table(ord, &ot) )
    return error(ERR_INSTANTIATION, "prefix_string/4", 1, ord);
  if ( !PL_get_chars(pre, &s1, CVT_ATOM|CVT_STRING|CVT_LIST|BUF_RING) )
    return error(ERR_INSTANTIATION, "prefix_string/4", 2, pre);
  if ( !PL_get_chars(t2, (char **)&s2, CVT_ATOM|CVT_STRING|CVT_LIST|BUF_RING) )
    return error(ERR_INSTANTIATION, "prefix_string/4", 4, t2);

  if ( (l1=strlen(s1)) <= strlen(s2) &&
       compare_strings_(s1, &s2, l1, ot) == 0 )
    return PL_unify_atom_chars(post, s2);

  return FALSE;
}


static foreign_t
pl_sub_string(term_t ord, term_t pre, term_t t2)
{ OrdTable ot;
  char *s1, *s2;
  unsigned int l1, l2;
  unsigned int offset = 0;

  if ( !get_order_table(ord, &ot) )
    return error(ERR_INSTANTIATION, "sub_string/3", 1, ord);
  if ( !PL_get_chars(pre, &s1, CVT_ATOM|CVT_STRING|CVT_LIST|BUF_RING) )
    return error(ERR_INSTANTIATION, "sub_string/3", 2, pre);
  if ( !PL_get_chars(t2, &s2, CVT_ATOM|CVT_STRING|CVT_LIST|BUF_RING) )
    return error(ERR_INSTANTIATION, "sub_string/3", 3, t2);

  l1 = strlen(s1);
  l2 = strlen(s2);

  for( ; l1+offset <= l2; offset++ )
  { if ( compare_strings(s1, s2+offset, l1, ot) == 0 )
      return TRUE;
  }

  return FALSE;
}


install_t
install_order()
{ init_constants();

  PL_register_foreign("new_order_table",	2, pl_new_order_table, 0);
  PL_register_foreign("order_table_mapping",	3, pl_order_table_mapping,
		      PL_FA_NONDETERMINISTIC);
  PL_register_foreign("compare_strings",	4, pl_compare_strings, 0);
  PL_register_foreign("prefix_string",		3, pl_prefix_string, 0);
  PL_register_foreign("prefix_string",		4, pl_prefix_string4, 0);
  PL_register_foreign("sub_string",		3, pl_sub_string, 0);
}
