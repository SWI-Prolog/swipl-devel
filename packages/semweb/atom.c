/*  $Id$

    Part of the SWI-Prolog Semweb package

    Author:        Jan Wielemaker
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2006, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <SWI-Stream.h>
#include <SWI-Prolog.h>
#include "atom.h"
#include "unicode_map.c"
#include <wchar.h>
#include <wctype.h>
#include <assert.h>
#ifdef __WINDOWS__
#define inline __inline
#endif


		 /*******************************
		 *	   TEXT HANDLING	*
		 *******************************/

typedef unsigned char charA;
typedef wchar_t       charW;

typedef struct text
{ const charA *a;
  const charW *w;
  size_t length;
} text;


static int
get_atom_text(atom_t atom, text *txt)
{ if ( (txt->a = (const charA*)PL_atom_nchars(atom, &txt->length)) )
  { txt->w = NULL;
    return TRUE;
  }
  if ( (txt->w = (const charW*)PL_atom_wchars(atom, &txt->length)) )
  { txt->a = NULL;
    return TRUE;
  }

  return FALSE;
}


inline wint_t
fetch(const text *txt, int i)
{ return txt->a ? (wint_t)txt->a[i] : (wint_t)txt->w[i];
}


		 /*******************************
		 *	      COMPARE		*
		 *******************************/

static inline int
cmpA(int c1, int c2, int *dl2)
{ if ( c1 == c2 )
  { return 0;
  } else
  { int k1 = sort_pointA(c1);
    int k2 = sort_pointA(c2);
    int d;
    
    if ( (d=((k1>>8)-(k2>>8))) == 0 )
    { if ( *dl2 == 0 )
	*dl2 = (k1&0xff) - (k2&0xff);
    }
    
    return d;
  }
}


static inline int
cmpW(int c1, int c2, int *dl2)
{ if ( c1 == c2 )
  { return 0;
  } else
  { int k1 = sort_point(c1);
    int k2 = sort_point(c2);
    int d;

    if ( (d=((k1>>8)-(k2>>8))) == 0 )
    { if ( *dl2 == 0 )
	*dl2 = (k1&0xff) - (k2&0xff);
    }
    
    return d;
  }
}


int
cmp_atoms(atom_t a1, atom_t a2)
{ text t1, t2;
  int i;
  int dl2 = 0;
  size_t n;
  
  if ( a1 == a2 )
    return 0;

  if ( !get_atom_text(a1, &t1) ||
       !get_atom_text(a2, &t2) )
  { goto cmphandles;			/* non-text atoms? */
  }

  if ( t1.a && t2.a )
  { const charA *s1 = t1.a;
    const charA *s2 = t2.a;
    int d;

    while((d=cmpA(*s1, *s2, &dl2)) == 0)
    { if ( *s1 == 0 )
	goto eq;
      s1++, s2++;
    }
    return d;
  }

  n = (t1.length < t2.length ? t1.length : t2.length);

  if ( t1.w && t2.w )
  { const charW *s1 = t1.w;
    const charW *s2 = t2.w;

    for(;;s1++, s2++)
    { if ( n-- == 0 )
      { if ( t1.length == t2.length )
	  goto eq;
	
	return t1.length < t2.length ? -1 : 1;
      } else
      { int d;

	if ( (d=cmpW(*s1, *s2, &dl2)) != 0 )
	  return d;
      }
    }
  }
  
  for(i=0; ; i++)
  { if ( n-- == 0 )
    { if ( t1.length == t2.length )
	  goto eq;
	
      return t1.length < t2.length ? -1 : 1;
    } else
    { wint_t c1 = fetch(&t1, i);
      wint_t c2 = fetch(&t2, i);
      int d;

      if ( (d=cmpW(c1, c2, &dl2)) != 0 )
	return d;
    }
  }

eq:
  if ( dl2 )
    return dl2;

cmphandles:
  return a1 < a2 ? -1 : 1;		/* == already covered */
}


		 /*******************************
		 *	       HASH		*
		 *******************************/

static unsigned int
string_hashA(const char *s, size_t len)
{ const unsigned char *t = (const unsigned char *)s;
  unsigned int value = 0;
  unsigned int shift = 5;

  while(len-- != 0)
  { unsigned int c = *t++;
    
    c = sort_pointA(c)>>8;		/* case insensitive */
    c -= 'a';
    value ^= c << (shift & 0xf);
    shift ^= c;
  }

  return value ^ (value >> 16);
}


static unsigned int
string_hashW(const wchar_t *t, size_t len)
{ unsigned int value = 0;
  unsigned int shift = 5;

  while(len-- != 0)
  { wint_t c = *t++;
    
    c = sort_point(c)>>8;		/* case insensitive */
    c -= 'a';
    value ^= c << (shift & 0xf);
    shift ^= c;
  }

  return value ^ (value >> 16);
}


unsigned int
atom_hash_case(atom_t a)
{ const char *s;
  const wchar_t *w;
  size_t len;

  if ( (s = PL_atom_nchars(a, &len)) )
    return string_hashA(s, len);
  else if ( (w = PL_atom_wchars(a, &len)) )
    return string_hashW(w, len);
  else
  { assert(0);
    return 0;
  }
}


		 /*******************************
		 *	    FIND FIRST		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Given an atom, return a new  one   that  has all its characters modified
such that it appears first in the   set  of atoms considered equal after
case canonisation and diacritics removal. This   is  required for prefix
search to find the first atom of the set.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

atom_t
first_atom(atom_t a, int match)
{ text t;

  if ( !get_atom_text(a, &t) )
  { return (atom_t)0;			/* not a textual atom */
  } else
  { size_t len = t.length;
    wchar_t buf[256];
    wchar_t *out, *s;
    int i;
    wint_t c;
    atom_t rc;
    
    if ( len <= 256 )
      out = buf;
    else
      out = PL_malloc(len*sizeof(wchar_t));
    
    for(s=out,i=0; (c=fetch(&t,i)); s++,i++)
    { if ( c == '*' && match == STR_MATCH_LIKE )
      { if ( i == 0 )			/* like '*...' */
	  return (atom_t)0;
	len = i;			/* only up to the first * */
      }
      *s = sort_point(c)>>8;
    }

    rc = PL_new_atom_wchars(len, out);

    if ( out != buf )
      PL_free(out);

    return rc;
  }
}

		 /*******************************
		 *	       MATCH		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
With the introduction of wide characters there   are two versions of the
match() function, one using char* and one using a structure and index to
fetch characters. Overall performance of  the   first  function is about
twice as good as the general one  and   as  most data will be handled by
this function in  practice  I  think  it   is  worthwhile  to  have  two
implementations. Both implementations are  very   similar  in design and
likely to have the same bugs. If  you   find  one, please fix it in both
branches!
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static const charA *
nextwordA(const charA *s)
{ while(*s && iswalnum(*s))
    s++;
  while(*s && !iswalnum(*s))
    s++;

  return s;
}


#define cmp_pointA(i) (sort_pointA(i)>>8)


static int
matchA(int how, const charA *f, const charA *l)
{ switch(how)
  { case STR_MATCH_EXACT:
    { for( ; *l && *f; l++, f++ )
      { if ( cmp_pointA(*l) != cmp_pointA(*f) )
	  return FALSE;
      }
      if ( *l == '\0' && *f == '\0' )
	return TRUE;
  
      return FALSE;
    }
    case STR_MATCH_PREFIX:
    { for( ; *l && *f; l++, f++ )
      { if ( cmp_pointA(*l) != cmp_pointA(*f) )
	  return FALSE;
      }
      if ( *f == '\0' )
	return TRUE;
  
      return FALSE;
    }
    case STR_MATCH_SUBSTRING:		/* use Boyle-More! */
    { const charA *h;
      const charA *f0 = f;
  
      for(h=l; *h; h++)
      { for( l=h,f=f0; *l && *f; l++, f++ )
	{ if ( cmp_pointA(*l) != cmp_pointA(*f) )
	    break;
	}
	if ( *f == '\0' )
	  return TRUE;
	if ( *h == '\0' )
	  return FALSE;
      }
  
      return FALSE;
    }
    case STR_MATCH_WORD:
    { const charA *h;
      const charA *f0 = f;
  
      for(h=l; *h; h = nextwordA(h))
      { for( l=h,f=f0; *l && *f; l++, f++ )
	{ if ( cmp_pointA(*l) != cmp_pointA(*f) )
	    break;
	}
	if ( *f == '\0' )
	{ if ( *l == '\0' || !iswalnum(*l) )
	    return TRUE;
	}
	if ( *l == '\0' )
	  return FALSE;
      }
  
      return FALSE;
    }
    case STR_MATCH_LIKE:		/* SeRQL like: * --> wildcart */
    { typedef struct chp { const charA *pattern;
			   const charA *label; } chp;
      chp chps[MAX_LIKE_CHOICES];
      int chn=0;

      for( ; *l && *f; l++, f++ )
      { if ( *f == '*' )
	{ f++;

	  if ( *f == '\0' )		/* foo* */
	    return TRUE;

	search_like:
	  while ( *l && cmp_pointA(*l) != cmp_pointA(*f) )
	    l++;

	  if ( *l )
	  { if ( chn >= MAX_LIKE_CHOICES )
	    { Sdprintf("rdf_db: too many * in `like' expression (>%d)",
		       MAX_LIKE_CHOICES);
	      return FALSE;
	    }
	    chps[chn].pattern = f;
	    chps[chn].label   = l+1;
	    chn++;

	    continue;
	  } else
	    goto retry_like;
	}

	if ( cmp_pointA(*l) != cmp_pointA(*f) )
	  goto retry_like;
      }
      if ( *l == '\0' && (*f == '\0' ||
			 (*f == '*' && f[1] == '\0')) )
	return TRUE;
  
retry_like:
      if ( chn > 0 )
      { chn--;
	f = chps[chn].pattern;
	l = chps[chn].label;
	goto search_like;
      }

      return FALSE;
    }
    default:
      assert(0);
      return FALSE;
  }
}


static unsigned int
nextword(text *txt, unsigned int i)
{ while(i<txt->length && iswalnum(fetch(txt, i)))
    i++;
  while(i<txt->length && !iswalnum(fetch(txt, i)))
    i++;

  return i;
}


#define cmp_point(i) (sort_point(i)>>8)


int
match_atoms(int how, atom_t search, atom_t label)
{ text l, f;

  if ( !get_atom_text(label, &l) ||
       !get_atom_text(search, &f) )
    return FALSE;			/* error? */
  
  if ( f.length == 0 )
    return TRUE;

  if ( f.a && l.a )
    return matchA(how, f.a, l.a);

  switch(how)
  { case STR_MATCH_EXACT:
    { if ( l.length == f.length )
      { unsigned int i;

	for(i=0; i<l.length; i++ )
	{ if ( cmp_point(fetch(&l, i)) != cmp_point(fetch(&f, i)) )
	    return FALSE;
	}

        return TRUE;
      }
  
      return FALSE;
    }
    case STR_MATCH_PREFIX:
    { if ( f.length <= l.length )
      { unsigned int i;

	for(i=0; i<f.length; i++ )
	{ if ( cmp_point(fetch(&l, i)) != cmp_point(fetch(&f, i)) )
	    return FALSE;
	}

	return TRUE;
      }  

      return FALSE;
    }
    case STR_MATCH_SUBSTRING:		/* use Boyle-More! */
    { if ( f.length <= l.length )
      { unsigned int i, s;

	for(s=0; s+f.length <= l.length; s++)
	{ for(i=0; i<f.length; i++)
	  { if ( cmp_point(fetch(&l, i+s)) != cmp_point(fetch(&f, i)) )
	      goto snext;
	  }
	  return TRUE;

	snext:;
	}
      }
  
      return FALSE;
    }
    case STR_MATCH_WORD:
    { if ( f.length <= l.length )
      { unsigned int i, s;

	for(s=0; s+f.length <= l.length; s = nextword(&l, s))
	{ for(i=0; i<f.length; i++)
	  { if ( cmp_point(fetch(&l, i+s)) != cmp_point(fetch(&f, i)) )
	      goto wnext;
	  }
	  if ( i+s == l.length || !iswalnum(fetch(&l,i+s)) )
	    return TRUE;

	wnext:;
	}
      }
  
      return FALSE;
    }
    case STR_MATCH_LIKE:		/* SeRQL like: * --> wildcart */
    { unsigned int ip, il;
      typedef struct chp { unsigned int ip;
			   unsigned int il;
			 } chp;
      chp chps[MAX_LIKE_CHOICES];
      int chn=0;

      for(ip=il=0; il < l.length && ip < f.length; ip++, il++ )
      { if ( fetch(&f, ip) == '*' )
	{ ip++;

	  if ( ip == f.length )		/* foo* */
	    return TRUE;

	search_like:
	  while ( il < l.length &&
		  cmp_point(fetch(&l, il)) != cmp_point(fetch(&f, ip)) )
	    il++;

	  if ( il < l.length )
	  { if ( chn >= MAX_LIKE_CHOICES )
	    { Sdprintf("rdf_db: too many * in `like' expression (>%d)",
		       MAX_LIKE_CHOICES);
	      return FALSE;
	    }
	    chps[chn].ip = ip;
	    chps[chn].il = il+1;
	    chn++;

	    continue;
	  } else
	    goto retry_like;
	}

	if ( cmp_point(fetch(&l, il)) != cmp_point(fetch(&f, ip)) )
	  goto retry_like;
      }
      if ( il == l.length && (ip == f.length ||
			      (fetch(&f,ip) == '*' && ip+1 == f.length)) )
	return TRUE;
  
retry_like:
      if ( chn > 0 )
      { chn--;
	ip = chps[chn].ip;
	il = chps[chn].il;
	goto search_like;
      }

      return FALSE;
    }
    default:
      assert(0);
      return FALSE;
  }
}
