/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

#include <h/kernel.h>
#include <h/str.h>

#undef min
#define min(a, b) ((a) < (b) ? (a) : (b))

#define sameEncoding(s1, s2) \
	if ( s1->iswide != s2->iswide ) \
	  return FALSE;


		 /*******************************
		 *	       ALLOC		*
		 *******************************/

inline int
str_allocsize(String s)
{ int len;

  len = isstrA(s) ? s->size : s->size*sizeof(charW);

  return ((len + sizeof(long))/sizeof(long))*sizeof(long);
}


inline void
str_pad(String s)			/* only 8-bit strings */
{ if ( isstrA(s) )
  { int from = s->size;
    int len  = str_allocsize(s);
    
    while(from < len)
      s->s_textA[from++] = '\0';
  } else
  { int from = s->size;
    int len  = str_allocsize(s)/sizeof(charW);
    
    while(from < len)
      s->s_textW[from++] = '\0';
  }
}


void
str_alloc(String s)
{ s->s_textA  = alloc(str_allocsize(s));
  s->readonly = FALSE;
  str_pad(s);
}


#define STR_RING_SIZE 16
static char *str_ring[STR_RING_SIZE] = {NULL};
int    str_ring_ptr = 0;

static void
str_ring_alloc(String s)
{ int size = str_allocsize(s);

  if ( !str_ring[str_ring_ptr] )
  { str_ring[str_ring_ptr] = pceMalloc(size);
  } else
  { str_ring[str_ring_ptr] = pceRealloc(str_ring[str_ring_ptr], size);
  }
  s->s_textA = str_ring[str_ring_ptr];
  s->readonly = TRUE;

  if ( ++str_ring_ptr == STR_RING_SIZE )
    str_ring_ptr = 0;
}


void
str_unalloc(String s)
{ if ( s->s_textA && !s->readonly )
  { unalloc(str_allocsize(s), s->s_textA);
    s->s_textA = NULL;
  }
}


String
str_init(String s, String proto, charA *data)
{ str_cphdr(s, proto);
  s->s_text = data;

  return s;
}


String
fstr_inithdr(String s, int iswide, void *data, int len)
{ str_inithdr(s, iswide);
  s->s_text = data;
  s->size = len;

  return s;
}


status
str_set_n_ascii(String str, int len, char *text)
{ if ( len > STR_MAX_SIZE )
    return errorPce(NIL, NAME_stringTooLong, toInt(len));

  str_inithdr(str, FALSE);
  str->size = len;
  str->s_textA = (charA *) text;

  succeed;
}


status
str_set_n_wchar(String str, int len, wchar_t *text)
{ if ( len > STR_MAX_SIZE )
    return errorPce(NIL, NAME_stringTooLong, toInt(len));

  str_inithdr(str, TRUE);
  str->size = len;
  str->s_textW = text;

  succeed;
}


status
str_set_ascii(String str, char *text)
{ int len = strlen(text);

  return str_set_n_ascii(str, len, text);
}


status
str_set_utf8(String str, const char *text)
{ const char *s = text;
  const char *e = &text[strlen(s)];
  int iswide = FALSE;
  int len = 0;

  while(s<e)
  { int chr;

    s = utf8_get_char(s, &chr);
    if ( chr > 0xff )
      iswide = TRUE;
    len++;
  }

  str_inithdr(str, iswide);
  str->size = len;
  str_ring_alloc(str);			/* NOTE: temporary space */

  for(len=0, s=text; s<e; len++)
  { int chr;

    s = utf8_get_char(s, &chr);
    str_store(str, len, chr);
  }

  succeed;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
str_set_static(): initialise a string from a static C-string
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

status
str_set_static(String str, const char *text)
{ int len = strlen(text);

  if ( len > STR_MAX_SIZE )
    return errorPce(NIL, NAME_stringTooLong, toInt(len));

  str_inithdr(str, FALSE);
  str->readonly = TRUE;
  str->size = len;
  str->s_textA = (charA *) text;

  succeed;
}


status
str_iswide(String s)
{ if ( s->iswide )
  { const charW *w = s->s_textW;
    const charW *e = &w[s->size];

    while(w<e)
    { if ( *w++ > 0xff )
	succeed;
    }
  }

  fail;
} 



		 /*******************************
		 *	     COPY STUFF		*
		 *******************************/

void
str_ncpy(String dest, int at, String src, int from, int len)
{ if ( dest->iswide == src->iswide )	/* same size */
  { if ( isstrA(dest) )
      memcpy(&dest->s_textA[at], &src->s_textA[from], len * sizeof(charA));
    else
      cpdata(&dest->s_textW[at], &src->s_textW[from], charW, len);
  } else if ( dest->iswide )		/* 8bit --> wide */
  { const charA *s = &src->s_textA[from];
    const charA *e = &s[len];
    charW *d = &dest->s_textW[at];

    while(s<e)
      *d++ = *s++;
  } else				/* wide --> 8bit (may truncate) */
  { const charW *s = &src->s_textW[from];
    const charW *e = &s[len];
    charA *d = &dest->s_textA[at];

    while(s<e)
      *d++ = *s++;
  }
}


void
str_cpy(String dest, String src)
{ str_cphdr(dest, src);
  str_ncpy(dest, 0, src, 0, src->size);
}


charA *
str_textp(String s, int i)
{ return isstrA(s) ? &s->s_textA[i] : (charA *)&s->s_textW[i];
}


		 /*******************************
		 *	CASE MANIPULATION	*
		 *******************************/

void
str_upcase(String str, int from, int to)
{ if ( isstrA(str) )
  { charA *s = &str->s_textA[from];

    for(; from < to; from++, s++)
      *s = toupper(*s);
  } else
  { charW *s = &str->s_textW[from];

    for(; from < to; from++, s++)
      *s = towupper(*s);
  }
}


void
str_downcase(String str, int from, int to)
{ if ( isstrA(str) )
  { charA *s = &str->s_textA[from];

    for(; from < to; from++, s++)
      *s = tolower(*s);
  } else
  { charW *s = &str->s_textW[from];

    for(; from < to; from++, s++)
      *s = towlower(*s);
  }
}


		 /*******************************
		 *	      COMPARE		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
int str_cmp(String s1, String s2)
    returns < 0 if s1 is before s2, == 0 if equal and > 0 if s2 is
    before s2.

int str_eq(String s1, String s2)
    returns != 0 if s1 equals s2.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

int
str_cmp(String s1, String s2)
{ int n = min(s1->size, s2->size);

  if ( s1->iswide == s2->iswide )
  { if ( isstrA(s1) )
    { charA *d1 = s1->s_textA;
      charA *d2 = s2->s_textA;
      int d;

      while(n-- > 0)
	if ( (d = (*d1++ - *d2++)) )
	  return d;

      return s1->size - s2->size;
    } else
    { charW *d1 = s1->s_textW;
      charW *d2 = s2->s_textW;
      int d;
  
      while(n-- > 0)
	if ( (d = (*d1++ - *d2++)) )
	  return d;
  
      return s1->size - s2->size;
    }
  } else				/* inconsistent encoding */
  { int i;

    for(i=0; i<n; i++)
    { wint_t c1 = str_fetch(s1, i);
      wint_t c2 = str_fetch(s2, i);

      if ( c1 != c2 )
	return c1 -c2;
    }

    return s1->size - s2->size;
  }
}


int
str_icase_cmp(String s1, String s2)
{ int n = min(s1->size, s2->size);

  if ( s1->iswide == s2->iswide )
  { if ( isstrA(s1) )
    { charA *d1 = s1->s_textA;
      charA *d2 = s2->s_textA;
      int d;
  
      for(; n-- > 0; d1++, d2++)
	if ( (d = (tolower(*d1) - tolower(*d2))) )
	  return d;
  
      return s1->size - s2->size;
    } else
    { charW *d1 = s1->s_textW;
      charW *d2 = s2->s_textW;
      int d;
  
      for(; n-- > 0; d1++, d2++)
	if ( (d = (towlower(*d1) - towlower(*d2))) )
	  return d;
  
      return s1->size - s2->size;
    }
  } else
  { int i;

    for(i=0; i<n; i++)
    { wint_t c1 = towlower(str_fetch(s1, i));
      wint_t c2 = towlower(str_fetch(s2, i));

      if ( c1 != c2 )
	return c1 -c2;
    }

    return s1->size - s2->size;
  }
}


int
str_eq(String s1, String s2)
{ if ( s1->size == s2->size )
    return str_cmp(s1, s2) == 0;
		     
  return FALSE;
}


int
str_icase_eq(String s1, String s2)
{ if ( s1->size == s2->size )
    return str_icase_cmp(s1, s2) == 0;
		     
  return FALSE;
}


int					/* s2 is prefix of s1+offset */
str_prefix_offset(String s1, unsigned int offset, String s2)
{ if ( s2->size <= s1->size-offset )
  { int n = s2->size;

    if ( isstrA(s1) && isstrA(s2) )
    { charA *d1 = s1->s_textA+offset;
      charA *d2 = s2->s_textA;

      while(n-- > 0)
	if ( *d1++ != *d2++ )
	  return FALSE;

      return TRUE;
    } else
    { int i;

      for(i=0; i<n; i++)
	if ( str_fetch(s1, i+offset) != str_fetch(s2, i) )
	  return FALSE;
    }

    return TRUE;
  } 

  return FALSE;
}


int
str_prefix(String s1, String s2)	/* s2 is prefix of s1 */
{ return str_prefix_offset(s1, 0, s2);
}


int
str_icase_prefix(String s1, String s2)	/* s2 is prefix of s1 */
{ sameEncoding(s1, s2);

  if ( s2->size <= s1->size )
  { int n = s2->size;

    if ( isstrA(s1) )
    { charA *d1 = s1->s_textA;
      charA *d2 = s2->s_textA;

      for(; n-- > 0; d1++, d2++)
	if ( tolower(*d1) != tolower(*d2) )
	  return FALSE;

      return TRUE;
    } else
    { charW *d1 = s1->s_textW;
      charW *d2 = s2->s_textW;

      for(; n-- > 0; d1++, d2++)
	if ( towlower(*d1) != towlower(*d2) )
	  return FALSE;
    }

    return TRUE;
  } 

  return FALSE;
}


int
str_suffix(String s1, String s2)	/* s2 is suffix of s1 */
{ sameEncoding(s1, s2);

  if ( s2->size <= s1->size )
  { int n = s2->size;
    int offset = s1->size - s2->size;

    if ( isstrA(s1) )
    { charA *d1 = &s1->s_textA[offset];
      charA *d2 = s2->s_textA;

      while(n-- > 0)
	if ( *d1++ != *d2++ )
	  return FALSE;

      return TRUE;
    } else
    { charW *d1 = &s1->s_textW[offset];
      charW *d2 = s2->s_textW;

      while(n-- > 0)
	if ( *d1++ != *d2++ )
	  return FALSE;
    }

    return TRUE;
  } 

  return FALSE;
}


int
str_icase_suffix(String s1, String s2)	/* s2 is suffix of s1 */
{ sameEncoding(s1, s2);

  if ( s2->size <= s1->size )
  { int n = s2->size;
    int offset = s1->size - s2->size;

    if ( isstrA(s1) )
    { charA *d1 = &s1->s_textA[offset];
      charA *d2 = s2->s_textA;

      for( ; n-- > 0; d1++, d2++)
      { if ( tolower(*d1) != tolower(*d2) )
	  return FALSE;
      }

      return TRUE;
    } else
    { charW *d1 = &s1->s_textW[offset];
      charW *d2 = s2->s_textW;

      for( ; n-- > 0; d1++, d2++)
      { if ( towlower(*d1) != towlower(*d2) )
	  return FALSE;
      }
    }

    return TRUE;
  } 

  return FALSE;
}


int
str_sub(String s1, String s2)		/* s2 is substring of s1 */
{ sameEncoding(s1, s2);

  if ( s2->size <= s1->size )
  { int n = 0;
    int m = s1->size - s2->size;
    
    if ( isstrA(s1) )
    { for(; n <= m; n++)
      { charA *d1 = &s1->s_textA[n];
	charA *d2 = s2->s_textA;
	int i = s2->size;

	while( i-- > 0 )
	  if ( *d1++ != *d2++ )
	    goto next8;

	return TRUE;
      next8:;
      }
    } else
    { for(; n <= m; n++)
      { charW *d1 = &s1->s_textW[n];
	charW *d2 = s2->s_textW;
	int i = s2->size;

	while( i-- > 0 )
	  if ( *d1++ != *d2++ )
	    goto next16;

	return TRUE;
      next16:;
      }
    } 
  }

  return FALSE;
}


int
str_icasesub(String s1, String s2)		/* s2 is substring of s1 */
{ sameEncoding(s1, s2);

  if ( s2->size <= s1->size )
  { int n = 0;
    int m = s1->size - s2->size;
    
    if ( isstrA(s1) )
    { for(; n <= m; n++)
      { charA *d1 = &s1->s_textA[n];
	charA *d2 = s2->s_textA;
	int i;

	for(i=s2->size; i-- > 0; d1++, d2++ )
	{ if ( tolower(*d1) != tolower(*d2) )
	    goto next8;
	}

	return TRUE;
      next8:;
      }
    } else
    { for(; n <= m; n++)
      { charW *d1 = &s1->s_textW[n];
	charW *d2 = s2->s_textW;
	int i;

	for(i=s2->size; i-- > 0; d1++, d2++ )
	{ if ( towlower(*d1) != towlower(*d2) )
	    goto next16;
	}

	return TRUE;
      next16:;
      }
    } 
  }

  return FALSE;
}


int
str_next_index(String s, int from, wint_t chr)
{ int i, n = s->size;

  if ( isstrA(s) )
  { charA *d = &s->s_textA[from];
    
    for(i=from; i<n; i++, d++)
      if ( *d == chr )
	return i;
  } else
  { charW *d = &s->s_textW[from];
    
    for(i=from; i<n; i++, d++)
      if ( *d == chr )
	return i;
  }

  return -1;
}


int
str_next_rindex(String s, int from, wint_t chr)
{ int i;

  if ( isstrA(s) )
  { charA *d = &s->s_textA[from];
    
    for(i=from; i >= 0; i--, d--)
      if ( *d == chr )
	return i;
  } else
  { charW *d = &s->s_textW[from];
    
    for(i=from; i >= 0; i--, d--)
      if ( *d == chr )
	return i;
  }

  return -1;
}


int
str_index(String s, wint_t chr)
{ return str_next_index(s, 0, chr);
} 


int
str_rindex(String s, wint_t chr)
{ return str_next_rindex(s, s->size, chr);
} 

/* count chr in [from,to) */

int
str_count_chr(String s, int from, int to, wint_t chr)
{ int i, count = 0;

  if ( isstrA(s) )
  { charA *d = &s->s_textA[from];
    
    for(i=from; i<to; i++, d++)
      if ( *d == chr )
	count++;
  } else
  { charW *d = &s->s_textW[from];
    
    for(i=from; i<to; i++, d++)
      if ( *d == chr )
	count++;
  }

  return count;
}


int
str_lineno(String s, int at)
{ return str_count_chr(s, 0, at, '\n') + 1;
}


wint_t
str_fetch(String s, int idx)
{ return s->iswide ? str_fetchW(s, idx)
		   : str_fetchA(s, idx) & 0xff;
}


int
str_store(String s, int idx, unsigned int chr)
{ return s->iswide ? str_storeW(s, idx, chr)
		   : str_storeA(s, idx, chr);
}

		 /*******************************
		 *	       UTIL		*
		 *******************************/

static void
str_from_char(String s, char c)
{ unsigned char *text = alloc(sizeof(char)*2);
  text[0] = c;
  text[1] = '\0';

  str_inithdr(s, FALSE);
  s->s_textA  = text;
  s->size     = 1;
}


static void
str_from_char16(String s, int c)
{ charW *text = alloc(sizeof(charW)*2);
  text[0] = c;
  text[1] = '\0';

  str_inithdr(s, FALSE);
  s->s_textW = text;
  s->size     = 1;
}


String
str_nl(String proto)
{ static string nl8;
  static string nl16;

  if ( !proto || !proto->iswide )
  { if ( !nl8.size )
      str_from_char(&nl8, '\n');

    return &nl8;
  } else
  { if ( !nl16.size )
      str_from_char16(&nl16, '\n');

    return &nl16;
  }
}


String
str_spc(String proto)
{ static string spc8;
  static string spc16;

  if ( !proto || !proto->iswide )
  { if ( !spc8.size )
      str_from_char(&spc8, ' ');

    return &spc8;
  } else
  { if ( !spc16.size )
      str_from_char16(&spc16, ' ');

    return &spc16;
  }
}


String
str_tab(String proto)
{ static string tab8;
  static string tab16;

  if ( !proto || !proto->iswide )
  { if ( !tab8.size )
      str_from_char(&tab8, '\t');

    return &tab8;
  } else
  { if ( !tab16.size )
      str_from_char16(&tab16, '\t');

    return &tab16;
  }
}


void
str_strip(String s)
{ int size = s->size;

  if ( isstrA(s) )
  { charA *f = s->s_textA;
    charA *t = s->s_textA;
    charA *e = &s->s_textA[size];
    
    while( f < e && isblank(*f) )
      f++;

    do
    { while( f < e && !isblank(*f) )
	*t++ = *f++;
      while( f < e && isblank(*f) )
	f++;
      if ( f < e )
	*t++ = ' ';
    } while( f < e );
  } else
    errorPce(CtoName("str_strip()"), NAME_notSupportedForChar16);
}


int
str_common_length(String s1, String s2)
{ int i = 0;
  int size = min(s1->size, s2->size);

  if ( s1->iswide == s2->iswide )
  { if ( isstrA(s1) )
    { charA *t1 = s1->s_textA;
      charA *t2 = s2->s_textA;

      while( i < size && *t1++ == *t2++ )
	i++;
    } else
    { charW *t1 = s1->s_textW;
      charW *t2 = s2->s_textW;

      while( i < size && *t1++ == *t2++ )
	i++;
    }
  }

  return i;
}
      

int
str_icase_common_length(String s1, String s2)
{ int i = 0;
  int size = min(s1->size, s2->size);

  if ( s1->iswide == s2->iswide )
  { if ( isstrA(s1) )
    { charA *t1 = s1->s_textA;
      charA *t2 = s2->s_textA;

      while( i < size && tolower(*t1) == tolower(*t2) )
	i++, t1++, t2++;
    } else
    { charW *t1 = s1->s_textW;
      charW *t2 = s2->s_textW;

      while( i < size && towlower(*t1) == towlower(*t2) )
	i++, t1++, t2++;
    }
  }

  return i;
}
      

		 /*******************************
		 *	 TEMPORARY STRINGS	*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Temporary strings are  designed  to   get  character  data  code-by-code
without knowing the size in advance or wether or not the data fits in an
ISO Latin-1 string or not.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

tmp_string *
str_tmp_init(tmp_string *tmp)
{ str_inithdr(&tmp->s, FALSE);
  tmp->s.s_textA = tmp->buffer;
  tmp->allocated = sizeof(tmp->buffer);

  return tmp;
}


wint_t
str_tmp_put(tmp_string *tmp, wint_t c)
{ String s = &tmp->s;

  if ( c > 0xff && !s->iswide )
  { if ( s->s_textA == tmp->buffer &&
	 s->size*sizeof(charW) < sizeof(tmp->buffer) )
    { charA b2[sizeof(tmp->buffer)];
      const charA *f = b2;
      const charA *e = &f[s->size];
      charW *t = s->s_textW;

      memcpy(b2, tmp->buffer, s->size);
      while(f<e)
	*t++ = *f++;
      tmp->allocated /= sizeof(charW);
    } else
    { charW *new = pceMalloc(tmp->allocated * sizeof(charW));
      const charA *f = tmp->buffer;
      const charA *e = &f[s->size];
      charW *t = new;
      
      while(f<e)
	*t++ = *f++;
      
      if ( s->s_textA != tmp->buffer )
	pceFree(s->s_textA);
      s->s_textW = new;
    }
    s->iswide = TRUE;
  }
  if ( s->size >= tmp->allocated )
  { if ( s->s_textA == tmp->buffer )
    { long len = tmp->allocated*2;

      if ( isstrA(s) )
      { s->s_textA = pceMalloc(len);
	
	memcpy(s->s_textA, tmp->buffer, sizeof(tmp->buffer));
      } else
      { s->s_textW = pceMalloc(len*sizeof(charW));

	memcpy(s->s_textA, tmp->buffer, sizeof(tmp->buffer));
      }
	   
      tmp->allocated = len;
    } else
    { tmp->allocated *= 2;

      if ( isstrA(s) )
	s->s_textA = pceRealloc(s->s_textA, tmp->allocated);
      else
	s->s_textW = pceRealloc(s->s_textW, tmp->allocated*sizeof(charW));
    }
  }

  if ( !s->iswide )
    s->s_textA[s->size++] = c;
  else
    s->s_textW[s->size++] = c;

  return c;
}


void
str_tmp_done(tmp_string *tmp)
{ if ( tmp->s.s_textA != tmp->buffer )
    pceFree(tmp->s.s_textA);
}
