/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#include <h/kernel.h>
#include <h/str.h>

#undef min
#define min(a, b) ((a) < (b) ? (a) : (b))

#define sameEncoding(s1, s2) \
	if ( s1->encoding != s2->encoding ) \
	  return FALSE;


		 /*******************************
		 *	       ALLOC		*
		 *******************************/

inline int
str_allocsize(String s)
{ if ( isstr8(s) )
    return (((s->size + sizeof(long)) / sizeof(long)) * sizeof(long));
  else
    return (((s->size * 2 + sizeof(long) - 1) / sizeof(long)) * sizeof(long));
}


inline void
str_pad(String s)			/* only 8-bit strings */
{ if ( isstr8(s) )
  { int from = s->size;
    int len  = str_allocsize(s);
    
    while(from < len)
      s->s_text8[from++] = '\0';
  }
}


void
str_alloc(String s)
{ s->s_text8  = alloc(str_allocsize(s));
  s->readonly = FALSE;
  str_pad(s);
}


void
str_unalloc(String s)
{ if ( s->s_text8 && !s->readonly )
  { unalloc(str_allocsize(s), s->s_text8);
    s->s_text8 = NULL;
  }
}


String
str_init(String s, String proto, char8 *data)
{ str_cphdr(s, proto);
  s->s_text = data;

  return s;
}


void
str_set_ascii(String str, char *text)
{ str_inithdr(str, ENC_ASCII);

  str->size = strlen(text);
  str->s_text8 = (char8 *) text;
}


void
str_set_n_ascii(String str, int len, char *text)
{ str_inithdr(str, ENC_ASCII);
  str->size = len;
  str->s_text8 = (char8 *) text;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
str_set_static(): initialise a string from a static C-string
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

void
str_set_static(String str, const char *text)
{ str_inithdr(str, ENC_ASCII);
  str->readonly = TRUE;
  str->size = strlen(text);
  str->s_text8 = (char8 *) text;
}


		 /*******************************
		 *	     COPY STUFF		*
		 *******************************/

void
str_ncpy(String dest, int at, String src, int from, int len)
{ if ( isstr8(dest) )
    memcpy(&dest->s_text8[at], &src->s_text8[from], len * sizeof(char8));
  else
    memcpy(&dest->s_text16[at], &src->s_text16[from], len * sizeof(char16));
}


void
str_cpy(String dest, String src)
{ str_cphdr(dest, src);
  str_ncpy(dest, 0, src, 0, src->size);
}


char8 *
str_textp(String s, int i)
{ return isstr8(s) ? &s->s_text8[i] : (char8 *)&s->s_text16[i];
}


		 /*******************************
		 *	CASE MANIPULATION	*
		 *******************************/

void
str_upcase(String str, int from, int to)
{ if ( isstr8(str) )
  { char8 *s = &str->s_text8[from];

    for(; from < to; from++, s++)
      *s = toupper(*s);
  } else
  { char16 *s = &str->s_text16[from];

    for(; from < to; from++, s++)
      *s = toupper(*s);
  }
}


void
str_downcase(String str, int from, int to)
{ if ( isstr8(str) )
  { char8 *s = &str->s_text8[from];

    for(; from < to; from++, s++)
      *s = tolower(*s);
  } else
  { char16 *s = &str->s_text16[from];

    for(; from < to; from++, s++)
      *s = tolower(*s);
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

  sameEncoding(s1, s2);

  if ( isstr8(s1) )
  { char8 *d1 = s1->s_text8;
    char8 *d2 = s2->s_text8;
    int d;

    while(n-- > 0)
      if ( (d = (*d1++ - *d2++)) )
	return d;

    return s1->size - s2->size;
  } else
  { char16 *d1 = s1->s_text16;
    char16 *d2 = s2->s_text16;
    int d;

    while(n-- > 0)
      if ( (d = (*d1++ - *d2++)) )
	return d;

    return s1->size - s2->size;
  }
}


int
str_icase_cmp(String s1, String s2)
{ int n = min(s1->size, s2->size);

  sameEncoding(s1, s2);

  if ( isstr8(s1) )
  { char8 *d1 = s1->s_text8;
    char8 *d2 = s2->s_text8;
    int d;

    for(; n-- > 0; d1++, d2++)
      if ( (d = (tolower(*d1) - tolower(*d2))) )
	return d;

    return s1->size - s2->size;
  } else
  { char16 *d1 = s1->s_text16;
    char16 *d2 = s2->s_text16;
    int d;

    for(; n-- > 0; d1++, d2++)
      if ( (d = (tolower(*d1) - tolower(*d2))) )
	return d;

    return s1->size - s2->size;
  }
}


int
str_eq(String s1, String s2)
{ if ( s1->size == s2->size )
    return str_cmp(s1, s2) == 0 && s1->encoding == s2->encoding;
		     
  return FALSE;
}


int
str_icase_eq(String s1, String s2)
{ if ( s1->size == s2->size )
    return str_icase_cmp(s1, s2) == 0;
		     
  return FALSE;
}


int
str_prefix(String s1, String s2)	/* s2 is prefix of s1 */
{ sameEncoding(s1, s2);

  if ( s2->size <= s1->size )
  { int n = s2->size;

    if ( isstr8(s1) )
    { char8 *d1 = s1->s_text8;
      char8 *d2 = s2->s_text8;

      while(n-- > 0)
	if ( *d1++ != *d2++ )
	  return FALSE;

      return TRUE;
    } else
    { char16 *d1 = s1->s_text16;
      char16 *d2 = s2->s_text16;

      while(n-- > 0)
	if ( *d1++ != *d2++ )
	  return FALSE;
    }

    return TRUE;
  } 

  return FALSE;
}


int
str_icase_prefix(String s1, String s2)	/* s2 is prefix of s1 */
{ sameEncoding(s1, s2);

  if ( s2->size <= s1->size )
  { int n = s2->size;

    if ( isstr8(s1) )
    { char8 *d1 = s1->s_text8;
      char8 *d2 = s2->s_text8;

      for(; n-- > 0; d1++, d2++)
	if ( tolower(*d1) != tolower(*d2) )
	  return FALSE;

      return TRUE;
    } else
    { char16 *d1 = s1->s_text16;
      char16 *d2 = s2->s_text16;

      for(; n-- > 0; d1++, d2++)
	if ( tolower(*d1) != tolower(*d2) )
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

    if ( isstr8(s1) )
    { char8 *d1 = &s1->s_text8[offset];
      char8 *d2 = s2->s_text8;

      while(n-- > 0)
	if ( *d1++ != *d2++ )
	  return FALSE;

      return TRUE;
    } else
    { char16 *d1 = &s1->s_text16[offset];
      char16 *d2 = s2->s_text16;

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

    if ( isstr8(s1) )
    { char8 *d1 = &s1->s_text8[offset];
      char8 *d2 = s2->s_text8;

      for( ; n-- > 0; d1++, d2++)
      { if ( tolower(*d1) != tolower(*d2) )
	  return FALSE;
      }

      return TRUE;
    } else
    { char16 *d1 = &s1->s_text16[offset];
      char16 *d2 = s2->s_text16;

      for( ; n-- > 0; d1++, d2++)
      { if ( tolower(*d1) != tolower(*d2) )
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
    
    if ( isstr8(s1) )
    { for(; n <= m; n++)
      { char8 *d1 = &s1->s_text8[n];
	char8 *d2 = s2->s_text8;
	int i = s2->size;

	while( i-- > 0 )
	  if ( *d1++ != *d2++ )
	    goto next8;

	return TRUE;
      next8:;
      }
    } else
    { for(; n <= m; n++)
      { char16 *d1 = &s1->s_text16[n];
	char16 *d2 = s2->s_text16;
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
str_next_index(String s, int from, wchar chr)
{ int i, n = s->size;

  if ( isstr8(s) )
  { char8 *d = &s->s_text8[from];
    
    for(i=from; i<n; i++, d++)
      if ( *d == chr )
	return i;
  } else
  { char16 *d = &s->s_text16[from];
    
    for(i=from; i<n; i++, d++)
      if ( *d == chr )
	return i;
  }

  return -1;
}


int
str_next_rindex(String s, int from, wchar chr)
{ int i;

  if ( isstr8(s) )
  { char8 *d = &s->s_text8[from];
    
    for(i=from; i >= 0; i--, d--)
      if ( *d == chr )
	return i;
  } else
  { char16 *d = &s->s_text16[from];
    
    for(i=from; i >= 0; i--, d--)
      if ( *d == chr )
	return i;
  }

  return -1;
}


int
str_index(String s, wchar chr)
{ return str_next_index(s, 0, chr);
} 


int
str_rindex(String s, wchar chr)
{ return str_next_rindex(s, s->size, chr);
} 

/* count chr in [from,to) */

int
str_count_chr(String s, int from, int to, wchar chr)
{ int i, count = 0;

  if ( isstr8(s) )
  { char8 *d = &s->s_text8[from];
    
    for(i=from; i<to; i++, d++)
      if ( *d == chr )
	count++;
  } else
  { char16 *d = &s->s_text16[from];
    
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


int
str_fetch(String s, int idx)
{ return s->b16 ? str_fetch16(s, idx) & 0xffff
		: str_fetch8(s, idx) & 0xff;
}


int
str_store(String s, int idx, unsigned int chr)
{ return s->b16 ? str_store16(s, idx, chr)
		: str_store8(s, idx, chr);
}

		 /*******************************
		 *	       UTIL		*
		 *******************************/

static void
str_from_char(String s, char c)
{ unsigned char *text = alloc(sizeof(char)*2);
  text[0] = c;
  text[1] = '\0';

  str_inithdr(s, ENC_ASCII);
  s->s_text8  = text;
  s->size     = 1;
}


static void
str_from_char16(String s, int c)
{ char16 *text = alloc(sizeof(char16)*2);
  text[0] = c;
  text[1] = '\0';

  str_inithdr(s, ENC_UNICODE);
  s->s_text16 = text;
  s->size     = 1;
}


String
str_nl(String proto)
{ static string nl8;
  static string nl16;

  if ( !proto || !proto->b16 )
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

  if ( !proto || !proto->b16 )
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

  if ( !proto || !proto->b16 )
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

  if ( isstr8(s) )
  { char8 *f = s->s_text8;
    char8 *t = s->s_text8;
    char8 *e = &s->s_text8[size];
    
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

  if ( s1->encoding == s2->encoding )
  { if ( isstr8(s1) )
    { char8 *t1 = s1->s_text8;
      char8 *t2 = s2->s_text8;

      while( i < size && *t1++ == *t2++ )
	i++;
    } else
    { char16 *t1 = s1->s_text16;
      char16 *t2 = s2->s_text16;

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

  if ( s1->encoding == s2->encoding )
  { if ( isstr8(s1) )
    { char8 *t1 = s1->s_text8;
      char8 *t2 = s2->s_text8;

      while( i < size && tolower(*t1) == tolower(*t2) )
	i++, t1++, t2++;
    } else
    { char16 *t1 = s1->s_text16;
      char16 *t2 = s2->s_text16;

      while( i < size && *t1++ == *t2++ )
	i++;
    }
  }

  return i;
}
      
