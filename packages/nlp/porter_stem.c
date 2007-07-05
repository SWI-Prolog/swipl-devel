/* $Id$

   This is the Porter stemming algorithm, coded up in ANSI C by the
   author. It may be be regarded as canonical, in that it follows the
   algorithm presented in

   Porter, 1980, An algorithm for suffix stripping, Program, Vol. 14,
   no. 3, pp 130-137,

   only differing from it at the points maked --DEPARTURE-- below.

   See also http://www.muscat.com/~martin/stem.html

   The algorithm as described in the paper could be exactly replicated
   by adjusting the points of DEPARTURE, but this is barely necessary,
   because (a) the points of DEPARTURE are definitely improvements, and
   (b) no encoding of the Porter stemmer I have seen is anything like
   as exact as this version, even with the points of DEPARTURE!

   You can compile it on Unix with 'gcc -O3 -o stem stem.c' after which
   'stem' takes a list of inputs and sends the stemmed equivalent to
   stdout.

   The algorithm as encoded here is particularly fast.

   Release 1
*/

#include <SWI-Prolog.h>

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <wctype.h>

/* The main part of the stemming algorithm starts here. b is a buffer
   holding a word to be stemmed. The letters are in b[k0], b[k0+1] ...
   ending at b[k]. In fact k0 = 0 in this demo program. k is readjusted
   downwards as the stemming progresses. Zero termination is not in fact
   used in the algorithm.

   Note that only lower case sequences are stemmed. Forcing to lower case
   should be done before stem(...) is called.
*/

typedef struct vars
{ char *b;		/* work to be stemmed */
  int   k, k0, j;	/* j is a general offset into the string */
} vars;

/* cons(i) is TRUE <=> vs->b[i] is a consonant. */

static int
cons(int i, vars *vs)
{ switch (vs->b[i])
  { case 'a': case 'e': case 'i': case 'o': case 'u': return FALSE;
    case 'y': return (i==vs->k0) ? TRUE : !cons(i-1, vs);
    default: return TRUE;
  }
}

/* m() measures the number of consonant sequences between vs->k0 and vs->j. if c is
   a consonant sequence and v a vowel sequence, and <..> indicates arbitrary
   presence,

      <c><v>       gives 0
      <c>vc<v>     gives 1
      <c>vcvc<v>   gives 2
      <c>vcvcvc<v> gives 3
      ....
*/

static int
m(vars *vs)
{  int n = 0;
   int i = vs->k0;
   while(TRUE)
   {  if (i > vs->j) return n;
      if (! cons(i, vs)) break; i++;
   }
   i++;
   while(TRUE)
   {  while(TRUE)
      {  if (i > vs->j) return n;
            if (cons(i, vs)) break;
            i++;
      }
      i++;
      n++;
      while(TRUE)
      {  if (i > vs->j) return n;
         if (! cons(i, vs)) break;
         i++;
      }
      i++;
    }
}

/* vowelinstem() is TRUE <=> vs->k0,...vs->j contains a vowel */

static int
vowelinstem(vars *vs)
{  int i; for (i = vs->k0; i <= vs->j; i++) if (! cons(i, vs)) return TRUE;
   return FALSE;
}

/* doublec(vs->j) is TRUE <=> vs->j,(vs->j-1) contain a double consonant. */

static int
doublec(int j, vars *vs)
{  if (j < vs->k0+1) return FALSE;
   if (vs->b[j] != vs->b[j-1]) return FALSE;
   return cons(j, vs);
}

/* cvc(i) is TRUE <=> i-2,i-1,i has the form consonant - vowel - consonant
   and also if the second c is not w,x or y. this is used when trying to
   restore an e at the end of a short word. e.g.

      cav(e), lov(e), hop(e), crim(e), but
      snow, box, tray.

*/

static int
cvc(int i, vars *vs)
{  if (i < vs->k0+2 || !cons(i, vs) || cons(i-1, vs) || !cons(i-2, vs))
     return FALSE;
   {  int ch = vs->b[i];
      if (ch == 'w' || ch == 'x' || ch == 'y') return FALSE;
   }
   return TRUE;
}

/* ends(s) is TRUE <=> vs->k0,...vs->k ends with the string s. */

static int
ends(char * s, vars *vs)
{  int length = s[0];
   if (s[length] != vs->b[vs->k]) return FALSE; /* tiny speed-up */
   if (length > vs->k-vs->k0+1) return FALSE;
   if (memcmp(vs->b+vs->k-length+1,s+1,length) != 0) return FALSE;
   vs->j = vs->k-length;
   return TRUE;
}

/* setto(s) sets (vs->j+1),...vs->k to the characters in the string s, readjusting
   vs->k. */

static void
setto(char * s, vars *vs)
{  int length = s[0];
   memmove(vs->b+vs->j+1,s+1,length);
   vs->k = vs->j+length;
}

/* r(s) is used further down. */

static void
r(char * s, vars *vs)
{ if (m(vs) > 0)
    setto(s, vs);
}

/* step1ab() gets rid of plurals and -ed or -ing. e.g.

       caresses  ->  caress
       ponies    ->  poni
       ties      ->  ti
       caress    ->  caress
       cats      ->  cat

       feed      ->  feed
       agreed    ->  agree
       disabled  ->  disable

       matting   ->  mat
       mating    ->  mate
       meeting   ->  meet
       milling   ->  mill
       messing   ->  mess

       meetings  ->  meet

*/

static void 
step1ab(vars *vs)
{  if (vs->b[vs->k] == 's')
   {  if (ends("\04" "sses", vs)) vs->k -= 2; else
      if (ends("\03" "ies", vs)) setto("\01" "i", vs); else
      if (vs->b[vs->k-1] != 's') vs->k--;
   }
   if (ends("\03" "eed", vs)) { if (m(vs) > 0) vs->k--; } else
   if ((ends("\02" "ed", vs) || ends("\03" "ing", vs)) && vowelinstem(vs))
   {  vs->k = vs->j;
      if (ends("\02" "at", vs)) setto("\03" "ate", vs); else
      if (ends("\02" "bl", vs)) setto("\03" "ble", vs); else
      if (ends("\02" "iz", vs)) setto("\03" "ize", vs); else
      if (doublec(vs->k, vs))
      {  vs->k--;
         {  int ch = vs->b[vs->k];
            if (ch == 'l' || ch == 's' || ch == 'z') vs->k++;
         }
      }
      else if (m(vs) == 1 && cvc(vs->k, vs)) setto("\01" "e", vs);
  }
}

/* step1c() turns terminal y to i when there is another vowel in the stem. */

static void 
step1c(vars *vs)
{ if (ends("\01" "y", vs) && vowelinstem(vs))
    vs->b[vs->k] = 'i';
}


/* step2() maps double suffices to single ones. so -ization ( = -ize plus
   -ation) maps to -ize etc. note that the string before the suffix must give
   m() > 0. */

static void step2(vars *vs)
{ switch (vs->b[vs->k-1])
  {
    case 'a': if (ends("\07" "ational", vs)) { r("\03" "ate", vs); break; }
              if (ends("\06" "tional", vs)) { r("\04" "tion", vs); break; }
              break;
    case 'c': if (ends("\04" "enci", vs)) { r("\04" "ence", vs); break; }
              if (ends("\04" "anci", vs)) { r("\04" "ance", vs); break; }
              break;
    case 'e': if (ends("\04" "izer", vs)) { r("\03" "ize", vs); break; }
              break;
    case 'l': if (ends("\03" "bli", vs)) { r("\03" "ble", vs); break; } /*-DEPARTURE-*/

 /* To match the published algorithm, replace this line with
    case 'l': if (ends("\04" "abli", vs)) { r("\04" "able", vs); break; } */

              if (ends("\04" "alli", vs)) { r("\02" "al", vs); break; }
              if (ends("\05" "entli", vs)) { r("\03" "ent", vs); break; }
              if (ends("\03" "eli", vs)) { r("\01" "e", vs); break; }
              if (ends("\05" "ousli", vs)) { r("\03" "ous", vs); break; }
              break;
    case 'o': if (ends("\07" "ization", vs)) { r("\03" "ize", vs); break; }
              if (ends("\05" "ation", vs)) { r("\03" "ate", vs); break; }
              if (ends("\04" "ator", vs)) { r("\03" "ate", vs); break; }
              break;
    case 's': if (ends("\05" "alism", vs)) { r("\02" "al", vs); break; }
              if (ends("\07" "iveness", vs)) { r("\03" "ive", vs); break; }
              if (ends("\07" "fulness", vs)) { r("\03" "ful", vs); break; }
              if (ends("\07" "ousness", vs)) { r("\03" "ous", vs); break; }
              break;
    case 't': if (ends("\05" "aliti", vs)) { r("\02" "al", vs); break; }
              if (ends("\05" "iviti", vs)) { r("\03" "ive", vs); break; }
              if (ends("\06" "biliti", vs)) { r("\03" "ble", vs); break; }
              break;
    case 'g': if (ends("\04" "logi", vs)) { r("\03" "log", vs); break; } /*-DEPARTURE-*/

 /* To match the published algorithm, delete this line */
  }
}

/* step3() deals with -ic-, -full, -ness etc. similar strategy to step2. */

static void
step3(vars *vs)
{ switch (vs->b[vs->k])
  {
    case 'e': if (ends("\05" "icate", vs)) { r("\02" "ic", vs); break; }
              if (ends("\05" "ative", vs)) { r("\00" "", vs); break; }
              if (ends("\05" "alize", vs)) { r("\02" "al", vs); break; }
              break;
    case 'i': if (ends("\05" "iciti", vs)) { r("\02" "ic", vs); break; }
              break;
    case 'l': if (ends("\04" "ical", vs)) { r("\02" "ic", vs); break; }
              if (ends("\03" "ful", vs)) { r("\00" "", vs); break; }
              break;
    case 's': if (ends("\04" "ness", vs)) { r("\00" "", vs); break; }
              break;
  }
}

/* step4() takes off -ant, -ence etc., in context <c>vcvc<v>. */

static void step4(vars *vs)
{  switch (vs->b[vs->k-1])
    {  case 'a': if (ends("\02" "al", vs)) break; return;
       case 'c': if (ends("\04" "ance", vs)) break;
                 if (ends("\04" "ence", vs)) break; return;
       case 'e': if (ends("\02" "er", vs)) break; return;
       case 'i': if (ends("\02" "ic", vs)) break; return;
       case 'l': if (ends("\04" "able", vs)) break;
                 if (ends("\04" "ible", vs)) break; return;
       case 'n': if (ends("\03" "ant", vs)) break;
                 if (ends("\05" "ement", vs)) break;
                 if (ends("\04" "ment", vs)) break;
                 if (ends("\03" "ent", vs)) break; return;
       case 'o': if (ends("\03" "ion", vs) && vs->j > 0 && (vs->b[vs->j] == 's' || vs->b[vs->j] == 't')) break;
                 if (ends("\02" "ou", vs)) break; return;
                 /* takes care of -ous */
       case 's': if (ends("\03" "ism", vs)) break; return;
       case 't': if (ends("\03" "ate", vs)) break;
                 if (ends("\03" "iti", vs)) break; return;
       case 'u': if (ends("\03" "ous", vs)) break; return;
       case 'v': if (ends("\03" "ive", vs)) break; return;
       case 'z': if (ends("\03" "ize", vs)) break; return;
       default: return;
    }
    if (m(vs) > 1) vs->k = vs->j;
}

/* step5() removes a final -e if m() > 1, and changes -ll to -l if
   m() > 1. */

static void step5(vars *vs)
{  vs->j = vs->k;
   if (vs->b[vs->k] == 'e')
   {  int a = m(vs);
      if (a > 1 || (a == 1 && !cvc(vs->k-1, vs))) vs->k--;
   }
   if (vs->b[vs->k] == 'l' && doublec(vs->k, vs) && m(vs) > 1) vs->k--;
}

/* In stem(p,i,vs->j), p is a char pointer, and the string to be stemmed is from
   p[i] to p[vs->j] inclusive. Typically i is zero and vs->j is the offset to the last
   character of a string, (p[vs->j+1] == '\0'). The stemmer adjusts the
   characters p[i] ... p[vs->j] and returns the new end-point of the string, vs->k.
   Stemming never increases word length, so i <= vs->k <= vs->j. To turn the stemmer
   into a module, declare 'stem' as extern, and delete the remainder of this
   file.
*/

static int
stem(char * p, int i, int j)
{ vars vs; 

  vs.b = p; vs.k = j; vs.k0 = i; 
  if ( vs.k <= vs.k0+1) return vs.k; /*-DEPARTURE-*/

   /* With this line, strings of length 1 or 2 don't go through the
      stemming process, although no mention is made of this in the
      published algorithm. Remove the line to match the published
      algorithm. */

  step1ab(&vs); step1c(&vs); step2(&vs); step3(&vs); step4(&vs); step5(&vs);
  return vs.k;
}

/*--------------------stemmer definition ends here------------------------*/

/* SWI-Prolog hooks */

static int unaccent(const char *in, size_t len, char *out, size_t size);

static foreign_t
pl_stem(term_t t_in, term_t t_stem)
{ char *word;
  size_t len, end;
  char *f, *t, *s, *ew;
  char buf[1024];
  char plain[1024];
  long l;
  int rc;

  if ( !PL_get_nchars(t_in, &len, &word, CVT_ALL|CVT_EXCEPTION) )
  { if ( PL_is_number(t_in) )
      return PL_unify(t_in, t_stem);
    return FALSE;
  }
  ew = &word[len];
  s = len+1 > sizeof(buf) ? PL_malloc(len+1) : buf;
  for(f=word, t=s; f<ew; )
    *t++ = tolower(*f++);

  if ( (l=unaccent(s, t-s, plain, sizeof(plain))) < (long)sizeof(plain) )
  { if ( l >= 0 )
    { if ( s != buf )
	PL_free(s);
      s = plain;
    }
  } else
  { char *s2 = PL_malloc(l+1);
    unaccent(s, t-s, s2, l+1);
    if ( s != buf )
      PL_free(s);
    s = s2;
  }

  end = stem(s, 0, (int)(len - 1));
  s[end + 1] = '\0';
  
  rc = PL_unify_atom_chars(t_stem, s);
  if ( s != plain && s != buf )
    PL_free(s);

  return rc;
}


		 /*******************************
		 *	       ACCENTS		*
		 *******************************/

static char *unaccent_def[] =
{ "A",	/* 192 */ "A", /* 193 */ "A",	/* 194 */ "A",	/* 195 */
  "A",	/* 196 */ "A", /* 197 */ "AE",	/* 198 */ "C",	/* 199 */
  "E",	/* 200 */ "E", /* 201 */ "E",	/* 202 */ "E",	/* 203 */
  "I",	/* 204 */ "I", /* 205 */ "I",	/* 206 */ "I",	/* 207 */
  "D",	/* 208 */ "N", /* 209 */ "O",	/* 210 */ "O",	/* 211 */
  "O",	/* 212 */ "O", /* 213 */ "O",	/* 214 */ NULL,	/* 215 */
  NULL,	/* 216 */ "U", /* 217 */ "U",	/* 218 */ "U",	/* 219 */
  "U",	/* 220 */ "Y", /* 221 */ NULL,	/* 222 */ "ss",	/* 223 */
  "a",	/* 224 */ "a", /* 225 */ "a",	/* 226 */ "a",	/* 227 */
  "a",	/* 228 */ "a", /* 229 */ "ae",	/* 230 */ "c",	/* 231 */
  "e",	/* 232 */ "e", /* 233 */ "e",	/* 234 */ "e",	/* 235 */
  "i",	/* 236 */ "i", /* 237 */ "i",	/* 238 */ "i",	/* 239 */
  "d",	/* 240 */ "n", /* 241 */ "o",	/* 242 */ "o",	/* 243 */
  "o",	/* 244 */ "o", /* 245 */ "o",	/* 246 */ NULL,	/* 247 */
  NULL,	/* 248 */ "u", /* 249 */ "u",	/* 250 */ "u",	/* 251 */
  "u",	/* 252 */ "y", /* 253 */ NULL,	/* 254 */ "y",	/* 255 */
};


static int
unaccent(const char *in, size_t len, char *out, size_t size)
{ char *to = out, *toe = &out[size];
  const char *ein = &in[len];
  int changes = 0;

  for( ; in < ein; in++)
  { int c = (*in)&0xff;
    char *m;

    if ( c < 192 || !(m=unaccent_def[c-192]) )
    { if ( to < toe )
	*to = c;
      to++;
    } else
    { changes++;

      while(*m)
      { if ( to < toe )
	  *to = *m;
	to++;
	m++;
      }
    }
  }

  if ( to < toe )
    *to = '\0';

  if ( changes == 0 )
    return (int)(out-to);		/* no change: negative */

  return (int)(to-out);
}


static foreign_t
pl_unaccent(term_t from, term_t to)
{ char buf[1024];
  char *f;
  int len;
  size_t fl;

  if ( !PL_get_nchars(from, &fl, &f, CVT_ALL|CVT_EXCEPTION) )
    return FALSE;

  if ( (len=unaccent(f, fl, buf, sizeof(buf))) <= (int)sizeof(buf) )
  { if ( len < 0 )			/* no change */
      return PL_unify(to, from);
    else
      return PL_unify_atom_nchars(to, len, buf);
  } else
  { char *t = PL_malloc(len+1);
    int rc;

    unaccent(f, fl, t, len+1);
    rc = PL_unify_atom_nchars(to, len, t);
    PL_free(t);
    return rc;
  }
}


		 /*******************************
		 *	     TOKENISE		*
		 *******************************/

typedef enum
{ TOK_INT,
  TOK_FLOAT,
  TOK_WORD,
  TOK_PUNCT,
  TOK_UNKNOWN
} toktype;

static int
tokenize(const char *in, size_t len,
	 int (*call)(const char *s,
		     size_t len,
		     toktype type,
		     void *closure),
	 void *closure)
{ const unsigned char *s = (const unsigned char*)in;
  const unsigned char *se = &s[len];
  toktype type;

  while(s<se)
  { const unsigned char *st;		/* start token */

    while(s<se && iswspace(*s))		/* skip blanks */
      s++;
    if ( s >= se )
      break;

    st = s;
    type = TOK_UNKNOWN;

    if ( (*s == '-' || *s == '+') && se-s > 1 && iswdigit(s[1]) )
    { s += 2;
      type = TOK_INT;
    } else if ( iswdigit(*s) )
    { s++;
      type = TOK_INT;
    }

    if ( type == TOK_INT )
    { while(s<se && iswdigit(*s))
	s++;
      if ( s+2 < se && *s == '.' && iswdigit(s[1]) )
      { s += 2;
	type = TOK_FLOAT;
	while(s<se && iswdigit(*s))
	  s++;
      }
      if ( s+2 < se &&
	   (*s == 'e' || *s == 'E') &&
	   (iswdigit(s[1]) || (s[1] == '-' && iswdigit(s[2]))) )
      { s += 2;
	type = TOK_FLOAT;
	while(s<se && iswdigit(*s))
	  s++;
      }

      if ( !(*call)((const char*)st, s-st, type, closure) )
	return FALSE;
    } else if ( iswalnum(*s) )
    { while(s<se && iswalnum(*s))
	s++;
      if ( !(*call)((const char*)st, s-st, TOK_WORD, closure) )
	return FALSE;
    } else
    { s++;
      if ( !(*call)((const char*)st, 1, TOK_PUNCT, closure) )
	return FALSE;
    }
  }

  return TRUE;
}


typedef struct
{ term_t head;
  term_t tail;
} list;


static int
unify_token(const char *s, size_t len, toktype type, void *closure)
{ list *l = closure;

  if ( PL_unify_list(l->tail, l->head, l->tail) )
  { char *ep;

    switch(type)
    { case TOK_INT:
#ifdef __WINDOWS__
      { long val = strtol(s, &ep, 10);

	return PL_unify_integer(l->head, val);
      }
#else
      { int64_t val = strtoll(s, &ep, 10);

	return PL_unify_int64(l->head, val);
      }
#endif
      case TOK_FLOAT:
      { double val = strtod(s, &ep);

	return PL_unify_float(l->head, val);
      }
      default:
	return PL_unify_atom_nchars(l->head, len, s);
    }
  }

  return FALSE;
}



static foreign_t
pl_tokenize(term_t text, term_t tokens)
{ char *s;
  size_t len;
  list l;

  if ( !PL_get_nchars(text, &len, &s, CVT_ALL|CVT_EXCEPTION) )
    return FALSE;

  l.tail = PL_copy_term_ref(tokens);
  l.head = PL_new_term_ref();

  if ( !tokenize(s, len, unify_token, &l) )
    return FALSE;

  return PL_unify_nil(l.tail);
}


static int
unify_stem(const char *s, size_t len, toktype type, void *closure)
{ list *list = closure;

  if ( type == TOK_PUNCT )
    return TRUE;
  if ( type == TOK_INT || type == TOK_FLOAT )
    return unify_token(s, len, type, closure);

  if ( PL_unify_list(list->tail, list->head, list->tail) )
  { char tmp[1024];
    char *buf = tmp;
    char *q;
    int i, rc, end, l;

					/* unaccent */
    l = abs(unaccent(s, len, buf, sizeof(tmp)));
    if ( l > (int)sizeof(buf) )
    { buf = PL_malloc(l+1);
      unaccent(s, len, buf, l+1);
    }

					/* downcase */
    for(q=buf, i=0; i++ < l; q++)
      *q = tolower(*q);
    
    end = stem(buf, 0, l-1);
    buf[++end] = '\0';

    rc = PL_unify_atom_nchars(list->head, end, buf);
    if ( buf != tmp )
      PL_free(buf);

    return rc;
  }

  return FALSE;
}



static foreign_t
pl_atom_to_stem_list(term_t text, term_t stems)
{ char *s;
  size_t len;
  list l;

  if ( !PL_get_nchars(text, &len, &s, CVT_ALL) )
    return FALSE;

  l.tail = PL_copy_term_ref(stems);
  l.head = PL_new_term_ref();

  if ( !tokenize(s, len, unify_stem, &l) )
    return FALSE;

  return PL_unify_nil(l.tail);
}




		 /*******************************
		 *	      INSTALL		*
		 *******************************/

install_t
install_porter_stem()
{ PL_register_foreign("porter_stem",       2, pl_stem,     0);
  PL_register_foreign("unaccent_atom",	   2, pl_unaccent, 0);
  PL_register_foreign("tokenize_atom",	   2, pl_tokenize, 0);
  PL_register_foreign("atom_to_stem_list", 2, pl_atom_to_stem_list, 0);
}


/* end of SWI-Prolog */
