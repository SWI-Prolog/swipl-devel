/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2024, University of Amsterdam
                              VU University Amsterdam
			      SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#ifndef PL_TEXT_H_INCLUDED
#define PL_TEXT_H_INCLUDED

typedef enum
{ PL_CHARS_VIRGIN = 0,			/* not initialised */
  PL_CHARS_MALLOC,			/* malloced data */
  PL_CHARS_STACK,			/* stored in the buffer ring */
  PL_CHARS_HEAP,			/* stored in program area (atoms) */
  PL_CHARS_PROLOG_STACK,			/* stored on the global stack */
  PL_CHARS_LOCAL			/* stored in in-line buffer */
} PL_chars_alloc_t;

					/* PL_canonicalise_text() errors */
#define ERR_TEXT_ILLEGAL_UTF8		       -1
#define ERR_TEXT_ILLEGAL_UTF16		       -2
#define ERR_TEXT_ILLEGAL_MULTIBYTE_SEQUENCE    -3
#define ERR_TEXT_INCOMPLETE_MULTIBYTE_SEQUENCE -4
#define ERR_TEXT_INVALID_CODE_POINT	       -5

typedef struct
{ union
  { char *t;				/* traditional 8-bit char* */
    pl_wchar_t *w;			/* wide character string */
  } text;
  size_t length;
					/* private stuff */
  IOENC encoding;			/* how it is encoded */
  PL_chars_alloc_t storage;		/* how it is stored */
  int canonical;			/* true: ENC_ISO_LATIN_1 or ENC_WCHAR */
  char buf[100];			/* buffer for simple stuff */
} PL_chars_t;

#define PL_init_text(txt) \
	{ (txt)->text.t    = NULL; \
	  (txt)->encoding  = ENC_UNKNOWN; \
	  (txt)->storage   = PL_CHARS_LOCAL; \
	  (txt)->canonical = false; \
	}

#if USE_LD_MACROS
#define	PL_get_text(l, text, flags)	LDFUNC(PL_get_text, l, text, flags)
#define textToAtom(text)		LDFUNC(textToAtom, text)
#define textToString(text)		LDFUNC(textToString, text)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

bool	PL_unify_text(term_t term, term_t tail, PL_chars_t *text, int type);
int	PL_unify_text_range(term_t term, const PL_chars_t *text,
			    size_t from, size_t len, int type);

int	PL_promote_text(PL_chars_t *text);
int	PL_mb_text(PL_chars_t *text, int flags);
int	PL_canonicalise_text(PL_chars_t *text) WUNUSED;
int	PL_canonicalise_text_ex(PL_chars_t *text) WUNUSED;

int	PL_cmp_text(PL_chars_t *t1, size_t o1, PL_chars_t *t2, size_t o2,
		    size_t len);
int	PL_concat_text(int n, PL_chars_t **text, PL_chars_t *result);

void	PL_free_text(PL_chars_t *text);
int	PL_save_text(PL_chars_t *text, int flags);
size_t  utf16_text_length(const PL_chars_t *text);

static inline size_t
PL_text_length(const PL_chars_t *text)
{ assert(text->encoding == ENC_ISO_LATIN_1 || text->encoding == ENC_WCHAR);
#if SIZEOF_WCHAR_T == 2
  if ( text->encoding == ENC_WCHAR )
    return utf16_text_length(text);
#endif
  return text->length;
}


int		PL_get_text(term_t l, PL_chars_t *text, int flags);
atom_t		textToAtom(PL_chars_t *text);
word		textToString(PL_chars_t *text);

IOSTREAM *	Sopen_text(PL_chars_t *text, const char *mode);
int		PL_text_recode(PL_chars_t *text, IOENC encoding);

/* Moved to pl-fli.c:
 int get_atom_ptr_text(Atom atom, PL_chars_t *text);
 int get_atom_text(atom_t atom, PL_chars_t *text);
 int get_string_text(atom_t atom, PL_chars_t *text);
*/
#undef LDFUNC_DECLARATIONS

static inline int
text_get_char(const PL_chars_t *t, size_t i)
{ assert(t->canonical);
  return t->encoding == ENC_ISO_LATIN_1 ? t->text.t[i]&0xff
					: t->text.w[i];
}


static inline size_t
text_chr(const PL_chars_t *t, int chr)
{ assert(t->canonical);
  if ( t->encoding == ENC_ISO_LATIN_1 )
  { if ( chr <= 0xff )
    { char *e = strchr(t->text.t, chr);
      if ( e )
	return e-t->text.t;
    }
  } else
  { wchar_t *e = wcschr(t->text.w, chr);
    if ( e )
      return(e-t->text.w);
  }

  return (size_t)-1;
}

		/*******************************
		*         STRING BUFFERS       *
		*******************************/

#define PL_STRINGS_MARK_IF_MALLOC(flags) \
	{ buf_mark_t __PL_mark; \
	  if ( flags&BUF_MALLOC ) \
	    PL_mark_string_buffers(&__PL_mark);
#define PL_STRINGS_RELEASE_IF_MALLOC(flags) \
	  if ( flags&BUF_MALLOC ) \
	    PL_release_string_buffers_from_mark(__PL_mark); \
	}

#endif /*PL_TEXT_H_INCLUDED*/
