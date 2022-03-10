/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2011-2022, University of Amsterdam
			      CWI, Amsterdam
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

#ifndef PL_FILE_H_INCLUDED
#define PL_FILE_H_INCLUDED

typedef enum
{ ST_FALSE = -1,			/* Do not check stream types */
  ST_LOOSE = 0,				/* Default: accept latin-1 for binary */
  ST_TRUE  = 1				/* Strict checking */
} st_check;

typedef enum iri_op
{ IRI_OPEN,		/* const char *how, term_t options -> IOSTREAM **s */
  IRI_ACCESS,		/* const char *mode -> bool */
  IRI_SIZE,		/* -> int64_t *sz */
  IRI_TIME		/* -> double *time */
} iri_op;

#if USE_LD_MACROS
#define	getTextInputStream(t, s)	LDFUNC(getTextInputStream, t, s)
#define	getBinaryInputStream(t, s)	LDFUNC(getBinaryInputStream, t, s)
#define	getTextOutputStream(t, s)	LDFUNC(getTextOutputStream, t, s)
#define	getBinaryOutputStream(t, s)	LDFUNC(getBinaryOutputStream, t, s)
#endif /*USE_LD_MACROS*/

#define LDFUNC_DECLARATIONS

/* pl-file.c */
void		initIO(void);
void		dieIO(void);
void		closeFiles(int all);
int		openFileDescriptors(unsigned char *buf, int size);
void		protocol(const char *s, size_t n);
int		getTextInputStream(term_t t, IOSTREAM **s);
int		getBinaryInputStream(term_t t, IOSTREAM **s);
int		getTextOutputStream(term_t t, IOSTREAM **s);
int		getBinaryOutputStream(term_t t, IOSTREAM **s);
int	        reportStreamError(IOSTREAM *s);
int		streamStatus(IOSTREAM *s);
int		setFileNameStream(IOSTREAM *s, atom_t name);
atom_t		fileNameStream(IOSTREAM *s);
int		getSingleChar(IOSTREAM *s, int signals);
int		readLine(IOSTREAM *in, IOSTREAM *out, char *buffer);
int		LockStream(void);
int		UnlockStream(void);
IOSTREAM *	PL_current_input(void);
IOSTREAM *	PL_current_output(void);
int		pl_see(term_t f);
int		pl_seen(void);
int		seeString(const char *s);
int		seeingString(void);
int		seenString(void);
int		tellString(char **s, size_t *size, IOENC enc);
int		toldString(void);
void		prompt1(atom_t prompt);
atom_t		PrologPrompt(void);
int		streamNo(term_t spec, int mode);
void		release_stream_handle(term_t spec);
int		unifyTime(term_t t, time_t time);
#ifdef __WINDOWS__
word		pl_make_fat_filemap(term_t dir);
#endif
int		PL_unify_stream_or_alias(term_t t, IOSTREAM *s);
void		pushOutputContext(void);
void		popOutputContext(void);
int		setupOutputRedirect(term_t to,
				    redir_context *ctx,
				    int redir);
int		closeOutputRedirect(redir_context *ctx);
void		discardOutputRedirect(redir_context *ctx);
int		push_input_context(atom_t type);
int		pop_input_context(void);
int		stream_encoding_options(atom_t type, atom_t encoding,
					int *bom, IOENC *enc);
int		file_name_is_iri(const char *path);
int		iri_hook(const char *url, iri_op op, ...);
atom_t		file_name_to_atom(const char *fn);

#undef LDFUNC_DECLARATIONS

#endif /*PL_FILE_H_INCLUDED*/
