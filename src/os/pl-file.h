/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2011, University of Amsterdam

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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#ifndef PL_FILE_H_INCLUDED
#define PL_FILE_H_INCLUDED

typedef enum
{ ST_FALSE = -1,			/* Do not check stream types */
  ST_LOOSE = 0,				/* Default: accept latin-1 for binary */
  ST_TRUE  = 1				/* Strict checking */
} st_check;

/* pl-file.c */
COMMON(void)		initIO(void);
COMMON(void)		dieIO(void);
COMMON(void)		closeFiles(int all);
COMMON(int)		openFileDescriptors(unsigned char *buf, int size);
COMMON(void)		protocol(const char *s, size_t n);
COMMON(int)		getTextInputStream__LD(term_t t, IOSTREAM **s ARG_LD);
COMMON(int)		getBinaryInputStream__LD(term_t t, IOSTREAM **s ARG_LD);
COMMON(int)		getTextOutputStream__LD(term_t t, IOSTREAM **s ARG_LD);
COMMON(int)		getBinaryOutputStream__LD(term_t t, IOSTREAM **s ARG_LD);
COMMON(int)	        reportStreamError(IOSTREAM *s);
COMMON(int)		streamStatus(IOSTREAM *s);
COMMON(atom_t)		fileNameStream(IOSTREAM *s);
COMMON(int)		getSingleChar(IOSTREAM *s, int signals);
COMMON(int)		readLine(IOSTREAM *in, IOSTREAM *out, char *buffer);
COMMON(int)		LockStream(void);
COMMON(int)		UnlockStream(void);
COMMON(IOSTREAM *)	PL_current_input(void);
COMMON(IOSTREAM *)	PL_current_output(void);
COMMON(int)		pl_see(term_t f);
COMMON(int)		pl_seen(void);
COMMON(int)		seeString(const char *s);
COMMON(int)		seeingString(void);
COMMON(int)		seenString(void);
COMMON(int)		tellString(char **s, size_t *size, IOENC enc);
COMMON(int)		toldString(void);
COMMON(void)		prompt1(atom_t prompt);
COMMON(atom_t)		PrologPrompt(void);
COMMON(int)		streamNo(term_t spec, int mode);
COMMON(void)		release_stream_handle(term_t spec);
COMMON(int)		unifyTime(term_t t, time_t time);
#ifdef __WINDOWS__
COMMON(word)		pl_make_fat_filemap(term_t dir);
#endif
COMMON(int)		PL_unify_stream_or_alias(term_t t, IOSTREAM *s);
COMMON(void)		pushOutputContext(void);
COMMON(void)		popOutputContext(void);
COMMON(IOENC)		atom_to_encoding(atom_t a);
COMMON(atom_t)		encoding_to_atom(IOENC enc);
COMMON(int)		setupOutputRedirect(term_t to,
					    redir_context *ctx,
					    int redir);
COMMON(int)		closeOutputRedirect(redir_context *ctx);
COMMON(void)		discardOutputRedirect(redir_context *ctx);
COMMON(int)		push_input_context(atom_t type);
COMMON(int)		pop_input_context(void);

#endif /*PL_FILE_H_INCLUDED*/
