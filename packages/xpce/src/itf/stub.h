/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1995 University of Amsterdam. All rights reserved.
*/

/* console.c */
void		Stub__vCprintf(const char *fmt, va_list args);
int		Stub__Cputchar(int chr);
void		Stub__Cflush(void);
char *		Stub__Cgetline(char *line, int size);

/* stub.c */
int		Stub__HostActionv(int action, va_list args);
int		Stub__HostQuery(int what, PceCValue *value);
int		Stub__HostSend(PceObject prolog, PceName sel, int argc, PceObject *argv);
PceObject	Stub__HostGet(PceObject prolog, PceName sel, int argc, PceObject *argv);
int		Stub__HostCallProc(PceObject handle,
				   PceObject rec, PceObject sel,
				   int ac, PceObject *av);
PceObject	Stub__HostCallFunc(PceObject handle,
				   PceObject rec, PceObject sel,
				   int ac, PceObject *av);
