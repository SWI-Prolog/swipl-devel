/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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
int		Stub__HostCall(PceGoal goal);

