/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2008, University of Amsterdam

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

#ifndef PL_FILES_H_INCLUDED
#define PL_FILES_H_INCLUDED

#define ACCESS_EXIST	0		/* AccessFile() modes */
#define ACCESS_EXECUTE	1
#define ACCESS_READ	2
#define ACCESS_WRITE	4

COMMON(void)		initFiles(void);
COMMON(int)		LastModifiedFile(const char *f, double *t);
COMMON(int)		RemoveFile(const char *path);
COMMON(int)		AccessFile(const char *path, int mode);
COMMON(char *)		DeRefLink(const char *link, char *buf);
COMMON(int)		ExistsFile(const char *path);
COMMON(int)		ExistsDirectory(const char *path);

#endif /*PL_FILES_H_INCLUDED*/
