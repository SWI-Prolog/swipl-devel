/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi-prolog.org
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

#ifndef _DIRENT_H_INCLUDED
#define _DIRENT_H_INCLUDED

#include <io.h>

#undef _export
#if defined(_UXNT_KERNEL) && !defined(__LCC__)
#define _export _declspec(dllexport)
#else
#define _export extern
#endif

#ifndef NAME_MAX
#define NAME_MAX 255
#endif

typedef struct dirent
{ void *      		data;		/* actually WIN32_FIND_DATA * */
  int			first;
  void *		handle;		/* actually HANDLE */
  void *		map;		/* win32s long filename mapping */
					/* dirent */
  char			d_name[NAME_MAX+1];
} DIR;

_export DIR *		opendir(const char *path);
_export int		closedir(DIR *dp);
_export struct dirent *	readdir(DIR *dp);

#endif /*_DIRENT_H_INCLUDED*/
