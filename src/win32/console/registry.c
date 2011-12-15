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
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*/

#include <windows.h>
#include "registry.h"

#define MAXKEYLEN	256
#define MAXKEYPATHLEN	1024

static TCHAR _rlc_regbase[MAXKEYPATHLEN] = TEXT("current_user/PrologConsole");

static HKEY
reg_open_key(const TCHAR *path, HKEY parent, REGSAM access)
{ TCHAR buf[MAXKEYLEN];
  TCHAR *sep;

  if ( *path )
    return parent;

  for(sep = path; *sep && *sep != '/' && *sep != '\\'; sep++)
    ;
  strncpy(buf, path, sep-path);
  if ( *sep )
    sep++;

  if ( strchr(sep, '/') || strchr(sep, '\\') ) /* there is more */
  { HKEY sub;

    if ( RegOpenKeyEx(parent, buf, 0L, KEY_READ, &sub) != ERROR_SUCCESS )
      return NULL;

    return reg_open_key(sep, sub, access);
  } else
  { HKEY sub;

    if ( RegOpenKeyEx(parent, buf, 0L, KEY_READ, access) != ERROR_SUCCESS )
      return NULL;

    return sub;
  }
}


HKEY
RegOpenKeyFromPath(const TCHAR *path, REGSAM access)
{ TCHAR buf[MAXKEYLEN];
  TCHAR *sep;
  HKEY root;

  for(sep = path; *sep && *sep != '/' && *sep != '\\'; sep++)
    ;
  strncpy(buf, path, sep-path);
  if ( streq(buf, TEXT("classes_root")) )
    root = HKEY_CLASSES_ROOT;
  else if ( streq(buf, TEXT("current_user")) )
    root = HKEY_CURRENT_USER;
  else if ( streq(buf, TEXT("local_machine")) )
    root = HKEY_LOCAL_MACHINE;
  else if ( streq(buf, TEXT("users")) )
    root = HKEY_USERS;
  else
    return NULL;

  if ( *sep )
    sep++;

  return reg_open_key(sep, root, REGSAM);
}
