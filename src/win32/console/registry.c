/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  1999-2011, University of Amsterdam
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
