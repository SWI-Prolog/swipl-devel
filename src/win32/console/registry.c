/*  $Id$

    Part of Console
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1997 University of Amsterdam. All rights reserved.
*/

#include <windows.h>
#include "registry.h"

#define MAXKEYLEN	256
#define MAXKEYPATHLEN	1024

static char _rlc_regbase[MAXKEYPATHLEN] = "current_user/PrologConsole";

static HKEY
reg_open_key(const char *path, HKEY parent, REGSAM access)
{ char buf[MAXKEYLEN];
  char *sep;

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
RegOpenKeyFromPath(const char *path, REGSAM access)
{ char buf[MAXKEYLEN];
  char *sep;
  HKEY root;

  for(sep = path; *sep && *sep != '/' && *sep != '\\'; sep++)
    ;
  strncpy(buf, path, sep-path);
  if ( streq(buf, "classes_root") )
    root = HKEY_CLASSES_ROOT;
  else if ( streq(buf, "current_user") )
    root = HKEY_CURRENT_USER;
  else if ( streq(buf, "local_machine") )
    root = HKEY_LOCAL_MACHINE;
  else if ( streq(buf, "users") )
    root = HKEY_USERS;
  else
    return NULL;

  if ( *sep )
    sep++;

  return reg_open_key(sep, root, REGSAM);
}


static char *
local_path(const char *key)
{
}


int
rlc_get_profile_int(const char *name)
{ 
}
