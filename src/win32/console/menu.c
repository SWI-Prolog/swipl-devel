/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2002 SWI, University of Amsterdam. All rights reserved.
*/

#include <windows.h>
#define _MAKE_DLL
#include "console.h"
#include "menu.h"

#ifndef EOS
#define EOS 0
#endif

#define streq(s,q) (strcmp((s), (q)) == 0)

static char **menuids;
static int nmenus;
static int nmenualloc;

static int
mklabelid(char *buf)
{ char *s, *o;

  for(s=o=buf; *s; s++)
  { if ( *s != '&' )
      *o++ = tolower(*s);
  }
  *o = EOS;

  return o-buf;
}

static UINT
lookupMenuLabel(const char *label)
{ int i;
  char lbuf[MAXLABELLEN];
  int llen;

  strcpy(lbuf, label);
  llen = mklabelid(lbuf);

  if ( streq(lbuf, "exit") )  return IDM_EXIT;
  if ( streq(lbuf, "cut") )   return IDM_CUT;
  if ( streq(lbuf, "copy") )  return IDM_COPY;
  if ( streq(lbuf, "paste") ) return IDM_PASTE;

  for(i=0; i<nmenus; i++)
  { if ( streq(menuids[i], label) )
      return i + IDM_USER;
  }

  if ( nmenus + 1 > nmenualloc )
  { if ( nmenualloc )
    { nmenualloc *= 2;
      menuids = rlc_realloc(menuids, nmenualloc*sizeof(char *));
    } else
    { nmenualloc = 32;
      menuids = rlc_malloc(nmenualloc*sizeof(char *));
    }
  }

  menuids[nmenus] = rlc_malloc(llen+1);
  memcpy(menuids[nmenus], lbuf, llen+1);
  
  return nmenus++ + IDM_USER;
}


const char *
lookupMenuId(UINT id)
{ if ( id >= IDM_USER && (int)id - IDM_USER < nmenus )
    return menuids[id-IDM_USER];

  switch(id)
  { case IDM_EXIT:  return "exit";
    case IDM_CUT:   return "cut";
    case IDM_COPY:  return "copy";
    case IDM_PASTE: return "paste";
  }

  return NULL;
}

int
insertMenu(HMENU in, const char *label, const char *before)
{ if ( !before )
  { if ( !label )
      AppendMenu(in, MF_SEPARATOR, 0, NULL);
    else
    { UINT id = lookupMenuLabel(label);
      
      AppendMenu(in, MF_STRING, id, label);
    }
  } else
  { UINT bid = lookupMenuLabel(before);
    MENUITEMINFO info;

    memset(&info, 0, sizeof(info));
    info.cbSize = sizeof(info);
    info.fMask = MIIM_TYPE;
    if ( label )
    { info.fType = MFT_STRING;
      info.fMask |= MIIM_ID;
      info.wID = lookupMenuLabel(label);
      info.dwTypeData = (char *)label;
      info.cch = strlen(label);
    } else
    { info.fType = MFT_SEPARATOR;
    }

    InsertMenuItem(in, bid, FALSE, &info);
  }

  return TRUE;
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Find popup with given name.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static HMENU
findPopup(const char *name, int *pos)
{ HMENU mb = GetMenu(rlc_hwnd());

  if ( mb )
  { int i;
    MENUITEMINFO info;

    memset(&info, 0, sizeof(info));
    info.cbSize = sizeof(info);
    info.fMask  = MIIM_TYPE;

    for(i=0; ; i++)
    { MENUITEMINFO info;
      char nbuf[MAXLABELLEN];

      memset(&info, 0, sizeof(info));
      info.cbSize = sizeof(info);
      info.fMask  = MIIM_TYPE|MIIM_SUBMENU;
      info.dwTypeData = nbuf;
      info.cch = sizeof(nbuf);

      if ( !GetMenuItemInfo(mb, i, TRUE, &info) )
	return NULL;

      if ( info.fType == MF_STRING )
      { mklabelid(nbuf);

	if ( streq(name, nbuf) )
	{ if ( pos )
	    *pos = i;

	  return info.hSubMenu;
	}
      }
    }
  }

  return 0;
}


void
rlc_add_menu_bar(HWND cwin)
{ HMENU menu = CreateMenu();
  HMENU file = CreatePopupMenu();
  HMENU edit = CreatePopupMenu();
  HMENU help = CreatePopupMenu();

  AppendMenu(file, MF_STRING, IDM_EXIT,  "&Exit");

/*AppendMenu(edit, MF_STRING, IDM_CUT,   "&Cut");*/
  AppendMenu(edit, MF_STRING, IDM_COPY,  "&Copy");
  AppendMenu(edit, MF_STRING, IDM_PASTE, "&Paste");
  
  AppendMenu(menu, MF_POPUP, (UINT)file, "&File");
  AppendMenu(menu, MF_POPUP, (UINT)edit, "&Edit");

  SetMenu(cwin, menu);
}


int
rlc_insert_menu_item(const char *menu, const char *label, const char *before)
{ HMENU popup;

  if ( (popup = findPopup(menu, NULL)) )
    return insertMenu(popup, label, before);
      
  return FALSE;
}


int
rlc_insert_menu(const char *label, const char *before)
{ HMENU mb;

  if ( !(mb = GetMenu(rlc_hwnd())) )
    return FALSE;

  if ( !findPopup(label, NULL) )	/* already there */
  { MENUITEMINFO info;
    int bid = -1;

    if ( before )
      findPopup(before, &bid);

    memset(&info, 0, sizeof(info));
    info.cbSize = sizeof(info);
    info.fMask = MIIM_TYPE|MIIM_SUBMENU;
    info.fType = MFT_STRING;
    info.hSubMenu = CreatePopupMenu();
    info.dwTypeData = (char *)label;
    info.cch = strlen(label);
    
    InsertMenuItem(mb, bid, TRUE, &info);
  }

  return TRUE;
}

