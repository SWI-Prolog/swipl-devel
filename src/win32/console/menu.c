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

static struct rl_item
{ UINT	      id;
  const char *name;
} rl_items[] =
{ { IDM_EXIT, "&Exit" },
  { IDM_CUT,  "&Cut" },
  { IDM_COPY, "&Copy" },
  { IDM_PASTE, "&Paste" },
  { IDM_BREAK, "&Interrupt" },
  { IDM_FONT,  "&Font ..." },
  { 0,         NULL }
};



static UINT
lookupMenuLabel(const char *label)
{ int i;
  int llen;
  struct rl_item *builtin;

  for(builtin = rl_items; builtin->id; builtin++)
  { if ( streq(builtin->name, label) )
      return builtin->id;
  }

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

  llen = strlen(label);
  menuids[nmenus] = rlc_malloc(llen+1);
  memcpy(menuids[nmenus], label, llen+1);
  
  return nmenus++ + IDM_USER;
}


const char *
lookupMenuId(UINT id)
{ struct rl_item *builtin;

  if ( id >= IDM_USER && (int)id - IDM_USER < nmenus )
    return menuids[id-IDM_USER];

  for(builtin = rl_items; builtin->id; builtin++)
  { if ( builtin->id == id )
      return builtin->name;
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
      { if ( streq(name, nbuf) )
	{ if ( pos )
	    *pos = i;

	  return info.hSubMenu;
	}
      }
    }
  }

  return 0;
}


static void
append_builtin(HMENU menu, UINT id)
{ AppendMenu(menu, MF_STRING, id, lookupMenuId(id));
}


void
rlc_add_menu_bar(HWND cwin)
{ HMENU menu     = CreateMenu();
  HMENU file	 = CreatePopupMenu();
  HMENU edit     = CreatePopupMenu();
  HMENU settings = CreatePopupMenu();
  HMENU run      = CreatePopupMenu();

  append_builtin(file, IDM_EXIT);

/*append_builtin(edit, IDM_CUT);*/
  append_builtin(edit, IDM_COPY);
  append_builtin(edit, IDM_PASTE);
  
  append_builtin(settings, IDM_FONT);

  append_builtin(run,  IDM_BREAK);

  AppendMenu(menu, MF_POPUP, (UINT)file,     "&File");
  AppendMenu(menu, MF_POPUP, (UINT)edit,     "&Edit");
  AppendMenu(menu, MF_POPUP, (UINT)settings, "&Settings");
  AppendMenu(menu, MF_POPUP, (UINT)run,      "&Run");

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
  HWND hwnd = rlc_hwnd();

  if ( !(mb = GetMenu(hwnd)) )
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
					/* force redraw; not automatic! */
    RedrawWindow(hwnd, NULL, 0,
		 RDW_FRAME|RDW_INVALIDATE|RDW_NOINTERNALPAINT);
  }

  return TRUE;
}

