/*  $Id$

    Part of SWI-Prolog

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2002 SWI, University of Amsterdam. All rights reserved.
*/

#define IDM_USER 100			/* reserve below 100 */
#define MAXLABELLEN 256			/* max length of menu-item label */

#define IDM_EXIT	10
#define IDM_CUT		11
#define IDM_COPY	12
#define IDM_PASTE	13
#define IDM_BREAK	14

const char *lookupMenuId(UINT id);
void 	    rlc_add_menu_bar(HWND win);
