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

					/* see also console.c */
#define WM_RLC_MENU	 WM_USER+15	/* Insert a menu */

#define IDM_USER 100			/* reserve below 100 */
#define MAXLABELLEN 256			/* max length of menu-item label */

#define IDM_EXIT	10
#define IDM_CUT		11
#define IDM_COPY	12
#define IDM_PASTE	13
#define IDM_BREAK	14
#define IDM_FONT	15

struct menu_data;

const TCHAR *lookupMenuId(UINT id);
void 	    rlc_add_menu_bar(HWND win);
void	    rlc_menu_action(rlc_console c, struct menu_data *data);
