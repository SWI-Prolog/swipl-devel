/*  $Id$

    Part of XPCE --- The SWI-Prolog GUI toolkit

    Author:        Jan Wielemaker and Anjo Anjewierden
    E-mail:        jan@swi.psy.uva.nl
    WWW:           http://www.swi.psy.uva.nl/projects/xpce/
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

void	r_msarc(int x, int y, int w, int h,
		int sx, int sy,
		int ex, int ey,
		Name close,
		Any fill);
void	ws_check_intr(void);
int	ws_free_file_descriptors(void);
int	ws_getpid(void);
void	ws_initialise(int argc, char **argv);
void	ws_sleep(int time);
void	ws_invalidate_window(PceWindow sw, Area a);
void	ws_scroll_window(PceWindow sw, int dx, int dy);
void	ws_redraw_window(PceWindow sw, IArea a, int clear);
int	ws_emulate_three_buttons(int time);
int	ws_mousebuttons(void);
void	ws_kill_process(Process p, int sig);
int	iswin32s(void);
char *	ws_os(void);
Name	WinStrError(int error, ...);
status	makeClassWinMF(Class class);
status	makeClassWinPrinter(Class class);
void	ws_frame_border(FrameObj fr, int *xb, int *yb, int *ycap);
