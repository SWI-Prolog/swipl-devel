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

typedef struct
{ unsigned char	width;
  unsigned char height;
  char		hot_x;
  char		hot_y;
  long		offset;
} cursor_glyph, *CursorGlyph;


typedef struct
{ int		magic;			/* magic number */
  int		entries;		/* # entries in file */
  int		offset;			/* offset of entries */
  long		dsize;			/* Data size (for bits) in bytes */
} cursor_glyph_file_header, *CursorGlyphFileHeader;

#define XPCE_CURSOR_FILE_MAGIC 0x626482	/* nice number */
