/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
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
