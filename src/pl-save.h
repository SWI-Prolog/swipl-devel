/*  $Id$

    Copyright (c) 1991 Jan Wielemaker. All rights reserved.
    jan@swi.psy.uva.nl

    Purpose: pl-save.c exports
*/

#define S_DATA		1	/* Data section */
#define S_TEXT		2	/* Incrementally loaded text */
#define S_CSTACK	3	/* C-stack section */
#define S_PLSTACK	4	/* Prolog-stack section */

#define RET_RETURN	1	/* save/1 */
#define RET_MAIN	2	/* save_program/[1,2] */

#define SAVE_FAILURE	0	/* save() failed */
#define SAVE_SAVE	1	/* save() successfully saved state */
#define SAVE_RESTORE	2	/* save() success after restore() */

#define MAX_SAVE_SECTIONS 50	/* for machines without alloca() */

typedef void * caddr;		/* anonymous address */

typedef struct save_section
{ caddr	start;			/* Start address in memory */
  long  length;			/* Length in bytes */
  long  offset;			/* Offset in the file */
  short type;			/* Which section is this? */
  short	flags;			/* Various flags */
} * SaveSection;

int	save(char *, char *, int, int, SaveSection);
int	restore(char *, int (*allocf)(SaveSection));
