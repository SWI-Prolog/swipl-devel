/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

typedef struct tokeniser *		Tokeniser;
typedef struct parser *			Parser;
typedef struct operator *		Operator;

GLOBAL Class ClassTokeniser;
GLOBAL Class ClassParser;
GLOBAL Class ClassOperator;

GLOBAL Constant EndOfFile;		/* @end_of_file */

NewClass(operator)
  Name		name;			/* Name of the operator */
  Int		priority;		/* Own priority */
  Int		left_priority;		/* Allowed priority left */
  Int		right_priority;		/* Allowed priority rigth */
End;

#include <prg/proto.h>

