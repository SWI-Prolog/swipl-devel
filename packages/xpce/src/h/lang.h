/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
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

