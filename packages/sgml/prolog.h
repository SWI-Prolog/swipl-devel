/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#ifndef PROLOG_H_INCLUDED
#define PROLOG_H_INCLUDED

#define DTD2PL_VERSION "0.1, March 2000"

#define PL_PRINT_PENTITIES	0x001
#define PL_PRINT_ENTITIES	0x002
#define PL_PRINT_ELEMENTS	0x004
#define PL_PRINT_ATTRIBUTES	0x008

#define PL_PRINT_ALL (PL_PRINT_PENTITIES| \
		      PL_PRINT_ENTITIES| \
		      PL_PRINT_ELEMENTS| \
		      PL_PRINT_ATTRIBUTES)

int            prolog_print_dtd(dtd *dtd, unsigned int flags);

#endif /*PROLOG_H_INCLUDED*/
