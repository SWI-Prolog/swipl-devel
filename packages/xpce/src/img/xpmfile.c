/*  $Id$

    Part of XPCE

    Author:  Jan Wielemaker and Anjo Anjewierden
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/xpce/
    Copying: GPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2001 SWI, University of Amsterdam. All rights reserved.
*/

#include <h/stream.h>

int
XpmReadStreamToData(IOSTREAM *fd, char ***data)
{ int n = 0;
  int allocated = 0;

  if ( !readXPMHeader(fd) )
    
  

}
