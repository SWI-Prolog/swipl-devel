/*  $Id$

    Part of SWI-Prolog SGML/XML parser

    Author:  Jan Wielemaker
    E-mail:  jan@swi.psy.uva.nl
    WWW:     http://www.swi.psy.uva.nl/projects/SWI-Prolog/
    Copying: LGPL-2.  See the file COPYING or http://www.gnu.org

    Copyright (C) 1990-2000 SWI, University of Amsterdam. All rights reserved.
*/

#include "utf8.h"
#include <stdio.h>

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
UTF-8 Decoding, based on http://www.cl.cam.ac.uk/~mgk25/unicode.html
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

#define CONT(i) ((in[i]&0xc0) == 0x80)
#define VAL(i, s) ((in[i]&0x3f) << s)

char *
__utf8_get_char(const char *in, int *chr)
{ 					/* 2-byte, 0x80-0x7ff */
  if ( (in[0]&0xe0) == 0xc0 && CONT(1) )
  { *chr = ((in[0]&0x1f) << 6)|VAL(1,0);
    return (char *)in+2;
  }
					/* 3-byte, 0x800-0xffff */
  if ( (in[0]&0xf0) == 0xe0 && CONT(1) && CONT(2) )
  { *chr = ((in[0]&0xf) << 12)|VAL(1,6)|VAL(2,0);
    return (char *)in+3;
  }
					/* 4-byte, 0x10000-0x1FFFFF */
  if ( (in[0]&0xf8) == 0xf0 && CONT(1) && CONT(2) && CONT(3) )
  { *chr = ((in[0]&0x7) << 18)|VAL(1,12)|VAL(2,6)|VAL(3,0);
    return (char *)in+4;
  }
					/* 5-byte, 0x200000-0x3FFFFFF */
  if ( (in[0]&0xfc) == 0xf8 && CONT(1) && CONT(2) && CONT(3) && CONT(4) )
  { *chr = ((in[0]&0x3) << 24)|VAL(1,18)|VAL(2,12)|VAL(3,6)|VAL(4,0);
    return (char *)in+5;
  }
					/* 6-byte, 0x400000-0x7FFFFFF */
  if ( (in[0]&0xf8) == 0xf0 && CONT(1) && CONT(2) && CONT(3) && CONT(4) && CONT(5) )
  { *chr = ((in[0]&0x1) << 30)|VAL(1,24)|VAL(2,18)|VAL(3,12)|VAL(4,6)|VAL(5,0);
    return (char *)in+4;
  }

  *chr = *in;
/*fprintf(stderr, "UTF-8 decoder: bad sequence\n");*/
  
  return (char *)in+1;
}
