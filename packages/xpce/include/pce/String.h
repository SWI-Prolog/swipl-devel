/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_STRING_H
#define _PCE_STRING_H

extern "C" {
Any	XPCE_to_string(const char *text);
Any	XPCE_to_tmp_char_array(const char *text);
void	XPCE_done_tmp_char_array(Any ca);
}

extern Any ClassString;
class PceString :public PceObject
{
public:
  PceString() :
    PceObject(ClassString)
  {
  }
  PceString(char *text) 		/* text already done by C(++) */
  { self = XPCE_to_string(text);
  }
  PceString(char *text, PceArg a)
  { Any av[2] = { XPCE_to_tmp_char_array(text), a.self };
    self = XPCE_newv(ClassString, NULL, 2, av);
    XPCE_done_tmp_char_array(av[0]);
  }
  PceString(PceArg format, PceArg a, PceArg a2) :
    PceObject(ClassString, format, a, a2)
  {
  }
  PceString(PceArg format, PceArg a, PceArg a2, PceArg a3) :
    PceObject(ClassString, format, a, a2, a3)
  {
  }
  PceString(PceArg format, PceArg a, PceArg a2, PceArg a3, PceArg a4) :
    PceObject(ClassString, format, a, a2, a3, a4)
  {
  }
  PceString(PceArg format, PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5) :
    PceObject(ClassString, format, a, a2, a3, a4, a5)
  {
  }
  PceString(PceArg format, PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5, PceArg a6) :
    PceObject(ClassString, format, a, a2, a3, a4, a5, a6)
  {
  }
  PceString(PceArg format, PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5, PceArg a6, PceArg a7) :
    PceObject(ClassString, format, a, a2, a3, a4, a5, a6, a7)
  {
  }
  PceString(PceArg format, PceArg a, PceArg a2, PceArg a3, PceArg a4, PceArg a5, PceArg a6, PceArg a7, PceArg a8) :
    PceObject(ClassString, format, a, a2, a3, a4, a5, a6, a7, a8)
  {
  }
};


PceString
AsString(PceArg a)
{ return *(PceString*) &a;
}

#endif /*!_PCE_STRING_H*/
