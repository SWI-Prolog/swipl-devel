/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_PROCESS_H
#define _PCE_PROCESS_H

extern Any ClassProcess;
class PceProcess :public PceObject
{
public:
  PceProcess(PceArg command) :
    PceObject(ClassProcess, command)
  {
  }
  PceProcess(PceArg command, PceArg argument) :
    PceObject(ClassProcess, command, argument)
  {
  }
  PceProcess(PceArg command, PceArg argument, PceArg argument2) :
    PceObject(ClassProcess, command, argument, argument2)
  {
  }
  PceProcess(PceArg command, PceArg argument, PceArg argument2, PceArg argument3) :
    PceObject(ClassProcess, command, argument, argument2, argument3)
  {
  }
  PceProcess(PceArg command, PceArg argument, PceArg argument2, PceArg argument3, PceArg argument4) :
    PceObject(ClassProcess, command, argument, argument2, argument3, argument4)
  {
  }
  PceProcess(PceArg command, PceArg argument, PceArg argument2, PceArg argument3, PceArg argument4, PceArg argument5) :
    PceObject(ClassProcess, command, argument, argument2, argument3, argument4, argument5)
  {
  }
  PceProcess(PceArg command, PceArg argument, PceArg argument2, PceArg argument3, PceArg argument4, PceArg argument5, PceArg argument6) :
    PceObject(ClassProcess, command, argument, argument2, argument3, argument4, argument5, argument6)
  {
  }
  PceProcess(PceArg command, PceArg argument, PceArg argument2, PceArg argument3, PceArg argument4, PceArg argument5, PceArg argument6, PceArg argument7) :
    PceObject(ClassProcess, command, argument, argument2, argument3, argument4, argument5, argument6, argument7)
  {
  }
  PceProcess(PceArg command, PceArg argument, PceArg argument2, PceArg argument3, PceArg argument4, PceArg argument5, PceArg argument6, PceArg argument7, PceArg argument8) :
    PceObject(ClassProcess, command, argument, argument2, argument3, argument4, argument5, argument6, argument7, argument8)
  {
  }
};

inline PceProcess
AsProcess(PceArg a)
{ return *((PceProcess*) &a);
}

#endif /*!_PCE_PROCESS_H*/
