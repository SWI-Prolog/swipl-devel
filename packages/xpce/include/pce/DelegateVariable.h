/*  $Id$

    Part of XPCE
    Designed and implemented by Anjo Anjewierden and Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1993 University of Amsterdam. All rights reserved.
*/

#ifndef _PCE_DELEGATEVARIABLE_H
#define _PCE_DELEGATEVARIABLE_H

extern Any ClassDelegateVariable;
class PceDelegateVariable :public PceObject
{
public:
  PceDelegateVariable(PceArg name) :
    PceObject(ClassDelegateVariable, name)
  {
  }
  PceDelegateVariable(PceArg name, PceArg type) :
    PceObject(ClassDelegateVariable, name, type)
  {
  }
  PceDelegateVariable(PceArg name, PceArg type, PceArg access) :
    PceObject(ClassDelegateVariable, name, type, access)
  {
  }
  PceDelegateVariable(PceArg name, PceArg type, PceArg access, PceArg wrapper) :
    PceObject(ClassDelegateVariable, name, type, access, wrapper)
  {
  }
  PceDelegateVariable(PceArg name, PceArg type, PceArg access, PceArg wrapper, PceArg summary) :
    PceObject(ClassDelegateVariable, name, type, access, wrapper, summary)
  {
  }
  PceDelegateVariable(PceArg name, PceArg type, PceArg access, PceArg wrapper, PceArg summary, PceArg group) :
    PceObject(ClassDelegateVariable, name, type, access, wrapper, summary, group)
  {
  }
};

inline PceDelegateVariable
AsDelegateVariable(PceArg a)
{ return *((PceDelegateVariable*) &a);
}

#endif /*!_PCE_DELEGATEVARIABLE_H*/
