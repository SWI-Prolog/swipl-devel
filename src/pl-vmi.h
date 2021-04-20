/*  File: pl-vmi.h

    This file provides the definition of type code.
*/

typedef enum
{
#define _VMI(Name,...) Name,
#include "pl-vmi.ih"
  VMI_END_LIST
} vmi;

#define I_HIGHEST ((int)VMI_END_LIST)
/* VM_SIGNATURE is defined in pl-vmi.h */