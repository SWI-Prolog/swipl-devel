/*  $Id$

    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 1994 University of Amsterdam. All rights reserved.
*/

#if __GNUC__ == 2

main(int argc, char **argv)
{ int dummy;

  bar:
    asm("nop");
  foo:
    dummy++;

  if ( &&bar == &&foo )
    exit(1);

  exit(0);
}

#else

main(int argc, char **argv)
{ exit(1);
}

#endif
