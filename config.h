/* 
  M_SYSTEMHOME      The SWI-Prolog path to its home. Look at your README how
                    your OS's paths are translated to SWI Prolog paths.
		    The default is "/usr/local/lib/pl"
*/

#define M_SYSTEMHOME "/staff/jan/src/pl"

/*
  M_DEFSTARTUP      The SWI Prolog path to the user's Prolog environment.
                    The default is ".plrc".
*/

#define M_DEFSTARTUP ".plrc"

/*
  M_BINDIR          The real path to the-SWI Prolog binaries.
		    The default is "/usr/local/bin"
*/

#define M_BINDIR "/staff/jan/bin/sun4"

/*
  M_PROLOG          The name for SWI-Prolog. This name is used for naming
                    saved states.
                    The default is "pl".
*/

#define M_PROLOG "pl"

		 /*******************************
		 *   NON-UNIX SYSTEM MAY NEED:	*
		 *******************************/

/* 
  M_SYSTEMDIR       The real path to M_SYSTEMHOME. This path may differ for
                    systems without Unix(tm)-style filenames.
		    The default is M_SYSTEMHOME.
		    Set this define for OS/2, TOS, Mac-OS.
*/
/*#define M_SYSTEMDIR  "C:\usr\public\SWI" */

/*
  M_SEP             Set this define to the path separator of your OS.
                    The default is "/".
*/
/*#define M_SEP       \\*/


/*
  M_SYS             The executable name for SWI-Prolog.
                    The default is M_PROLOG.
*/
/*#define M_SYS "pl.exe"*/

		 /*******************************
		 *	     DEFAULTS		*
		 *******************************/

#ifndef M_SYSTEMHOME
#    define M_SYSTEMHOME "/usr/local/lib/pl"
#endif
#ifndef M_BINDIR
#    define M_BINDIR "/usr/local/bin/pl"
#endif
#ifndef M_PROLOG
#    define M_PROLOG "pl"
#endif
#ifndef M_DEFSTARTUP
#    define M_DEFSTARTUP ".plrc"
#endif
					/* Non-Unix paths  */
#ifndef M_SYSTEMDIR
#    define M_SYSTEMDIR M_SYSTEMHOME
#endif
#ifndef M_SEP
#    define M_SEP "/"
#endif
#ifndef M_SYS
#    define M_SYS M_PROLOG
#endif
