			     Extending XPCE


You may want to add new classes to the base system.  If the class can be
expressed in terms of existing classes   or  the combination of existing
classes with features provided by the host-language (Prolog, Lisp, C++),
the best way to define the new functionality   is  to use the XPCE class
definition protocol defined for your host-language.

If you want to add new features  that   need  to be very fast or require
access to the operating system at a level   nor  provided by XPCE or the
host-language, you have the choice between   using  the C++ interface or
defining a new class  in  C.   The   C++  interface  is  defined  in
``A C++ interface for XPCE'', available from

	swi.psy.uva.nl/pub/xpce/doc/xpce/doc/C++/pce-C++.ps.gz

or from man/C++/* (LaTeX source).

To be able to define a new class  (or   method)  in C, you need the full
source-tree of XPCE.  It is important to   note  that we DO NOT GUARANTEE
THE INTERFACE TO REMAIN THE SAME.  If you have additions, the best is to
write them in cooperation with us, so we   can add it to the base-system
and maintain it.

Steps to add a new C-class:
===========================

	1) Determine a name for the class.  The class should be defined
	in a file with the same name, or a descriptive name if the
	classname consists of symbols rather than letters and digits.

	Determine the category the new file should be in and copy a
	simple class into the new file.  I often take adt/attribute.c.
	Lets assume our new class is called `rpc' (Remote Procedure
	Call).  Then this becomes:

		% cp adt/attribute.c unx/rpc.c

	2) Edit unx/rpc.c.  First change all `attribute' in the various
	capitalisations into rpc with the corresponding capitalisation.

		2a) Define the class structure.  If the class is a
		subclass of class object, this becomes:

		NewClass(rpc)
		  Name	function;	/* Function to be called */
		  ...
		End;

		If the class is not a subclass of object, the
		super-class should define a macro ABSTRACT_<classname>
		to inherit the super attributes, and the definition
		will look like this:

		NewClass(my_window)
		    ABSTRACT_WINDOW
		    Any		my_slot;
		End;

		Types of attributes are XPCE types.  See h/types.h for
		type-names defined.

		You may add private C handles, but remember that each
		slot should have the size of a pointer (normally 32
		bits).

		The NewClass() definition may be in a file in ../h/
		or be in the sourcefile of the class.  The first is
		only required you want to have direct access to the
		structure attributes from other classes.

		2b) Define the pointer type, which is a capitalised
		version of the structure name.  The definition may
		be in .../h/types.h or in the sourcefile itself.

		2c) Define a global variable of type Class with name
		Class<CapitalisedClassName>.  The definition again may
		be in .../h/types.h or the local file.

		2d) Update the makeClassRpc() function for the correct
		attribute definitions.  There should be the same number
		of localClass() definitions as there are slots.
		Non-XPCE slots should be tagged alien:<Ctype> for their
		type.

		2c) Update the termClass() definition and the
		declaration for sendMethod(class, NAME_initialise, ...
		as well as the initialiseRpc() function.
		
		2d) Define the real methods.  See the various examples
		for details.

	3) Make the class known to the system:

		3a) Add a declaration to ker/declarations.c
		
		3b) Add the file to the local Makefile
		
		3c) Either run `make proto' (if you have mkproto(1))
		or add the makeClassRpc() prototype to the local proto.h
		file.

	4) Make atoms known to the system:

		run ./FN unx/rpc.c from the src directory

	5) Run make from the main xpce directory to a build a new image
	   holding the new class.  Check the class definition with the
	   online manual.  You can also write the documentation for the
	   class by switching the manual into edit mode using
	   File/Edit Mode.


Tips and Hints for writing C-classes:
=====================================

1. First just write the data definition and go through the steps above.
   verify the data-definition is ok (using the online manual), create
   and instance and run checkpce/0 to if XPCE thinks the instance is
   fine.

2. Consider prototyping (part of) the class using Prolog-defined methods
   (pce_extend_class/pce_end_class).  The development cycle is much shorter!

3. When using XPCE/SWI-Prolog, debugging using gdb works like this: goto
   the src directory.  Do:

	% ln -s ../pl/src/xpce.base .
	% gdb xpce.base
	(gdb) r -r ../bin/xpce
	?- <xpce goals to debug>

   On some machines this poses problems as restoring the saved state under
   gdb does not work or the result cannot be traced.  In this case try:

        % ln -s ../pl/src/xpce.base .
	% gdb xpce.base
	(gdb) r
	?- [xpce].
	?- <xpce goals to debug>

   Another hint: when trapped in the gdb debugger, you can do

	(gdb) call pl_break()
	Break Level [1]
	[1] 2 ?- <prolog goals to examine the status>
	[1] n ?- ^D
	(gdb)

   Normally only recompile the classes you suspect using -g as recompiling
   all will make the linking process very slow.  Using the standard setup
   you can compile for debugging using:

	% make COFLAGS=-g

   For other Prologs many of these tricks will apply too.

4. Integers are tagged types in XPCE.  To turn a C int into its XPCE 
   equivalent, using toInt(x).  valInt(x) performs the other operation.
   Note that valInt(valInt(x)) returns nonsense.  So does toInt(toInt(x)).

5. Assignment to an instance slot may *never* be done using the C structure
   assignment ptr->slot = value.  Instead, one should use

	assign(ptr, slot, value)

   which maintains reference counts for the garbage collector and allows
   for tracing the slot.

6. Checkpce/0 and running the system in debugging mode will help you
   locating trouble.  The latter is achieved using:

	?- send(@pce, trace, user).

   It will cause all assignments to be type-checked.

7. If you need debug statements, the following macro helps:

	DEBUG(NAME_<topic>, <C-code>)

   you can activate the C-code there using
   
        ?- debugpce(<topic>).

   The macros pp(x) (pretty-print) prints any XPCE data structure in a human
   readable format.  An example could be:

	DEBUG(NAME_rpc, printf("calling to %s\n", pp(dest)));

   where `dest' is a local variable that should hold XPCE data.  pp(x) is
   very careful and generally prints non-XPCE values hexadecimal, but
   occasionally crashes when passed non-XPCE values.

8. Use the ANSI assert() macro.  The XPCE definition is changed a bit
   such that a failing assertion will generate a fatal error calling 
   sysPce().  Put a gdb breakpoint on this function if failing asserts
   need to be analysed using gdb.

9. Make the ->unlink method fool proof: do not expect the instance to be
   in a consistent state and be prepared to be called more than once.

