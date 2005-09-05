Issues:

* Handle them through H_INDIRECT/B_INDIRECT		DONE
	* Add A_MPZ for optimised arithmetic
		- Compilation				DONE
		- Emulator				DONE
		- Decompiler				DONE
	* Handle them in computing index key from argument
		- Now only first 64 bits.  Good enough?
	* recorded database				DONE
	* Handle them in .wic
		- use standard byte-order through mpz_import/export
	* Handle writing
		- write_term/3, etc.			DONE
		- number_codes/2			DONE
		- atom_number/2				DONE
		- format/2
			~D				DONE
			~r				DONE
	* Handle reading				DONE
	* Flags bounded, max_integer, min_integer	DONE
	* Deal with cleanup of allocated resources	TEST
	* Deal with optimised arithmetic		TEST
	* Overflow checking in conversion to float
	* Documentation					DONE
	* Windows built
	* Test scripts
	* Add --without-gmp to configure		DONE
	* Deal with floats


Functions:

	+/2			DONE
	-/2			DONE
	-/1			DONE
	*/2			DONE
	//2			DONE
	///2			DONE
	mod/2			DONE
	min/2			DONE
	max/2			DONE
	sign/1			DONE
	float_fractional_part/1 DONE
	ar_float_integer_part/1	DONE
	ar_truncate/1		DONE
	ceil/1			DONE
	floor/1			DONE
	\//2			DONE
	/\/2			DONE
	^/2			DONE
	>>/2			DONE
	<</2			DONE
	\/1			DONE
	msb/1			DONE
	abs/1			DONE
	float/1			DONE
	integer/1		DONE
	**/2			DONE
	rem/2			DONE

Predicates:

	=:=, <, =<, >, >=, =\=	DONE
	==, @<. >@, compare/3	DONE
	=@=			DONE
	between/3
	plus/3
	succ/2

Foreign code handling:

	PL_get_nchars(), etc	DONE
	PL_get_term_value()

Internal functions:

	do_compare()		DONE
	valInteger()		CHECKED usage

================================================================
Rationals:

	rdiv/2

 * If evalExpression finds rdiv(X, Y) create MPQ
 * Ordering: int --> mpz --> mpq --> real

printing ~<n>f:

	* format MPQ
		~f			DONE
		~g			DONE
		~e			DONE
	* Document new format behaviour	DONE


		 /*******************************
		 *	      WINDOWS		*
		 *******************************/


	* http://developer.berlios.de/projects/win32gmp/
		- Static library
		- Big (2.5MB, adds 800 KB to Prolog dll) 

	* http://na-inet.jp/na/bnc/wingmp.html

	* http://fp.gladman.plus.com/computing/gmp4win.htm
		- Brian Gladman's build instructions for GMP on
		  MSVC
