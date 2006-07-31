		 /*******************************
		 *	 ELEMENTARY FACES	*
		 *******************************/

%%	faces
%	_italic_ *bold* =code=

%%	faces_with_punctuation
%	_italic_, *bold* and =code=.

%%	embedded_underscores
%	See also current_prolog_flag/2.

%%	faces_with_multiword
%	_italic text_.

%%	faces_with_pred
%	*bold name/2*.

%%	dcg_rule
%	See also name/2 and eos//0.


		 /*******************************
		 *	       LISTS		*
		 *******************************/

%%	list_unordered
%
%		* Item 1
%		* Item 2

%%	list_ordered
%
%		1. Item 1
%		2. Item 2

%%	list_description
%
%		$ a : argument is an atom
%		$ w : convert argument using write

%%	nested_list
%
%		* Item 1
%			* sub item 1
%			* sub item 2
%		* Item 2
%			1. ordered item 1
%			2. ordered item 2


		 /*******************************
		 *	       TABLES		*
		 *******************************/

%%	table
%	| First Name | Jan |
%	| Last Name | Wielemaker |


		 /*******************************
		 *	      VERBATIM		*
		 *******************************/

%%	verbatim
%
%	==
%	main :-
%		current_prolog_flag(argv, AllArgv),
%		append(_, [--|Argv], AllArgv),
%		main(Argv).
%	==




		 /*******************************
		 *     COMPLETE SCHELETONS	*
		 *******************************/

%%	read_line(+In:stream, -Line:codes) is det.
%
%	Read next input line from In and return it as a list of codes.
%	
%	  * Nice item
%	  * Item 2
