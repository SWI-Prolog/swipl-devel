		 /*******************************
		 *	 ELEMENTARY FACES	*
		 *******************************/

%%	Faces _italic_ *bold* =code=

%%	Faces with punctuation _italic_, *bold* and =code=.

%%	Faces with multiword: _italic text_.

%%	Faces with pred: *bold name/2*.

%%	See also name/2 and eos//0.

		 /*******************************
		 *	       LISTS		*
		 *******************************/

%%	A simple unordered list
%
%		* Item 1
%		* Item 2

%%	A simple ordered list
%
%		1. Item 1
%		2. Item 2

%%	A description
%
%		$ a : argument is an atom
%		$ w : convert argument using write

%%	A nested list
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

%%	| First Name | Jan |
%	| Last Name | Wielemaker |


		 /*******************************
		 *     COMPLETE SCHELETONS	*
		 *******************************/

%%	read_line(+In:stream, -Line:codes) is det.
%
%	Read next input line from In and return it as a list of codes.
%	
%	  * Nice item
%	  * Item 2
