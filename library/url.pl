/*  $Id$

    Part of SWI-Prolog
    Designed and implemented by Jan Wielemaker
    E-mail: jan@swi.psy.uva.nl

    Copyright (C) 2000 University of Amsterdam.
    Licence: GPL
*/

:- module(url,
	  [ parse_url/2,		% +URL, -Parts
	    parse_url/3,		% +BaseURL, +URL|URI, -Parts
	    global_url/3,		% +Local, +Base, -Global
	    http_location/2		% +Parts, -Location
	  ]).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Utility library to break down URL  specifications. This library is based
on RFC-1738, specifying  the  URL  syntax   with  a  few  commonly  used
extensions:

	* Allow for ~ character in path-names.
	* Assuming http as default protocol.
	* Allow for file:<path>

Public interface:

parse_url(+URL, -Parts)
    Parse a URL into its parts.  Parts is a list of Name(Value).  Attributes
    depend on the protocol.  Currently the implementation is not defined
    for all protocols.  Defined:

        protocol	All protocols
	host		Network based protocols
	port		Network based protocols
	path		file,http,ftp
	search		http

    For example:

    ?- parse_url('http://swi.psy.uva.nl/hello?world', P).

    P = [ protocol(http),
	  search(world),
	  path('/hello'),
	  host('swi.psy.uva.nl')
	] 

parse_url(+URL, +BaseURL, -Parts)
    As above, but `URL' may be relative to some base url.  If URL is
    absolute, BaseURL is ignored.

global_url(+URL, +BaseURL, +AbsoluteUrl)
    Transform a possible local URL into a global one.

http_location(+Parts, -Location)
    If `Parts' is the part-list of an http URL, construct a string for
    the location.  So, to access a page with an HTTP url, do:

    	parse_url(URL, Parts),
	memberchk(host(Host), Parts),
	(   memberchk(port(Port), Parts)
	->  true
	;   Port = 80
	),
	<connect to host and port>
	http_location(Parts, Location),
	format(Stream, 'GET ~w HTTP/1.0\r\nHost: ~w\r\n\r\n',
	       [ Location, Host ]),
	...
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


		 /*******************************
		 *	      GLOBALISE		*
		 *******************************/

global_url(URL, BaseURL, Global) :-
	parse_url(BaseURL, URL, Attributes),
	phrase(curl(Attributes), Chars),
	atom_codes(Global, Chars).


		 /*******************************
		 *	  CREATE URL/URI	*
		 *******************************/

%	http_location(+Parts, -Location)
%
%	Translate the relevant parts of an URL into an HTTP location
%	on a server.

http_location(Parts, Location) :-
	phrase(curi(Parts), String), !,
	atom_codes(Location, String).


curl(A0) -->
	{ select(A0, protocol(Protocol), A1)
	},
	catomic(Protocol),
	":",
	curl(Protocol, A1).

curl(file, A) -->
	cpart(path, "", A).
curl(http, A) -->
	"//",
	cpart(host, "", A),
	cpart(port, ":", A),
	cpart(path, "", A),
	cpart(search, "?", A).

curi(A) -->
	cpart(path, "", A),
	cpart(search, "?", A).

cpart(Field, Sep, A) -->
	{ Term =.. [Field, Value],
	  memberchk(Term, A)
	}, !,
	Sep,
	catomic(Value).
cpart(_,_,_) -->
	[].

catomic(A, In, Out) :-
	atom_codes(A, Codes),
	append(Codes, Out, In).
		
		 /*******************************
		 *	      PARSING		*
		 *******************************/

%	parse_url(+URL, [+BaseURL], -Attributes)
%
%	Parse a URL to a sequence of attributes.  In the version with
%	three arguments, URL may be an URL relative to BaseURL.
%
%	AttributesL
%
%		protocol(Protocol)	Protocol identifier
%		host(Host)		Name of host
%		port(Port)		Number of port to contact
%		path(Path)		The path
%		search(Search)		Search specification

parse_url(URL, Attributes) :-
	nonvar(URL), !,
	atom_codes(URL, Codes),
	phrase(url(Attributes), Codes).
parse_url(URL, Attributes) :-
	phrase(curl(Attributes), Codes), !,
	atom_codes(URL, Codes).


parse_url(URL, BaseURL, Attributes) :-
	atom_codes(URL, Codes),
	(   phrase(absolute_url, Codes, _)
	->  phrase(url(Attributes), Codes)
	;   parse_url(BaseURL, BaseA0),
	    memberchk(protocol(Protocol), BaseA0),
	    select(BaseA0, path(BasePath), BaseA1),
	    delete(BaseA1, search(_), BaseA),
	    phrase(uri(Protocol, URIA0), Codes),
	    select(URIA0, path(LocalPath), URIA1),
	    globalise_path(LocalPath, BasePath, Path),
	    append(BaseA, [path(Path)|URIA1], Attributes)
	).
	
globalise_path(LocalPath, _, LocalPath) :-
	sub_atom(LocalPath, 0, _, _, /), !.
globalise_path(LocalPath, BasePath, Path) :-
	(   sub_atom(BasePath, _, _, 0, /)
	->  BaseDir = BasePath
	;   file_directory_name(BasePath, BaseDir0),
	    atom_concat(BaseDir0, /, BaseDir)
	),
	atom_concat(BaseDir, LocalPath, Path).

absolute_url -->
	identifier(_),
	":", !.

url(A) -->
	case_insensitive("url:"), !,
	url(A).
url([protocol(Protocol)|A]) -->
	identifier(Protocol),
	":", !,
	url(Protocol, A).
url([protocol(http)|A]) -->
	implicit_http(A).

url(file, [path(Path)]) -->
	(   "/"
	->  path(Path0),
	    { atom_concat(/, Path0, Path)
	    }
	;   path(Path0),
	    { absolute_file_name(Path0, Path)
	    }
	).

url(http, Attributes) --> !,
	"//",
	implicit_http(Attributes).

implicit_http(Attributes) -->
	hostport(A0),
	(   "/"
	->  path(Path0),
	    { atom_concat(/, Path0, Path),
	      A1 = [path(Path)|A0]
	    }
	;   { A1 = [path(/)|A0]		% assume / if no path is specified
	    }
	),
	(   "?"
	->  search(Search),
	    { A2 = [search(Search)|A1]
	    }
	;   { A2 = A1
	    }
	),
	{ Attributes = A2
	}.

%	uri(+Protocol, -Attributes)
%
%	resolve attributes of local URL

uri(http, [path(Path)|A]) -->
	path(Path),
	(   "?"
	->  search(Search),
	    { A = [search(Search)]
	    }
	;   { A = []
	    }
	).
uri(file, [path(Path)]) -->
	path(Path).


		 /*******************************
		 *	    URL ELEMENTS	*
		 *******************************/

hostport([host(Host)|A]) -->
	host(Host),
	(   ":"
	->  integer(Port),
	    { A = [port(Port)]
	    }
	;   { A = []
	    }
	).

host(H) -->
	hostname(H), !.
host(H) -->
	hostnumber(H).

hostname(Host) -->
	ialpha(Part1),
	(   "."
	->  hostname(Domain),
	    { concat_atom([Part1, '.', Domain], Host)
	    }
	;   { Host = Part1
	    }
	).

hostnumber(Host) -->
	integer(A),
	".",
	integer(B),
	".",
	integer(C),
	".",
	integer(D),
	{ concat_atom([A,B,C,D], '.', Host)
	}.
%hostnumber(ip(A,B,C,D)) -->		% Might be cleaner
%	integer(A),
%	".",
%	integer(B),
%	".",
%	integer(C),
%	".",
%	integer(D).

path(Path) -->
	segment(S0), !,
	(   "/"
	->  path(R),
	    { concat_atom([S0, /, R], Path)
	    }
	;   { Path = S0
	    }
	).
path('') -->
	[].

segment(Segment) -->
	xalphas(Chars), !,
	{ atom_codes(Segment, Chars)
	}.

search(Search) -->
	xalphas(Part1Chars),
	{ atom_codes(Part1, Part1Chars)
	},
	(   "+"
	->  search(Rest),
	    { concat_atom([Part1, +, Rest], Search)
	    }
	;   { Search = Part1
	    }
	).

		 /*******************************
		 *	       BASICS		*
		 *******************************/

integer(I) -->
	digit(D0),
	digits(D),
	{ number_chars(I, [D0|D])
	}.

		 /*******************************
		 *	     SEQUENCES		*
		 *******************************/

digits([C|T]) -->
	digit(C), !, 
	digits(T).
digits([]) -->
	[].

xalphas([C|T]) -->
	xalpha(C), !, 
	xalphas(T).
xalphas([]) -->
	[].

ialpha(Atom) -->
	alpha(C0),
	xalphas(S),
	{ atom_codes(Atom, [C0|S])
	}.

		 /*******************************
		 *	  URL DCG BASICS	*
		 *******************************/

digit(C, [C|T], T) :- code_type(C, digit).
alpha(C, [C|T], T) :- code_type(C, alpha).
alphanum(C, [C|T], T) :- code_type(C, alnum).
safe(C, [C|T], T) :- safe(C).
extra(C, [C|T], T) :- extra(C).
reserved(C, [C|T], T) :- reserved(C).
hex(C, [C|T], T) :-  code_type(C, xdigit(_)).
national(C, [C|T], T) :- national(C).
escape(C) -->
	"%",
	[C1, C2],
	{ code_type(C1, xdigit(D1)),
	  code_type(C2, xdigit(D2)),
	  C is 16*D1 + D2
	}.

xalpha(C, [C|T], T) :-
	(   code_type(C, alnum)		% alpha | digit
	;   safe(C)
	;   extra(C)
	), !.
xalpha(C) -->
	escape(C).

safe(0'$).
safe(0'-).
safe(0'_).
safe(0'@).
safe(0'.).
safe(0'&).
safe(0'+).
safe(0'~).				% JW: not official URL

extra(0'!).
extra(0'*).
extra(0'").
extra(0'').
extra(0'().
extra(0')).
extra(0',).

reserved(0'=).
reserved(0';).
reserved(0'/).
reserved(0'#).
reserved(0'?).
reserved(0':).
reserved(0' ).

xdigit(0'a).
xdigit(0'b).
xdigit(0'c).
xdigit(0'd).
xdigit(0'e).
xdigit(0'f).

xdigit(0'A).
xdigit(0'B).
xdigit(0'C).
xdigit(0'D).
xdigit(0'E).
xdigit(0'F).

national(0'{).
national(0'}).
national(0'[).
national(0']).
national(0'\).
national(0'^).
national(0'~).

		 /*******************************
		 *	     DCG BASICS		*
		 *******************************/

%	case_insensitive(+String)
%
%	match a string case insensitive.

case_insensitive([]) -->
	[].
case_insensitive([H|T]) -->
	[C],
	{ code_type(H, to_lower(C))
	},
	case_insensitive(T).


%	identifier(-Id)
%
%	Match input string holding csym characters to an identifier

identifier(Id) -->
	chars(alpha, Chars),
	{ name(Id, Chars)
	}.

%	chars(+Type, -Chars)
%
%	Get as many as possible	characters of this type

chars(Type, [C0|C]) -->
	[C0],
	{ code_type(C0, Type)
	}, !,
	chars(Type, C).
chars(_, []) -->
	[].

