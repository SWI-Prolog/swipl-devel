/*  $Id$

    Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (C): 1985-2009, University of Amsterdam

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(url,
	  [ parse_url/2,		% +URL, -Parts | -URL +Parts
	    parse_url/3,		% +URL|URI, +BaseURL, -Parts
					% -URL, +BaseURL, +Parts
	    is_absolute_url/1,		% +URL
	    global_url/3,		% +Local, +Base, -Global
	    http_location/2,		% ?Parts, ?Location
	    www_form_encode/2,		% Value <-> Encoded
	    parse_url_search/2,		% Form-data <-> Form fields

	    url_iri/2,			% ?URL, ?IRI

	    file_name_to_url/2,		% ?FileName, ?URL

	    set_url_encoding/2		% ?Old, +New
	  ]).
:- use_module(library(lists)).
:- use_module(library(error)).
:- use_module(library(utf8)).

/** <module> Analysing and constructing URL

This library deals with the analysis and construction of a URL,
Universal Resource Locator. URL is the basis for communicating locations
of resources (data) on the web. A URL consists of a protocol identifier
(e.g. HTTP, FTP, and a protocol-specific syntax further defining the
location. URLs are standardized in RFC-1738.

The implementation in this library covers only a small portion of the
defined protocols.  Though the initial implementation followed RFC-1738
strictly, the current is more relaxed to deal with frequent violations
of the standard encountered in practical use.

@author	Jan Wielemaker
@author Lukas Faulstich
@deprecated New code should use library(uri), provided by the =clib=
	    package.
*/

		 /*******************************
		 *	      GLOBALISE		*
		 *******************************/

%%	global_url(+URL, +Base, -Global) is det.
%
%	Translate a possibly relative URL  into   an  absolute  one.
%
%	@error syntax_error(illegal_url) if URL is not legal.

global_url(URL, BaseURL, Global) :-
	(   is_absolute_url(URL),
	    \+ sub_atom(URL, _, _, _, '%')	% may have escape, use general
	->  Global = URL
	;   sub_atom(URL, 0, _, _, '//')
	->  parse_url(BaseURL, [], Attributes),
	    memberchk(protocol(Proto), Attributes),
	    atomic_list_concat([Proto, :, URL], Global)
	;   sub_atom(URL, 0, _, _, #)
	->  (   sub_atom(BaseURL, _, _, 0, #)
	    ->	sub_atom(URL, 1, _, 0, NoHash),
		atom_concat(BaseURL, NoHash, Global)
	    ;	atom_concat(BaseURL, URL, Global)
	    )
	;   parse_url(URL, BaseURL, Attributes)
	->  phrase(curl(Attributes), Chars),
	    atom_codes(Global, Chars)
	;   throw(error(syntax_error(illegal_url), URL))
	).

%%	is_absolute_url(+URL)
%
%	True if URL is an absolute URL. That  is, a URL that starts with
%	a protocol identifier.

is_absolute_url(URL) :-
	sub_atom(URL, 0, _, _, 'http://'), !.
is_absolute_url(URL) :-
	sub_atom(URL, 0, _, _, 'https://'), !.
is_absolute_url(URL) :-
	sub_atom(URL, 0, _, _, 'ftp://'), !.
is_absolute_url(URL) :-
	sub_atom(URL, 0, _, _, 'file://'), !.
is_absolute_url(URL) :-
	atom_codes(URL, Codes),
	phrase(absolute_url, Codes, _), !.


		 /*******************************
		 *	  CREATE URL/URI	*
		 *******************************/

%%	http_location(?Parts, ?Location)
%
%	Construct or analyze an  HTTP  location.   This  is  similar  to
%	parse_url/2, but only deals with the   location  part of an HTTP
%	URL. That is, the path, search   and fragment specifiers. In the
%	HTTP protocol, the first line of a message is
%
%	    ==
%	    <Action> <Location> HTTP/<version>
%	    ==
%
%	@param Location	Atom or list of character codes.

http_location(Parts, Location) :-	% Parts --> Location
	nonvar(Parts), !,
	phrase(curi(Parts), String), !,
	atom_codes(Location, String).
http_location(Parts, Location) :-	% Location --> Parts
	atom(Location), !,
	atom_codes(Location, Codes),
	phrase(http_location(Parts), Codes).
http_location(Parts, Codes) :-		% LocationCodes --> Parts
	is_list(Codes),
	phrase(http_location(Parts), Codes).


curl(A) -->
	{ memberchk(protocol(Protocol), A)
	}, !,
	catomic(Protocol),
	":",
	curl(Protocol, A).
curl(A) -->
	curl(http, A).

curl(file, A) --> !,
	(   "//"
	->  cpath(A)
	;   cpath(A)
	).
curl(_, A) -->
	"//",
	cuser(A),
	chost(A),
	cport(A),
	cpath(A),
	csearch(A),
	cfragment(A).

curi(A) -->
	cpath(A),
	csearch(A).

cpath(A) -->
	(   { memberchk(path(Path), A) }
	->  { atom_codes(Path, Codes) },
	    www_encode(Codes, "/+:,")
	;   ""
	).

cuser(A) -->
	(   { memberchk(user(User), A) }
	->  { atom_codes(User, Codes) },
	    www_encode(Codes, ":"),
	    "@"
	;   ""
	).

chost(A) -->
	(   { memberchk(host(Host), A) }
	->  { atom_codes(Host, Codes) },
	    www_encode(Codes, "")
	;   ""
	).

cport(A) -->
	(   { memberchk(port(Port), A), Port \== 80 }
	->  { number_codes(Port, Codes) },
	    ":",
	    www_encode(Codes, "")
	;   ""
	).


catomic(A, In, Out) :-
	atom_codes(A, Codes),
	append(Codes, Out, In).

%%	csearch(+Attributes)//

csearch(A)-->
	(   { memberchk(search(Parameters), A) }
	->  csearch(Parameters, "?")
	;   []
	).

csearch([], _) -->
	[].
csearch([Parameter|Parameters], Sep) --> !,
	codes(Sep),
	cparam(Parameter),
	csearch(Parameters, "&").

cparam(Name=Value) --> !,
	cname(Name),
	"=",
	cvalue(Value).
cparam(NameValue) -->			% allow to feed Name(Value)
	{ compound(NameValue), !,
	  NameValue =.. [Name,Value]
	},
	cname(Name),
	"=",
	cvalue(Value).
cparam(Name)-->
	cname(Name).

codes([]) --> [].
codes([H|T]) --> [H], codes(T).

cname(Atom) -->
	{ atom_codes(Atom, Codes) },
	www_encode(Codes, "").

%%	cvalue(+Value)// is det.
%
%	Construct a string from  Value.  Value   is  either  atomic or a
%	code-list.

cvalue(Value) -->
	{ atomic(Value), !,
	  atom_codes(Value, Codes)
	},
	www_encode(Codes, "").
cvalue(Codes) -->
	{ must_be(codes, Codes)
	},
	www_encode(Codes, "").


%%	cfragment(+Attributes)//

cfragment(A) -->
	{ memberchk(fragment(Frag), A), !,
	  atom_codes(Frag, Codes)
	},
	"#",
	www_encode(Codes, "").
cfragment(_) -->
	"".


		 /*******************************
		 *	      PARSING		*
		 *******************************/

%%	parse_url(+URL, -Attributes) is det.
%
%	Construct or analyse a URL. URL is an   atom  holding a URL or a
%	variable. Attributes is a list of  components. Each component is
%	of the format Name(Value). Defined components are:
%
%	    * protocol(Protocol)
%	    The used protocol. This is, after  the optional =|url:|=, an
%	    identifier separated from the remainder of  the URL using :.
%	    parse_url/2 assumes the =http= protocol   if  no protocol is
%	    specified and the URL can be parsed  as a valid HTTP url. In
%	    addition to the RFC-1738  specified   protocols,  the =file=
%	    protocol is supported as well.
%
%	    * host(Host)
%	    Host-name or IP-address on which   the  resource is located.
%	    Supported by all network-based protocols.
%
%	    * port(Port)
%	    Integer port-number to access on   the \arg{Host}. This only
%	    appears if the port is  explicitly   specified  in  the URL.
%	    Implicit default ports (e.g., 80 for   HTTP) do _not_ appear
%	    in the part-list.
%
%	    * path(Path)
%	    (File-) path addressed by the URL. This is supported for the
%	    =ftp=, =http= and =file= protocols. If  no path appears, the
%	    library generates the path =|/|=.
%
%	    * search(ListOfNameValue)
%	    Search-specification of HTTP URL. This is the part after the
%	    =|?|=, normally used to transfer data   from HTML forms that
%	    use the =GET=  protocol.  In  the   URL  it  consists  of  a
%	    www-form-encoded list of Name=Value pairs. This is mapped to
%	    a list of Prolog Name=Value  terms   with  decoded names and
%	    values.
%
%	    * fragment(Fragment)
%	    Fragment specification of HTTP URL. This   is the part after
%	    the =|#|= character.
%
%	The example below illustrates all of this for an HTTP URL.
%
%	    ==
%	    ?- parse_url('http://www.xyz.org/hello?msg=Hello+World%21#x',
%		   P).
%
%	    P = [ protocol(http),
%		  host('www.xyz.org'),
%		  fragment(x),
%		  search([ msg = 'Hello World!'
%			 ]),
%		  path('/hello')
%	        ]
%	    ==
%
%	By instantiating the parts-list this predicate   can  be used to
%	create a URL.

parse_url(URL, Attributes) :-
	nonvar(URL), !,
	atom_codes(URL, Codes),
	phrase(url(Attributes), Codes).
parse_url(URL, Attributes) :-
	phrase(curl(Attributes), Codes), !,
	atom_codes(URL, Codes).

%%	parse_url(+URL, +BaseURL, -Attributes) is det.
%
%	Similar to parse_url/2 for relative URLs.  If URL is relative,
%	it is resolved using the absolute URL BaseURL.

parse_url(URL, BaseURL, Attributes) :-
	nonvar(URL), !,
	atom_codes(URL, Codes),
	(   phrase(absolute_url, Codes, _)
	->  phrase(url(Attributes), Codes)
	;   (   atomic(BaseURL)
	    ->  parse_url(BaseURL, BaseA0)
	    ;	BaseA0 = BaseURL
	    ),
	    select(path(BasePath), BaseA0, BaseA1),
	    delete(BaseA1, search(_), BaseA2),
	    delete(BaseA2, fragment(_), BaseA3),
	    phrase(relative_uri(URIA0), Codes),
	    select(path(LocalPath), URIA0, URIA1), !,
	    globalise_path(LocalPath, BasePath, Path),
	    append(BaseA3, [path(Path)|URIA1], Attributes)
	).
parse_url(URL, BaseURL, Attributes) :-
	parse_url(BaseURL, BaseAttributes),
	memberchk(path(BasePath), BaseAttributes),
	(   memberchk(path(LocalPath), Attributes)
	->  globalise_path(LocalPath, BasePath, Path)
	;   Path = BasePath
	),
	append([path(Path)|Attributes], BaseAttributes, GlobalAttributes),
	phrase(curl(GlobalAttributes), Chars),
	atom_codes(URL, Chars).


%%	globalise_path(+LocalPath, +RelativeTo, -FullPath) is det.
%
%	The first clause deals with the  standard URL /... global paths.
%	The second with file://drive:path on MS-Windows.   This is a bit
%	of a cludge, but unfortunately common practice is -especially on
%	Windows- not always following the standard

globalise_path(LocalPath, _, LocalPath) :-
	sub_atom(LocalPath, 0, _, _, /), !.
globalise_path(LocalPath, _, LocalPath) :-
	is_absolute_file_name(LocalPath), !.
globalise_path(Local, Base, Path) :-
	base_dir(Base, BaseDir),
	make_path(BaseDir, Local, Path).

base_dir(BasePath, BaseDir) :-
	(   atom_concat(BaseDir, /, BasePath)
	->  true
	;   file_directory_name(BasePath, BaseDir)
	).

make_path(Dir, Local, Path) :-
	atom_concat('../', L2, Local),
	file_directory_name(Dir, Parent),
	Parent \== Dir, !,
	make_path(Parent, L2, Path).
make_path(/, Local, Path) :- !,
	atom_concat(/, Local, Path).
make_path(Dir, Local, Path) :-
	atomic_list_concat([Dir, /, Local], Path).


%%	absolute_url//
%
%	True if the input  describes  an   absolute  URL.  This means it
%	starts with a URL schema. We demand a   schema  of length > 1 to
%	avoid confusion with Windows drive letters.

absolute_url -->
	lwalpha(_First),
	schema_chars(Rest),
	{ Rest \== [] },
	":", !.


		 /*******************************
		 *	     SEQUENCES		*
		 *******************************/

digits(L) -->
	digits(L, []).

digits([C|T0], T) -->
	digit(C), !,
	digits(T0, T).
digits(T, T) -->
	[].


digit(C, [C|T], T) :- code_type(C, digit).

		 /*******************************
		 *	      RFC-3986		*
		 *******************************/

%%	uri(-Parts)//

url([protocol(Schema)|Parts]) -->
	schema(Schema),
	":", !,
	hier_part(Schema, Parts, P2),
	query(P2, P3),
	fragment(P3, []).
url([protocol(http)|Parts]) -->		% implicit HTTP
	authority(Parts, [path(Path)]),
	path_abempty(Path).

relative_uri(Parts) -->
	relative_part(Parts, P2),
	query(P2, P3),
	fragment(P3, []).

relative_part(Parts, Tail) -->
	"//", !,
	authority(Parts, [path(Path)|Tail]),
	path_abempty(Path).
relative_part([path(Path)|T], T) -->
	(   path_absolute(Path)
	;   path_noschema(Path)
	;   path_empty(Path)
	), !.

http_location([path(Path)|P2]) -->
	path_abempty(Path),
	query(P2, P3),
	fragment(P3, []).

%%	schema(-Atom)//
%
%	Schema  is  case-insensitive  and  the    canonical  version  is
%	lowercase.
%
%	==
%	Schema ::= ALPHA *(ALPHA|DIGIT|"+"|"-"|".")
%	==

schema(Schema) -->
	lwalpha(C0),
	schema_chars(Codes),
	{ atom_codes(Schema, [C0|Codes]) }.

schema_chars([H|T]) -->
	schema_char(H), !,
	schema_chars(T).
schema_chars([]) -->
	[].

schema_char(H) -->
	[C],
	{ C < 128,
	  (   code_type(C, alpha)
	  ->  code_type(H, to_lower(C))
	  ;   code_type(C, digit)
	  ->  H = C
	  ;   schema_extra(C)
	  ->  H = C
	  )
	}.

schema_extra(0'+).
schema_extra(0'-).
schema_extra(0'.).	% 0'


%%	hier_part(+Schema, -Parts, ?Tail)//

hier_part(file, [path(Path)|Tail], Tail) --> !,
	"//",
	(   win_drive_path(Path)
	;   path_absolute(Path)
	;   path_rootless(Path)
	;   path_empty(Path)
	), !.
hier_part(_, Parts, Tail) -->
	"//", !,
	authority(Parts, [path(Path)|Tail]),
	path_abempty(Path).
hier_part(_, [path(Path)|T], T) -->
	(   path_absolute(Path)
	;   path_rootless(Path)
	;   path_empty(Path)
	), !.

authority(Parts, Tail) -->
	user_info_chars(UserChars),
	"@", !,
	{ atom_codes(User, UserChars),
	  Parts = [user(User),host(Host)|T0]
	},
	host(Host),
	port(T0,Tail).
authority([host(Host)|T0], Tail) -->
	host(Host),
	port(T0, Tail).

user_info_chars([H|T]) -->
	user_info_char(H), !,
	user_info_chars(T).
user_info_chars([]) -->
	[].

user_info_char(_) --> "@", !, {fail}.
user_info_char(C) --> pchar(C).

%host(Host) --> ip_literal(Host), !.		% TBD: IP6 addresses
host(Host) --> ip4_address(Host), !.
host(Host) --> reg_name(Host).

ip4_address(Atom) -->
	i256_chars(Chars, [0'.|T0]),
	i256_chars(T0, [0'.|T1]),
	i256_chars(T1, [0'.|T2]),
	i256_chars(T2, []),
	{ atom_codes(Atom, Chars) }.

i256_chars(Chars, T) -->
	digits(Chars, T),
	{ \+ \+ ( T = [],
		  Chars \== [],
		  number_codes(I, Chars),
		  I < 256
		)
	}.

reg_name(Host) -->
	reg_name_chars(Chars),
	{ atom_codes(Host, Chars) }.

reg_name_chars([H|T]) -->
	reg_name_char(H), !,
	reg_name_chars(T).
reg_name_chars([]) -->
	[].

reg_name_char(C) -->
	pchar(C),
	{ C \== 0':,
	  C \== 0'@
	}.

port([port(Port)|T], T) -->
	":", !,
	digit(D0),
	digits(Ds),
	{ number_codes(Port, [D0|Ds]) }.
port(T, T) -->
	[].

path_abempty(Path) -->
	segments_chars(Chars, []),
	{   Chars == []
	->  Path = '/'
	;   atom_codes(Path, Chars)
	}.


win_drive_path(Path) -->
	drive_letter(C0),
	":",
	(   "/"
	->  {Codes = [C0, 0':, 0'/|Chars]}
	;   {Codes = [C0, 0':|Chars]}
	),
	segment_nz_chars(Chars, T0),
	segments_chars(T0, []),
	{ atom_codes(Path, Codes) }.


path_absolute(Path) -->
	"/",
	segment_nz_chars(Chars, T0),
	segments_chars(T0, []),
	{ atom_codes(Path, [0'/| Chars]) }.

path_noschema(Path) -->
	segment_nz_nc_chars(Chars, T0),
	segments_chars(T0, []),
	{ atom_codes(Path, Chars) }.

path_rootless(Path) -->
	segment_nz_chars(Chars, T0),
	segments_chars(T0, []),
	{ atom_codes(Path, Chars) }.

path_empty('/') -->
	"".

segments_chars([0'/|Chars], T) -->	% 0'
	"/", !,
	segment_chars(Chars, T0),
	segments_chars(T0, T).
segments_chars(T, T) -->
	[].

segment_chars([H|T0], T) -->
	pchar(H), !,
	segment_chars(T0, T).
segment_chars(T, T) -->
	[].

segment_nz_chars([H|T0], T) -->
	pchar(H),
	segment_chars(T0, T).

segment_nz_nc_chars([H|T0], T) -->
	segment_nz_nc_char(H), !,
	segment_nz_nc_chars(T0, T).
segment_nz_nc_chars(T, T) -->
	[].

segment_nz_nc_char(_) --> ":", !, {fail}.
segment_nz_nc_char(C) --> pchar(C).


%%	query(-Parts, ?Tail)// is det.
%
%	Extract &Name=Value, ...

query([search(Params)|T], T) -->
	"?", !,
	search(Params).
query(T,T) -->
	[].

search([Parameter|Parameters])-->
	parameter(Parameter), !,
	(   search_sep
        ->  search(Parameters)
        ;   { Parameters = [] }
        ).
search([]) -->
	[].

parameter(Param)--> !,
	search_chars(NameS),
	{ atom_codes(Name, NameS)
	},
	(   "="
        ->  search_value_chars(ValueS),
	    { atom_codes(Value, ValueS),
	      Param = (Name = Value)
	    }
        ;   { Param = Name
	    }
        ).

search_chars([C|T]) -->
	search_char(C), !,
	search_chars(T).
search_chars([]) -->
	[].

search_char(_) --> search_sep, !, { fail }.
search_char(_) --> "=", !, { fail }.
search_char(C) --> fragment_char(C).

search_value_chars([C|T]) -->
	search_value_char(C), !,
	search_value_chars(T).
search_value_chars([]) -->
	[].

search_value_char(_) --> search_sep, !, { fail }.
search_value_char(C) --> fragment_char(C).

%%	search_sep// is semidet.
%
%	Matches a search-parameter separator.  Traditionally, this is the
%	&-char, but these days there are `newstyle' ;-char separators.
%
%	@see http://perldoc.perl.org/CGI.html
%	@tbd This should be configurable

search_sep --> "&", !.
search_sep --> ";".


%%	fragment(-Fragment, ?Tail)//
%
%	Extract the fragment (after the =#=)

fragment([fragment(Fragment)|T], T) -->
	"#", !,
	fragment_chars(Codes),
	{ atom_codes(Fragment, Codes) }.
fragment(T, T) -->
	[].

fragment_chars([H|T]) -->
	fragment_char(H), !,
	fragment_chars(T).
fragment_chars([]) -->
	[].


%%	fragment_char(-Char)
%
%	Find a fragment character.

fragment_char(C)   --> pchar(C), !.
fragment_char(0'/) --> "/", !.
fragment_char(0'?) --> "?", !.
fragment_char(0'[) --> "[", !.		% Not according RDF3986!
fragment_char(0']) --> "]", !.


		 /*******************************
		 *	CHARACTER CLASSES	*
		 *******************************/

%%	pchar(-Code)//
%
%	unreserved|pct_encoded|sub_delim|":"|"@"
%
%	Performs UTF-8 decoding of percent encoded strings.

pchar(0'\s) --> "+", !.
pchar(C) -->
	[C],
	{   unreserved(C)
	;   sub_delim(C)
	;   C == 0':
        ;   C == 0'@
	}, !.
pchar(C) -->
	percent_coded(C).

%%	lwalpha(-C)//
%
%	Demand alpha, return as lowercase

lwalpha(H) -->
	[C],
	{ C < 128,
	  code_type(C, alpha),
	  code_type(H, to_lower(C))
	}.

drive_letter(C) -->
	[C],
	{ C < 128,
	  code_type(C, alpha)
	}.


		 /*******************************
		 *	RESERVED CHARACTERS	*
		 *******************************/

%%	sub_delim(?Code)
%
%	Sub-delimiters

sub_delim(0'!).
sub_delim(0'$).
sub_delim(0'&).
sub_delim(0'').
sub_delim(0'().
sub_delim(0')).
sub_delim(0'*).
sub_delim(0'+).
sub_delim(0',).
sub_delim(0';).
sub_delim(0'=).


%%	unreserved(+C)
%
%	Characters that can be represented without percent escaping
%	RFC 3986, section 2.3

term_expansion(unreserved(map), Clauses) :-
	findall(unreserved(C), unreserved_(C), Clauses).

unreserved_(C) :-
	between(1, 128, C),
	code_type(C, alnum).
unreserved_(0'-).
unreserved_(0'.).
unreserved_(0'_).
unreserved_(0'~).			% 0'

unreserved(map).			% Expanded


		 /*******************************
		 *	        FORMS		*
		 *******************************/

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Encoding/decoding of form-fields  using   the  popular  www-form-encoded
encoding used with the HTTP GET.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

%%	www_form_encode(+Value, -XWWWFormEncoded) is det.
%%	www_form_encode(-Value, +XWWWFormEncoded) is det.
%
%	En/decode   to/from   application/x-www-form-encoded.   Encoding
%	encodes all characters  except  RFC   3986  _unreserved_  (ASCII
%	=alnum= (see code_type/2)), and  one   of  "-._~"  using percent
%	encoding.  Newline  is  mapped  to  =|%OD%OA|=.  When  decoding,
%	newlines appear as a single newline (10) character.
%
%	Note that a space  is  encoded   as  =|%20|=  instead  of =|+|=.
%	Decoding decodes both to a space.
%
%	@deprecated Use uri_encoded/3 for new code.

www_form_encode(Value, Encoded) :-
	atomic(Value), !,
	atom_codes(Value, Codes),
	phrase(www_encode(Codes, ""), EncCodes),
	atom_codes(Encoded, EncCodes).
www_form_encode(Value, Encoded) :-
	atom_codes(Encoded, EncCodes),
	phrase(www_decode(Codes), EncCodes),
	atom_codes(Value, Codes).

%%	www_encode(+Codes, +ExtraUnescaped)//

www_encode([0'\r, 0'\n|T], Extra) --> !,
	"%0D%0A",
	www_encode(T, Extra).
www_encode([0'\n|T], Extra) --> !,
	"%0D%0A",
	www_encode(T, Extra).
www_encode([H|T], Extra) -->
	percent_encode(H, Extra),
	www_encode(T, Extra).
www_encode([], _) -->
	"".

percent_encode(C, _Extra) -->
	{ unreserved(C) }, !,
	[C].
percent_encode(C, Extra) -->
	{ memberchk(C, Extra) }, !,
	[C].
%percent_encode(0' , _) --> !, "+".	% Deprecated: use %20
percent_encode(C, _) -->
	{ C =< 127 }, !,
	percent_byte(C).
percent_encode(C, _) -->		% Unicode characters
	{ current_prolog_flag(url_encoding, utf8), !,
	  phrase(utf8_codes([C]), Bytes)
	},
	percent_bytes(Bytes).
percent_encode(C, _) -->
	{ C =< 255 }, !,
	percent_byte(C).
percent_encode(_C, _) -->
	{ representation_error(url_character)
	}.

percent_bytes([]) -->
	"".
percent_bytes([H|T]) -->
	percent_byte(H),
	percent_bytes(T).

percent_byte(C) -->
	[0'%, D1, D2],
	{   nonvar(C)
	->  Dv1 is (C>>4 /\ 0xf),
	    Dv2 is (C /\ 0xf),
	    code_type(D1, xdigit(Dv1)),
	    code_type(D2, xdigit(Dv2))
	;   code_type(D1, xdigit(Dv1)),
	    code_type(D2, xdigit(Dv2)),
	    C is ((Dv1)<<4) + Dv2
	}.

percent_coded(C) -->
	percent_byte(C0), !,
	(   { C0 == 13			% %0D%0A --> \n
	    },
	    "%0",
	    ( "A" ; "a" )
	->  { C = 10
	    }
	;   { C0 >= 0xc0 },		% UTF-8 lead-in
	    utf8_cont(Cs),
	    { phrase(utf8_codes([C]), [C0|Cs]) }
	->  []
	;   { C = C0
	    }
	).

%%	www_decode(-Codes)//

www_decode([0' |T]) -->
	"+", !,
        www_decode(T).
www_decode([C|T]) -->
	percent_coded(C), !,
	www_decode(T).
www_decode([C|T]) -->
	[C], !,
	www_decode(T).
www_decode([]) -->
	[].

utf8_cont([H|T]) -->
	percent_byte(H),
	{ between(0x80, 0xbf, H) }, !,
	utf8_cont(T).
utf8_cont([]) -->
	[].


%%	set_url_encoding(?Old, +New) is semidet.
%
%	Query and set the encoding for URLs.  The default is =utf8=.
%	The only other defined value is =iso_latin_1=.
%
%	@tbd	Having a global flag is highly inconvenient, but a
%		work-around for old sites using ISO Latin 1 encoding.

:- create_prolog_flag(url_encoding, utf8, [type(atom)]).

set_url_encoding(Old, New) :-
	current_prolog_flag(url_encoding, Old),
	(   Old == New
	->  true
	;   must_be(oneof([utf8, iso_latin_1]), New),
	    set_prolog_flag(url_encoding, New)
	).


		 /*******************************
		 *	 IRI PROCESSING		*
		 *******************************/

%%	url_iri(+Encoded, -Decoded) is det.
%%	url_iri(-Encoded, +Decoded) is det.
%
%	Convert between a URL, encoding in US-ASCII   and an IRI. An IRI
%	is a fully expanded Unicode string.   Unicode  strings are first
%	encoded into UTF-8, after which %-encoding takes place.

url_iri(Encoded, Decoded) :-
	nonvar(Encoded), !,
	(   sub_atom(Encoded, _, _, _, '%')
	->  atom_codes(Encoded, Codes),
	    unescape_precent(Codes, UTF8),
	    phrase(utf8_codes(Unicodes), UTF8),
	    atom_codes(Decoded, Unicodes)
	;   Decoded = Encoded
	).
url_iri(URL, IRI) :-
	atom_codes(IRI, IRICodes),
	phrase(percent_encode(IRICodes, "/:?#&="), UrlCodes),
	atom_codes(URL, UrlCodes).


unescape_precent([], []).
unescape_precent([0'%,C1,C2|T0], [H|T]) :- !,	%'
	code_type(C1, xdigit(D1)),
	code_type(C2, xdigit(D2)),
	H is D1*16 + D2,
	unescape_precent(T0, T).
unescape_precent([H|T0], [H|T]) :-
	unescape_precent(T0, T).


		 /*******************************
		 *	     FORM DATA		*
		 *******************************/

%%	parse_url_search(?Spec, ?Fields:list(Name=Value)) is det.
%
%	Construct or analyze an HTTP   search  specification. This deals
%	with       form       data       using       the       MIME-type
%	=application/x-www-form-urlencoded=  as  used   in    HTTP   GET
%	requests.

parse_url_search(Spec, Fields) :-
	atomic(Spec), !,
	atom_codes(Spec, Codes),
	phrase(search(Fields), Codes).
parse_url_search(Codes, Fields) :-
	is_list(Codes), !,
	phrase(search(Fields), Codes).
parse_url_search(Codes, Fields) :-
	must_be(list, Fields),
	phrase(csearch(Fields, ""), Codes).


		 /*******************************
		 *	    FILE URLs		*
		 *******************************/

%%	file_name_to_url(+File, -URL) is det.
%%	file_name_to_url(-File, +URL) is semidet.
%
%	Translate between a filename and a file:// URL.
%
%	@tbd	Current implementation does not deal with paths that
%		need special encoding.

file_name_to_url(File, FileURL) :-
	nonvar(File), !,
	absolute_file_name(File, Path),
	atom_concat('file://', Path, FileURL), !.
file_name_to_url(File, FileURL) :-
	atom_concat('file://', File, FileURL), !.

