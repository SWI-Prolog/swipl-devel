:- module(rdf_cache,
	  [ rdf_set_cache_options/1,	% +Options
	    rdf_cache_file/3		% +URL, +RW, -File
	  ]).
:- use_module(library(error)).
:- use_module(library(filesex)).

/** <module> Cache RDF triples

Triples may be cached to reduce load time   as well as access to network
resources (e.g. HTTP). We use two caching locations: typically files may
be cached locally (i.e. in a  .cache   sub-directory  of  the file). All
objects can be cached  in  a  global   cache  directory.  The  policy is
determined by rdf_cache_options/1.
*/

:- dynamic
	cache_option/1.

set_setfault_options :-
	assert(cache_option(enabled(true))),
	(   current_prolog_flag(windows, true)
	->  assert(cache_option(local_directory('_cache')))
	;   assert(cache_option(local_directory('.cache')))
	).

:- set_setfault_options.		% _only_ when loading!

%%	rdf_set_cache_options(+Options)
%
%	Change the cache policy.  Provided options are:
%
%	  * enabled(Boolean)
%	  If =true=, caching is enabled.
%
%	  * local_directory(Name).
%	  Plain name of local directory.  Default =|.cache|=
%	  (=|_cache|= on Windows).
%
%	  * create_local_directory(Bool)
%	  If =true=, try to create local cache directories
%
%	  * global_directory(Dir)
%	  Writeable directory for storing cached parsed files.
%
%	  * create_global_directory(Bool)
%	  If =true=, try to create the global cache directory.

rdf_set_cache_options([]) :- !.
rdf_set_cache_options([H|T]) :- !,
	rdf_set_cache_options(H),
	rdf_set_cache_options(T).
rdf_set_cache_options(Opt) :-
	functor(Opt, Name, Arity),
	arg(1, Opt, Value),
	(   cache_option(Name, Type)
	->  must_be(Type, Value)
	;   domain_error(cache_option, Opt)
	),
	functor(Gen, Name, Arity),
	retractall(cache_option(Gen)),
	expand_option(Opt, EOpt),
	assert(cache_option(EOpt)).

cache_option(enabled,		      boolean).
cache_option(local_directory,	      atom).
cache_option(create_local_directory,  boolean).
cache_option(global_directory,	      atom).
cache_option(create_global_directory, boolean).

expand_option(global_directory(Local), global_directory(Global)) :- !,
	absolute_file_name(Local, Global).
expand_option(Opt, Opt).


%%	rdf_cache_location(+URL, +ReadWrite, -File) is semidet.
%
%	File is the cache file  for  URL.   If  ReadWrite  is =read=, it
%	returns the name of an existing file.  If =write= it returns the
%	where a new cache file can be overwritten or created.

rdf_cache_file(_URL, _, _File) :-
	cache_option(enabled(false)), !,
	fail.
rdf_cache_file(URL, read, File) :- !,
	(   atom_concat('file://', Path, URL),
	    cache_option(local_directory(Local)),
	    file_directory_name(Path, Dir),
	    local_cache_file(URL, LocalFile),
	    atomic_list_concat([Dir, Local, LocalFile], /, File)
	;   cache_option(global_directory(Dir)),
	    url_cache_file(URL, Dir, trp, read, File)
	),
	access_file(File, read), !.
rdf_cache_file(URL, write, File) :- !,
	(   atom_concat('file://', Path, URL),
	    cache_option(local_directory(Local)),
	    file_directory_name(Path, Dir),
	    (	cache_option(create_local_directory(true))
	    ->	RWDir = write
	    ;	RWDir = read
	    ),
	    ensure_dir(Dir, Local, RWDir, CacheDir),
	    local_cache_file(URL, LocalFile),
	    atomic_list_concat([CacheDir, LocalFile], /, File)
	;   cache_option(global_directory(Dir)),
	    ensure_global_cache(Dir),
	    url_cache_file(URL, Dir, trp, write, File)
	),
	access_file(File, write), !.


ensure_global_cache(Dir) :-
	exists_directory(Dir), !.
ensure_global_cache(Dir) :-
	cache_option(create_global_directory(true)),
	make_directory_path(Dir),
	print_message(informational, rdf(cache_created(Dir))).


		 /*******************************
		 *	   LOCAL CACHE		*
		 *******************************/

%%	local_cache_file(+FileURL, -File) is det.
%
%	Return the name of the cache file   for FileURL. The name is the
%	plain filename with the .trp extension.  As   the  URL is a file
%	URL, it is guaranteed  to  be   a  valid  filename.  Assumes the
%	hosting OS can handle  multiple   exensions  (=|.x.y|=)  though.
%	These days thats even true on Windows.

local_cache_file(URL, File) :-
	file_base_name(URL, Name),
	file_name_extension(Name, trp, File).


		 /*******************************
		 *	   GLOBAL CACHE		*
		 *******************************/

%%	url_cache_file(+URL, +Dir, +Ext, +RW, -Path) is semidet.
%
%	Determine location of cache-file for the   given  URL in Dir. If
%	Ext is provided, the  returned  Path   is  ensured  to  have the
%	specified extension.
%
%	@param RW	If =read=, no directories are created and the call
%			fails if URL is not in the cache.

url_cache_file(URL, Dir, Ext, RW, Path) :-
	term_hash(URL, Hash0),
	Hash is Hash0 + 100000,		% make sure > 4 characters
	format(string(Hex), '~16r', [Hash]),
	sub_atom(Hex, _, 2, 0, L1),
	ensure_dir(Dir, L1, RW, Dir1),
	sub_atom(Hex, _, 2, 2, L2),
	ensure_dir(Dir1, L2, RW, Dir2),
	url_to_file(URL, File),
	ensure_ext(File, Ext, FileExt),
	atomic_list_concat([Dir2, /, FileExt], Path).

ensure_dir(D0, Sub, RW, Dir) :-
	atomic_list_concat([D0, /, Sub], Dir),
	(   exists_directory(Dir)
	->  true
	;   RW == write
	->  catch(make_directory(Dir), _, fail)
	).

ensure_ext(File, '', File) :- !.
ensure_ext(File, Ext, File) :-
	file_name_extension(_, Ext, File), !.
ensure_ext(File, Ext, FileExt) :-
	file_name_extension(File, Ext, FileExt).

%%	url_to_file(+URL, -File)
%
%	Convert a URL in something that fits  in a file, i.e. avoiding /
%	and :. We  simply  replace  these  by   -.  We  could  also  use
%	www_form_encode/2, but confusion when to replace  as well as the
%	fact that we loose the '.' (extension)   makes this a less ideal
%	choice.  We could also consider base64 encoding of the name.

url_to_file(URL, File) :-
	atom_codes(URL, Codes),
	phrase(safe_file_name(Codes), FileCodes),
	atom_codes(File, FileCodes).

safe_file_name([]) -->
	[].
safe_file_name([H|T]) -->
	replace(H), !,
	safe_file_name(T).
safe_file_name([H|T]) -->
	[H],
	safe_file_name(T).

%%	replace(+Code)//
%
%	Replace a character  code  that  cannot   safely  be  put  in  a
%	filename. Should we use %XX?

replace(0'/)  --> "-".			% directory separator
replace(0'\\) --> "-".			% not allowed in Windows filename
replace(0':)  --> "-".			% idem
replace(0'?)  --> "-".			% idem
replace(0'*)  --> "-".			% idem


		 /*******************************
		 *	       MESSAGES		*
		 *******************************/

:- multifile prolog:message/3.

prolog:message(rdf(cache_created(Dir))) -->
	[ 'Created RDF cache directory ~w'-[Dir] ].
