/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           https://www.swi-prolog.org
    Copyright (c)  2012-2024, VU University Amsterdam
                              CWI, Amsterdam
                              SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(prolog_pack,
          [ pack_list_installed/0,
            pack_info/1,                % +Name
            pack_list/1,                % +Keyword
            pack_list/2,                % +Query, +Options
            pack_search/1,              % +Keyword
            pack_install/1,             % +Name
            pack_install/2,             % +Name, +Options
            pack_install_local/3,       % :Spec, +Dir, +Options
            pack_upgrade/1,             % +Name
            pack_rebuild/1,             % +Name
            pack_rebuild/0,             % All packages
            pack_remove/1,              % +Name
            pack_remove/2,              % +Name, +Options
            pack_publish/2,             % +URL, +Options
            pack_property/2             % ?Name, ?Property
          ]).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(filesex)).
:- use_module(library(xpath)).
:- use_module(library(settings)).
:- use_module(library(uri)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/http_client), []).
:- use_module(library(debug), [assertion/1]).
:- use_module(library(pairs),
              [pairs_keys/2, map_list_to_pairs/3, pairs_values/2]).
:- autoload(library(git)).
:- autoload(library(sgml)).
:- autoload(library(sha)).
:- autoload(library(build/tools)).
:- autoload(library(ansi_term), [ansi_format/3]).
:- autoload(library(pprint), [print_term/2]).
:- autoload(library(prolog_versions), [require_version/3, cmp_versions/3]).
:- autoload(library(ugraphs), [vertices_edges_to_ugraph/3, ugraph_layers/2]).
:- autoload(library(process), [process_which/2]).
:- autoload(library(aggregate), [aggregate_all/3]).

:- meta_predicate
    pack_install_local(2, +, +).

/** <module> A package manager for Prolog

The library(prolog_pack) provides the SWI-Prolog   package manager. This
library lets you inspect installed   packages,  install packages, remove
packages, etc. This library complemented by the built-in predicates such
as attach_packs/2 that makes installed packages available as libraries.

The important functionality of this library is encapsulated in the _app_
`pack`. For help, run

    swipl pack help
*/

                 /*******************************
                 *          CONSTANTS           *
                 *******************************/

:- setting(server, atom, 'https://www.swi-prolog.org/pack/',
           'Server to exchange pack information').


		 /*******************************
		 *       LOCAL DECLARATIONS	*
		 *******************************/

:- op(900, xfx, @).                     % Token@Version

:- meta_predicate det_if(0,0).

                 /*******************************
                 *         PACKAGE INFO         *
                 *******************************/

%!  current_pack(?Pack) is nondet.
%!  current_pack(?Pack, ?Dir) is nondet.
%
%   True if Pack is a currently installed pack.

current_pack(Pack) :-
    current_pack(Pack, _).

current_pack(Pack, Dir) :-
    '$pack':pack(Pack, Dir).

%!  pack_list_installed is det.
%
%   List currently installed packages  and   report  possible dependency
%   issues.

pack_list_installed :-
    pack_list('', [installed(true)]),
    validate_dependencies.

%!  pack_info(+Pack)
%
%   Print more detailed information about Pack.

pack_info(Name) :-
    pack_info(info, Name).

pack_info(Level, Name) :-
    must_be(atom, Name),
    findall(Info, pack_info(Name, Level, Info), Infos0),
    (   Infos0 == []
    ->  print_message(warning, pack(no_pack_installed(Name))),
        fail
    ;   true
    ),
    findall(Def,  pack_default(Level, Infos, Def), Defs),
    append(Infos0, Defs, Infos1),
    sort(Infos1, Infos),
    show_info(Name, Infos, [info(Level)]).


show_info(_Name, _Properties, Options) :-
    option(silent(true), Options),
    !.
show_info(_Name, _Properties, Options) :-
    option(show_info(false), Options),
    !.
show_info(Name, Properties, Options) :-
    option(info(list), Options),
    !,
    memberchk(title(Title), Properties),
    memberchk(version(Version), Properties),
    format('i ~w@~w ~28|- ~w~n', [Name, Version, Title]).
show_info(Name, Properties, _) :-
    !,
    print_property_value('Package'-'~w', [Name]),
    findall(Term, pack_level_info(info, Term, _, _), Terms),
    maplist(print_property(Properties), Terms).

print_property(_, nl) :-
    !,
    format('~n').
print_property(Properties, Term) :-
    findall(Term, member(Term, Properties), Terms),
    Terms \== [],
    !,
    pack_level_info(_, Term, LabelFmt, _Def),
    (   LabelFmt = Label-FmtElem
    ->  true
    ;   Label = LabelFmt,
        FmtElem = '~w'
    ),
    multi_valued(Terms, FmtElem, FmtList, Values),
    atomic_list_concat(FmtList, ', ', Fmt),
    print_property_value(Label-Fmt, Values).
print_property(_, _).

multi_valued([H], LabelFmt, [LabelFmt], Values) :-
    !,
    H =.. [_|Values].
multi_valued([H|T], LabelFmt, [LabelFmt|LT], Values) :-
    H =.. [_|VH],
    append(VH, MoreValues, Values),
    multi_valued(T, LabelFmt, LT, MoreValues).


pvalue_column(31).
print_property_value(Prop-Fmt, Values) :-
    !,
    pvalue_column(C),
    ansi_format(comment, '% ~w:~t~*|', [Prop, C]),
    ansi_format(code, Fmt, Values),
    ansi_format([], '~n', []).

pack_info(Name, Level, Info) :-
    '$pack':pack(Name, BaseDir),
    pack_dir_info(BaseDir, Level, Info).

pack_dir_info(BaseDir, Level, Info) :-
    (   Info = directory(BaseDir)
    ;   pack_info_term(BaseDir, Info)
    ),
    pack_level_info(Level, Info, _Format, _Default).

:- public pack_level_info/4.                    % used by web-server

pack_level_info(_,    title(_),         'Title',                   '<no title>').
pack_level_info(_,    version(_),       'Installed version',       '<unknown>').
pack_level_info(info, automatic(_),	'Automatic (dependency only)', -).
pack_level_info(info, directory(_),     'Installed in directory',  -).
pack_level_info(info, link(_),		'Installed as link to'-'~w', -).
pack_level_info(info, built(_,_),	'Built on'-'~w for SWI-Prolog ~w', -).
pack_level_info(info, author(_, _),     'Author'-'~w <~w>',        -).
pack_level_info(info, maintainer(_, _), 'Maintainer'-'~w <~w>',    -).
pack_level_info(info, packager(_, _),   'Packager'-'~w <~w>',      -).
pack_level_info(info, home(_),          'Home page',               -).
pack_level_info(info, download(_),      'Download URL',            -).
pack_level_info(_,    provides(_),      'Provides',                -).
pack_level_info(_,    requires(_),      'Requires',                -).
pack_level_info(_,    conflicts(_),     'Conflicts with',          -).
pack_level_info(_,    replaces(_),      'Replaces packages',       -).
pack_level_info(info, library(_),	'Provided libraries',      -).
pack_level_info(info, autoload(_),	'Autoload',                -).

pack_default(Level, Infos, Def) :-
    pack_level_info(Level, ITerm, _Format, Def),
    Def \== (-),
    \+ memberchk(ITerm, Infos).

%!  pack_info_term(+PackDir, ?Info) is nondet.
%
%   True when Info is meta-data for the package PackName.

pack_info_term(BaseDir, Info) :-
    directory_file_path(BaseDir, 'pack.pl', InfoFile),
    catch(
        term_in_file(valid_term(pack_info_term), InfoFile, Info),
        error(existence_error(source_sink, InfoFile), _),
        ( print_message(error, pack(no_meta_data(BaseDir))),
          fail
        )).
pack_info_term(BaseDir, library(Lib)) :-
    atom_concat(BaseDir, '/prolog/', LibDir),
    atom_concat(LibDir, '*.pl', Pattern),
    expand_file_name(Pattern, Files),
    maplist(atom_concat(LibDir), Plain, Files),
    convlist(base_name, Plain, Libs),
    member(Lib, Libs),
    Lib \== 'INDEX'.
pack_info_term(BaseDir, autoload(true)) :-
    atom_concat(BaseDir, '/prolog/INDEX.pl', IndexFile),
    exists_file(IndexFile).
pack_info_term(BaseDir, automatic(Boolean)) :-
    once(pack_status_dir(BaseDir, automatic(Boolean))).
pack_info_term(BaseDir, built(Arch, Prolog)) :-
    pack_status_dir(BaseDir, built(Arch, Prolog, _How)).
pack_info_term(BaseDir, link(Dest)) :-
    read_link(BaseDir, _, Dest).

base_name(File, Base) :-
    file_name_extension(Base, pl, File).

%!  term_in_file(:Valid, +File, -Term) is nondet.
%
%   True when Term appears in file and call(Valid, Term) is true.

:- meta_predicate
    term_in_file(1, +, -).

term_in_file(Valid, File, Term) :-
    exists_file(File),
    setup_call_cleanup(
        open(File, read, In, [encoding(utf8)]),
        term_in_stream(Valid, In, Term),
        close(In)).

term_in_stream(Valid, In, Term) :-
    repeat,
        read_term(In, Term0, []),
        (   Term0 == end_of_file
        ->  !, fail
        ;   Term = Term0,
            call(Valid, Term0)
        ).

:- meta_predicate
    valid_term(1,+).

valid_term(Type, Term) :-
    Term =.. [Name|Args],
    same_length(Args, Types),
    Decl =.. [Name|Types],
    (   call(Type, Decl)
    ->  maplist(valid_info_arg, Types, Args)
    ;   print_message(warning, pack(invalid_term(Type, Term))),
        fail
    ).

valid_info_arg(Type, Arg) :-
    must_be(Type, Arg).

%!  pack_info_term(?Term) is nondet.
%
%   True when Term describes name and   arguments of a valid package
%   info term.

pack_info_term(name(atom)).                     % Synopsis
pack_info_term(title(atom)).
pack_info_term(keywords(list(atom))).
pack_info_term(description(list(atom))).
pack_info_term(version(version)).
pack_info_term(author(atom, email_or_url_or_empty)).     % Persons
pack_info_term(maintainer(atom, email_or_url)).
pack_info_term(packager(atom, email_or_url)).
pack_info_term(pack_version(nonneg)).           % Package convention version
pack_info_term(home(atom)).                     % Home page
pack_info_term(download(atom)).                 % Source
pack_info_term(provides(atom)).                 % Dependencies
pack_info_term(requires(dependency)).
pack_info_term(conflicts(dependency)).          % Conflicts with package
pack_info_term(replaces(atom)).                 % Replaces another package
pack_info_term(autoload(boolean)).              % Default installation options

:- multifile
    error:has_type/2.

error:has_type(version, Version) :-
    atom(Version),
    is_version(Version).
error:has_type(email_or_url, Address) :-
    atom(Address),
    (   sub_atom(Address, _, _, _, @)
    ->  true
    ;   uri_is_global(Address)
    ).
error:has_type(email_or_url_or_empty, Address) :-
    (   Address == ''
    ->  true
    ;   error:has_type(email_or_url, Address)
    ).
error:has_type(dependency, Value) :-
    is_dependency(Value).

is_version(Version) :-
    split_string(Version, ".", "", Parts),
    maplist(number_string, _, Parts).

is_dependency(Var) :-
    var(Var),
    !,
    fail.
is_dependency(Token) :-
    atom(Token),
    !.
is_dependency(Term) :-
    compound(Term),
    compound_name_arguments(Term, Op, [Token,Version]),
    atom(Token),
    cmp(Op, _),
    is_version(Version),
    !.
is_dependency(PrologToken) :-
    is_prolog_token(PrologToken).

cmp(<,  @<).
cmp(=<, @=<).
cmp(==, ==).
cmp(>=, @>=).
cmp(>,  @>).


                 /*******************************
                 *            SEARCH            *
                 *******************************/

%!  pack_list(+Query) is det.
%!  pack_list(+Query, +Options) is det.
%!  pack_search(+Query) is det.
%
%   Query package server and  installed   packages  and display results.
%   Query is matches case-insensitively against the   name  and title of
%   known and installed packages. For each   matching  package, a single
%   line is displayed that provides:
%
%     - Installation status
%       - __p__: package, not installed
%       - __i__: installed package; up-to-date with public version
%       - __a__: as __i__, but installed only as dependency
%       - __U__: installed package; can be upgraded
%       - __A__: installed package; newer than publically available
%       - __l__: installed package; not on server
%     - Name@Version
%     - Name@Version(ServerVersion)
%     - Title
%
%   Options processed:
%
%     - installed(true)
%       Only list packages that are locally installed.  Contacts the
%       server to compare our local version to the latest available
%       version.
%     - outdated(true)
%       Only list packages that need to be updated.  This option
%       implies installed(true).
%     - server(Server|false)
%       If `false`, do not contact the server. This implies
%       installed(true).  Otherwise, use the given pack server.
%
%   Hint: ``?- pack_list('').`` lists all known packages.
%
%   The predicates pack_list/1 and  pack_search/1   are  synonyms.  Both
%   contact the package server  at   https://www.swi-prolog.org  to find
%   available packages. Contacting the server can   be avoided using the
%   server(false) option.

pack_list(Query) :-
    pack_list(Query, []).

pack_search(Query) :-
    pack_list(Query, []).

pack_list(Query, Options) :-
    (   option(installed(true), Options)
    ;   option(outdated(true), Options)
    ;   option(server(false), Options)
    ),
    !,
    local_search(Query, Local),
    maplist(arg(1), Local, Packs),
    (   option(server(false), Options)
    ->  Hits = []
    ;   query_pack_server(info(Packs), true(Hits), Options)
    ),
    list_hits(Hits, Local, Options).
pack_list(Query, Options) :-
    query_pack_server(search(Query), Result, Options),
    (   Result == false
    ->  (   local_search(Query, Packs),
            Packs \== []
        ->  forall(member(pack(Pack, Stat, Title, Version, _), Packs),
                   format('~w ~w@~w ~28|- ~w~n',
                          [Stat, Pack, Version, Title]))
        ;   print_message(warning, pack(search_no_matches(Query)))
        )
    ;   Result = true(Hits), % Hits = list(pack(Name, p, Title, Version, URL))
        local_search(Query, Local),
        list_hits(Hits, Local, [])
    ).

list_hits(Hits, Local, Options) :-
    append(Hits, Local, All),
    sort(All, Sorted),
    join_status(Sorted, Packs0),
    include(filtered(Options), Packs0, Packs),
    maplist(list_hit(Options), Packs).

filtered(Options, pack(_,Tag,_,_,_)) :-
    option(outdated(true), Options),
    !,
    Tag == 'U'.
filtered(_, _).

list_hit(_Options, pack(Pack, Tag, Title, Version, _URL)) =>
    list_tag(Tag),
    ansi_format(code, '~w', [Pack]),
    format('@'),
    list_version(Tag, Version),
    format('~35|- ', []),
    ansi_format(comment, '~w~n', [Title]).

list_tag(Tag) :-
    tag_color(Tag, Color),
    ansi_format(Color, '~w ', [Tag]).

list_version(Tag, VersionI-VersionS) =>
    tag_color(Tag, Color),
    ansi_format(Color, '~w', [VersionI]),
    ansi_format(bold, '(~w)', [VersionS]).
list_version(_Tag, Version) =>
    ansi_format([], '~w', [Version]).

tag_color('U', warning) :- !.
tag_color('A', comment) :- !.
tag_color(_, []).

%!  join_status(+PacksIn, -PacksOut) is det.
%
%   Combine local and remote information to   assess  the status of each
%   package. PacksOut is a list of  pack(Name, Status, Version, URL). If
%   the     versions     do      not       match,      `Version`      is
%   `VersionInstalled-VersionRemote` and similar for thee URL.

join_status([], []).
join_status([ pack(Pack, i, Title, Version, URL),
              pack(Pack, p, Title, Version, _)
            | T0
            ],
            [ pack(Pack, Tag, Title, Version, URL)
            | T
            ]) :-
    !,
    (   pack_status(Pack, automatic(true))
    ->  Tag = a
    ;   Tag = i
    ),
    join_status(T0, T).
join_status([ pack(Pack, i, Title, VersionI, URLI),
              pack(Pack, p, _,     VersionS, URLS)
            | T0
            ],
            [ pack(Pack, Tag, Title, VersionI-VersionS, URLI-URLS)
            | T
            ]) :-
    !,
    version_sort_key(VersionI, VDI),
    version_sort_key(VersionS, VDS),
    (   VDI @< VDS
    ->  Tag = 'U'
    ;   Tag = 'A'
    ),
    join_status(T0, T).
join_status([ pack(Pack, i, Title, VersionI, URL)
            | T0
            ],
            [ pack(Pack, l, Title, VersionI, URL)
            | T
            ]) :-
    !,
    join_status(T0, T).
join_status([H|T0], [H|T]) :-
    join_status(T0, T).

%!  local_search(+Query, -Packs:list(atom)) is det.
%
%   Search locally installed packs.

local_search(Query, Packs) :-
    findall(Pack, matching_installed_pack(Query, Pack), Packs).

matching_installed_pack(Query, pack(Pack, i, Title, Version, URL)) :-
    current_pack(Pack),
    findall(Term,
            ( pack_info(Pack, _, Term),
              search_info(Term)
            ), Info),
    (   sub_atom_icasechk(Pack, _, Query)
    ->  true
    ;   memberchk(title(Title), Info),
        sub_atom_icasechk(Title, _, Query)
    ),
    option(title(Title), Info, '<no title>'),
    option(version(Version), Info, '<no version>'),
    option(download(URL), Info, '<no download url>').

search_info(title(_)).
search_info(version(_)).
search_info(download(_)).


                 /*******************************
                 *            INSTALL           *
                 *******************************/

%!  pack_install(+Spec:atom) is det.
%!  pack_install(+SpecOrList, +Options) is det.
%
%   Install one or more packs from   SpecOrList.  SpecOrList is a single
%   specification or a list of specifications. A specification is one of
%
%     * A pack name.  This queries the pack repository
%       at https://www.swi-prolog.org
%     * Archive file name
%     * A http(s) URL of an archive file name.  This URL may contain a
%       star (*) for the version.  In this case pack_install/1 asks
%       for the directory content and selects the latest version.
%     * An https GIT URL
%     * A local directory name given as ``file://`` URL
%     * `'.'`, in which case a relative symlink is created to the
%       current directory (all other options for Spec make a copy
%       of the files).  Installation using a symlink is normally
%       used during development of a pack.
%
%   Processes the options below. Default  options   as  would be used by
%   pack_install/1 are used to complete the  provided Options. Note that
%   pack_install/2 can be used through the   SWI-Prolog command line app
%   `pack` as below. Most of the options of this predicate are available
%   as command line options.
%
%      swipl pack install <name>
%
%   Options:
%
%     * url(+URL)
%       Source for downloading the package
%     * pack_directory(+Dir)
%       Directory into which to install the package.
%     * global(+Boolean)
%       If `true`, install in the XDG common application data path,
%       making the pack accessible to everyone. If `false`, install in
%       the XDG user application data path, making the pack accessible
%       for the current user only. If the option is absent, use the
%       first existing and writable directory. If that doesn't exist
%       find locations where it can be created and prompt the user to do
%       so.
%     * insecure(+Boolean)
%       When `true` (default `false`), do not perform any checks on SSL
%       certificates when downloading using `https`.
%     * interactive(+Boolean)
%       Use default answer without asking the user if there
%       is a default action.
%     * silent(+Boolean)
%       If `true` (default false), suppress informational progress
%       messages.
%     * upgrade(+Boolean)
%       If `true` (default `false`), upgrade package if it is already
%       installed.
%     * rebuild(Condition)
%       Rebuild the foreign components.  Condition is one of
%       `if_absent` (default, do nothing if the directory with foreign
%       resources exists), `make` (run `make`) or `true` (run `make
%       distclean` followed by the default configure and build steps).
%     * test(Boolean)
%       If `true` (default), run the pack tests.
%     * git(+Boolean)
%       If `true` (default `false` unless `URL` ends with ``.git``),
%       assume the URL is a GIT repository.
%     * link(+Boolean)
%       Can be used if the installation source is a local directory
%       and the file system supports symbolic links.  In this case
%       the system adds the current directory to the pack registration
%       using a symbolic link and performs the local installation steps.
%     * version(+Version)
%       Demand the pack to satisfy some version requirement.  Version
%       is as defined by require_version/3.  For example `'1.5'` is the
%       same as `>=('1.5')`.
%     * branch(+Branch)
%       When installing from a git repository, clone this branch.
%     * commit(+Commit)
%       When installing from a git repository, checkout this commit.
%       Commit is either a hash, a tag, a branch or `'HEAD'`.
%     * build_type(+Type)
%       When building using CMake, use ``-DCMAKE_BUILD_TYPE=Type``.
%       Default is the build type of Prolog or ``Release``.
%     * register(+Boolean)
%       If `true` (default), register packages as downloaded after
%       performing the download.  This contacts the server with the
%       meta-data of each pack that was downloaded.  The server will
%       either register the location as a new version or increment
%       the download count.  The server stores the IP address of the
%       client.  Subsequent downloads of the same version from the
%       same IP address are ignored.
%     * server(+URL)
%       Pack server to contact. Default is the setting
%       `prolog_pack:server`, by default set to
%       ``https://www.swi-prolog.org/pack/``
%
%   Non-interactive installation can be established using the option
%   interactive(false). It is adviced to   install from a particular
%   _trusted_ URL instead of the  plain   pack  name  for unattented
%   operation.

pack_install(Spec) :-
    pack_default_options(Spec, Pack, [], Options),
    pack_install(Pack, [pack(Pack)|Options]).

pack_install(Specs, Options) :-
    is_list(Specs),
    !,
    maplist(pack_options(Options), Specs, Pairs),
    pack_install_dir(PackTopDir, Options),
    pack_install_set(Pairs, PackTopDir, Options).
pack_install(Spec, Options) :-
    pack_default_options(Spec, Pack, Options, DefOptions),
    (   option(already_installed(Installed), DefOptions)
    ->  print_message(informational, pack(already_installed(Installed)))
    ;   merge_options(Options, DefOptions, PackOptions),
        pack_install_dir(PackTopDir, PackOptions),
        pack_install_set([Pack-PackOptions], PackTopDir, Options)
    ).

pack_options(Options, Spec, Pack-PackOptions) :-
    pack_default_options(Spec, Pack, Options, DefOptions),
    merge_options(Options, DefOptions, PackOptions).

%!  pack_default_options(+Spec, -Pack, +OptionsIn, -Options) is det.
%
%   Establish  the  pack  name  (Pack)  and    install  options  from  a
%   specification and options (OptionsIn) provided by the user.  Cases:
%
%     1. Already installed.  We must pass that as pack_default_options/4
%        is called twice from pack_install/2.
%     2. Install from a URL due to a url(URL) option. Determine whether
%        the URL is a GIT repository, get the version and pack from the
%        URL.
%     3. Install a local archive file. Extract the pack and version from
%        the archive name.
%     4. Install from a git URL.  Determines the pack, sets git(true)
%        and adds the URL as option.
%     5. Install from a directory. Get the info from the `packs.pl`
%        file.
%     6. Install from `'.'`.  Create a symlink to make the current dir
%        accessible as a pack.
%     7. Install from a non-git URL
%        Determine pack and version.
%     8. Pack name.  Query the server to find candidate packs and
%        select an adequate pack.


pack_default_options(_Spec, Pack, OptsIn, Options) :-   % (1)
    option(already_installed(pack(Pack,_Version)), OptsIn),
    !,
    Options = OptsIn.
pack_default_options(_Spec, Pack, OptsIn, Options) :-   % (2)
    option(url(URL), OptsIn),
    !,
    (   option(git(_), OptsIn)
    ->  Options = OptsIn
    ;   git_url(URL, Pack)
    ->  Options = [git(true)|OptsIn]
    ;   Options = OptsIn
    ),
    (   nonvar(Pack)
    ->  true
    ;   option(pack(Pack), Options)
    ->  true
    ;   pack_version_file(Pack, _Version, URL)
    ).
pack_default_options(Archive, Pack, OptsIn, Options) :- % (3)
    must_be(atom, Archive),
    \+ uri_is_global(Archive),
    expand_file_name(Archive, [File]),
    exists_file(File),
    !,
    (   pack_version_file(Pack, Version, File)
    ->  uri_file_name(FileURL, File),
        merge_options([url(FileURL), version(Version)], OptsIn, Options)
    ;   domain_error(pack_file_name, Archive)
    ).
pack_default_options(URL, Pack, OptsIn, Options) :-     % (4)
    git_url(URL, Pack),
    !,
    merge_options([git(true), url(URL)], OptsIn, Options).
pack_default_options(FileURL, Pack, _, Options) :-      % (5)
    uri_file_name(FileURL, Dir),
    exists_directory(Dir),
    pack_info_term(Dir, name(Pack)),
    !,
    (   pack_info_term(Dir, version(Version))
    ->  uri_file_name(DirURL, Dir),
        Options = [url(DirURL), version(Version)]
    ;   throw(error(existence_error(key, version, Dir),_))
    ).
pack_default_options('.', Pack, OptsIn, Options) :-     % (6)
    pack_info_term('.', name(Pack)),
    !,
    working_directory(Dir, Dir),
    (   pack_info_term(Dir, version(Version))
    ->  uri_file_name(DirURL, Dir),
        NewOptions = [url(DirURL), version(Version) | Options1],
        (   current_prolog_flag(windows, true)
        ->  Options1 = []
        ;   Options1 = [link(true), rebuild(make)]
        ),
        merge_options(NewOptions, OptsIn, Options)
    ;   throw(error(existence_error(key, version, Dir),_))
    ).
pack_default_options(URL, Pack, OptsIn, Options) :-      % (7)
    pack_version_file(Pack, Version, URL),
    download_url(URL),
    !,
    available_download_versions(URL, Available, Options),
    Available = [URLVersion-LatestURL|_],
    NewOptions = [url(LatestURL)|VersionOptions],
    version_options(Version, URLVersion, Available, VersionOptions),
    merge_options(NewOptions, OptsIn, Options).
pack_default_options(Pack, Pack, Options, Options) :-    % (8)
    \+ uri_is_global(Pack).

version_options(Version, Version, _, [version(Version)]) :- !.
version_options(Version, _, Available, [versions(Available)]) :-
    sub_atom(Version, _, _, _, *),
    !.
version_options(_, _, _, []).

%!  pack_install_dir(-PackDir, +Options) is det.
%
%   Determine the directory below which to  install new packs. This find
%   or creates a writeable directory.  Options:
%
%     - pack_directory(+PackDir)
%       Use PackDir. PackDir is created if it does not exist.
%     - global(+Boolean)
%       If `true`, find a writeable global directory based on the
%       file search path `common_app_data`.  If `false`, find a
%       user-specific writeable directory based on `user_app_data`
%     - If neither of the above is given, use the search path
%       `pack`.
%
%   If no writeable directory is found, generate possible location where
%   this directory can be created and  ask   the  user  to create one of
%   them.

pack_install_dir(PackDir, Options) :-
    option(pack_directory(PackDir), Options),
    ensure_directory(PackDir),
    !.
pack_install_dir(PackDir, Options) :-
    base_alias(Alias, Options),
    absolute_file_name(Alias, PackDir,
                       [ file_type(directory),
                         access(write),
                         file_errors(fail)
                       ]),
    !.
pack_install_dir(PackDir, Options) :-
    pack_create_install_dir(PackDir, Options).

base_alias(Alias, Options) :-
    option(global(true), Options),
    !,
    Alias = common_app_data(pack).
base_alias(Alias, Options) :-
    option(global(false), Options),
    !,
    Alias = user_app_data(pack).
base_alias(Alias, _Options) :-
    Alias = pack('.').

pack_create_install_dir(PackDir, Options) :-
    base_alias(Alias, Options),
    findall(Candidate = create_dir(Candidate),
            ( absolute_file_name(Alias, Candidate, [solutions(all)]),
              \+ exists_file(Candidate),
              \+ exists_directory(Candidate),
              file_directory_name(Candidate, Super),
              (   exists_directory(Super)
              ->  access_file(Super, write)
              ;   true
              )
            ),
            Candidates0),
    list_to_set(Candidates0, Candidates),   % keep order
    pack_create_install_dir(Candidates, PackDir, Options).

pack_create_install_dir(Candidates, PackDir, Options) :-
    Candidates = [Default=_|_],
    !,
    append(Candidates, [cancel=cancel], Menu),
    menu(pack(create_pack_dir), Menu, Default, Selected, Options),
    Selected \== cancel,
    (   catch(make_directory_path(Selected), E,
              (print_message(warning, E), fail))
    ->  PackDir = Selected
    ;   delete(Candidates, PackDir=create_dir(PackDir), Remaining),
        pack_create_install_dir(Remaining, PackDir, Options)
    ).
pack_create_install_dir(_, _, _) :-
    print_message(error, pack(cannot_create_dir(pack(.)))),
    fail.

%!  pack_unpack_from_local(+Source, +PackTopDir, +Name, -PackDir, +Options)
%
%   Unpack a package from a  local  media.   If  Source  is a directory,
%   either copy or link the directory. Else,   Source must be an archive
%   file. Options:
%
%      - link(+Boolean)
%        If the source is a directory, link or copy the directory?
%      - upgrade(true)
%        If the target is already there, wipe it and make a clean
%        install.

pack_unpack_from_local(Source0, PackTopDir, Name, PackDir, Options) :-
    exists_directory(Source0),
    remove_slash(Source0, Source),
    !,
    directory_file_path(PackTopDir, Name, PackDir),
    (   option(link(true), Options)
    ->  (   same_file(Source, PackDir)
        ->  true
        ;   remove_existing_pack(PackDir, Options),
            atom_concat(PackTopDir, '/', PackTopDirS),
            relative_file_name(Source, PackTopDirS, RelPath),
            link_file(RelPath, PackDir, symbolic),
            assertion(same_file(Source, PackDir))
        )
    ;   \+ option(git(false), Options),
        is_git_directory(Source)
    ->  remove_existing_pack(PackDir, Options),
        run_process(path(git), [clone, Source, PackDir], [])
    ;   prepare_pack_dir(PackDir, Options),
        copy_directory(Source, PackDir)
    ).
pack_unpack_from_local(Source, PackTopDir, Name, PackDir, Options) :-
    exists_file(Source),
    directory_file_path(PackTopDir, Name, PackDir),
    prepare_pack_dir(PackDir, Options),
    pack_unpack(Source, PackDir, Name, Options).

%!  pack_unpack(+SourceFile, +PackDir, +Pack, +Options)
%
%   Unpack an archive to the given package dir.
%
%   @tbd If library(archive) is  not  provided   we  could  check  for a
%   suitable external program such as `tar` or `unzip`.

:- if(exists_source(library(archive))).
pack_unpack(Source, PackDir, Pack, Options) :-
    ensure_loaded_archive,
    pack_archive_info(Source, Pack, _Info, StripOptions),
    prepare_pack_dir(PackDir, Options),
    archive_extract(Source, PackDir,
                    [ exclude(['._*'])          % MacOS resource forks
                    | StripOptions
                    ]).
:- else.
pack_unpack(_,_,_,_) :-
    existence_error(library, archive).
:- endif.

%!  pack_install_local(:Spec, +Dir, +Options) is det.
%
%   Install a number of packages in   a  local directory. This predicate
%   supports installing packages local  to   an  application rather than
%   globally.

pack_install_local(M:Gen, Dir, Options) :-
    findall(Pack-PackOptions, call(M:Gen, Pack, PackOptions), Pairs),
    pack_install_set(Pairs, Dir, Options).

pack_install_set(Pairs, Dir, Options) :-
    must_be(list(pair), Pairs),
    ensure_directory(Dir),
    partition(known_media, Pairs, Local, Remote),
    maplist(pack_options_to_versions, Local, LocalVersions),
    (   Remote == []
    ->  AllVersions = LocalVersions
    ;   pairs_keys(Remote, Packs),
        prolog_description(Properties),
        query_pack_server(versions(Packs, Properties), Result, Options),
        (   Result = true(RemoteVersions)
        ->  append(LocalVersions, RemoteVersions, AllVersions)
        ;   print_message(error, pack(query_failed(Result))),
            fail
        )
    ),
    local_packs(Dir, Existing),
    pack_resolve(Pairs, Existing, AllVersions, Plan0, Options),
    !,                                      % for now, only first plan
    maplist(hsts_info(Options), Plan0, Plan),
    Options1 = [pack_directory(Dir)|Options],
    download_plan(Pairs, Plan, PlanB, Options1),
    register_downloads(PlanB, Options),
    maplist(update_automatic, PlanB),
    build_plan(PlanB, Built, Options1),
    publish_download(PlanB, Options),
    work_done(Pairs, Plan, PlanB, Built, Options).

hsts_info(Options, Info0, Info) :-
    hsts(Info0.get(url), URL, Options),
    !,
    Info = Info0.put(url, URL).
hsts_info(_Options, Info, Info).

%!  known_media(+Pair) is semidet.
%
%   True when the options specify installation   from  a known media. If
%   that applies to all packs, there is no  need to query the server. We
%   first  download  and  unpack  the  known  media,  then  examine  the
%   requirements and, if necessary, go to the server to resolve these.

known_media(_-Options) :-
    option(url(_), Options).

%!  pack_resolve(+Pairs, +Existing, +Versions, -Plan, +Options) is det.
%
%   Generate an installation plan. Pairs is a list of Pack-Options pairs
%   that  specifies  the  desired  packages.  Existing   is  a  list  of
%   pack(Pack, i, Title, Version, URL) terms that represents the already
%   installed packages. Versions  is  obtained   from  the  server.  See
%   `pack.pl` from the web server for  details. On success, this results
%   in a Plan to satisfies  the  requirements.   The  plan  is a list of
%   packages to install with  their  location.   The  steps  satisfy the
%   partial  ordering  of  dependencies,  such   that  dependencies  are
%   installed before the dependents.  Options:
%
%     - upgrade(true)
%       When specified, we try to install the latest version of all
%       the packages.  Otherwise, we try to minimise the installation.

pack_resolve(Pairs, Existing, Versions, Plan, Options) :-
    insert_existing(Existing, Versions, AllVersions, Options),
    phrase(select_version(Pairs, AllVersions,
                          [ plan(PlanA),           % access to plan
                            dependency_for([])     % dependencies
                          | Options
                          ]),
           PlanA),
    mark_installed(PlanA, Existing, Plan).

%!  insert_existing(+Existing, +Available, -Candidates, +Options) is det.
%
%   Combine the already existing packages  with   the  ones  reported as
%   available by the server to a list of Candidates, where the candidate
%   of  each  package  is   ordered    according   by  preference.  When
%   upgrade(true) is specified, the existing is   merged into the set of
%   Available versions. Otherwise Existing is prepended to Available, so
%   it is selected as first.

:- det(insert_existing/4).
insert_existing(Existing, [], Versions, _Options) =>
    maplist(existing_to_versions, Existing, Versions).
insert_existing(Existing, [Pack-Versions|T0], AllPackVersions, Options),
    select(Installed, Existing, Existing2),
    Installed.pack == Pack =>
    can_upgrade(Installed, Versions, Installed2),
    insert_existing_(Installed2, Versions, AllVersions, Options),
    AllPackVersions = [Pack-AllVersions|T],
    insert_existing(Existing2, T0, T, Options).
insert_existing(Existing, [H|T0], AllVersions, Options) =>
    AllVersions = [H|T],
    insert_existing(Existing, T0, T, Options).

existing_to_versions(Installed, Pack-[Version-[Installed]]) :-
    Pack = Installed.pack,
    Version = Installed.version.

insert_existing_(Installed, Versions, AllVersions, Options) :-
    option(upgrade(true), Options),
    !,
    insert_existing_(Installed, Versions, AllVersions).
insert_existing_(Installed, Versions, AllVersions, _) :-
    AllVersions = [Installed.version-[Installed]|Versions].

insert_existing_(Installed, [H|T0], [H|T]) :-
    H = V0-_Infos,
    cmp_versions(>, V0, Installed.version),
    !,
    insert_existing_(Installed, T0, T).
insert_existing_(Installed, [H0|T], [H|T]) :-
    H0 = V0-Infos,
    V0 == Installed.version,
    !,
    H = V0-[Installed|Infos].
insert_existing_(Installed, Versions, All) :-
    All =  [Installed.version-[Installed]|Versions].

%!  can_upgrade(+Installed, +Versions, -Installed2) is det.
%
%   Add a `latest_version` key to Installed if its version is older than
%   the latest available version.

can_upgrade(Info, [Version-_|_], Info2) :-
    cmp_versions(>, Version, Info.version),
    !,
    Info2 = Info.put(latest_version, Version).
can_upgrade(Info, _, Info).

%!  mark_installed(+PlanA, +Existing, -Plan) is det.
%
%   Mark  already  up-to-date  packs  from  the   plan  and  add  a  key
%   `upgrade:true` to elements of PlanA  in   Existing  that are not the
%   same.

mark_installed([], _, []).
mark_installed([Info|T], Existing, Plan) :-
    (   member(Installed, Existing),
        Installed.pack == Info.pack
    ->  (   (   Installed.git == true
            ->  Info.git == true,
                Installed.hash == Info.hash
            ;   Version = Info.get(version)
            ->  Installed.version == Version
            )
        ->  Plan = [Info.put(keep, true)|PlanT]    % up-to-date
        ;   Plan = [Info.put(upgrade, Installed)|PlanT] % needs upgrade
        )
    ;   Plan = [Info|PlanT]                        % new install
    ),
    mark_installed(T, Existing, PlanT).

%!  select_version(+PackAndOptions, +Available, +Options)// is nondet.
%
%   True when the output is a list of   pack info dicts that satisfy the
%   installation requirements of PackAndOptions from  the packs known to
%   be Available.

select_version([], _, _) -->
    [].
select_version([Pack-PackOptions|More], Versions, Options) -->
    { memberchk(Pack-PackVersions, Versions),
      member(Version-Infos, PackVersions),
      compatible_version(Pack, Version, PackOptions),
      member(Info, Infos),
      pack_options_compatible_with_info(Info, PackOptions),
      pack_satisfies(Pack, Version, Info, Info2, PackOptions),
      all_downloads(PackVersions, Downloads)
    },
    add_to_plan(Info2.put(_{version: Version, all_downloads:Downloads}),
                Versions, Options),
    select_version(More, Versions, Options).
select_version([Pack-_PackOptions|_More], _Versions, _Options) -->
    { existence_error(pack, Pack) }.               % or warn and continue?

all_downloads(PackVersions, AllDownloads) :-
    aggregate_all(sum(Downloads),
                  ( member(_Version-Infos, PackVersions),
                    member(Info, Infos),
                    get_dict(downloads, Info, Downloads)
                  ),
                  AllDownloads).

add_requirements([], _, _) -->
    [].
add_requirements([H|T], Versions, Options) -->
    { is_prolog_token(H),
      !,
      prolog_satisfies(H)
    },
    add_requirements(T, Versions, Options).
add_requirements([H|T], Versions, Options) -->
    { member(Pack-PackVersions, Versions),
      member(Version-Infos, PackVersions),
      member(Info, Infos),
      (   Provides = @(Pack,Version)
      ;   member(Provides, Info.get(provides))
      ),
      satisfies_req(Provides, H),
      all_downloads(PackVersions, Downloads)
    },
    add_to_plan(Info.put(_{version: Version, all_downloads:Downloads}),
                Versions, Options),
    add_requirements(T, Versions, Options).

%!  add_to_plan(+Info, +Versions, +Options) is semidet.
%
%   Add Info to the plan. If an Info   about the same pack is already in
%   the plan, but this is a different version  of the pack, we must fail
%   as we cannot install two different versions of a pack.

add_to_plan(Info, _Versions, Options) -->
    { option(plan(Plan), Options),
      member_nonvar(Planned, Plan),
      Planned.pack == Info.pack,
      !,
      same_version(Planned, Info)                  % same pack, different version
    }.
add_to_plan(Info, _Versions, _Options) -->
    { member(Conflict, Info.get(conflicts)),
      is_prolog_token(Conflict),
      prolog_satisfies(Conflict),
      !,
      fail                                         % incompatible with this Prolog
    }.
add_to_plan(Info, _Versions, Options) -->
    { option(plan(Plan), Options),
      member_nonvar(Planned, Plan),
      info_conflicts(Info, Planned),               % Conflicts with a planned pack
      !,
      fail
    }.
add_to_plan(Info, Versions, Options) -->
    { select_option(dependency_for(Dep0), Options, Options1),
      Options2 = [dependency_for([Info.pack|Dep0])|Options1],
      (   Dep0 = [DepFor|_]
      ->  add_dependency_for(DepFor, Info, Info1)
      ;   Info1 = Info
      )
    },
    [Info1],
    add_requirements(Info.get(requires,[]), Versions, Options2).

add_dependency_for(Pack, Info, Info) :-
    Old = Info.get(dependency_for),
    !,
    b_set_dict(dependency_for, Info, [Pack|Old]).
add_dependency_for(Pack, Info0, Info) :-
    Info = Info0.put(dependency_for, [Pack]).

same_version(Info, Info) :-
    !.
same_version(Planned, Info) :-
    Hash = Planned.get(hash),
    Hash \== (-),
    !,
    Hash == Info.get(hash).
same_version(Planned, Info) :-
    Planned.get(version) == Info.get(version).

%!  info_conflicts(+Info1, +Info2) is semidet.
%
%   True if Info2 is in conflict with Info2. The relation is symetric.

info_conflicts(Info, Planned) :-
    info_conflicts_(Info, Planned),
    !.
info_conflicts(Info, Planned) :-
    info_conflicts_(Planned, Info),
    !.

info_conflicts_(Info, Planned) :-
    member(Conflict, Info.get(conflicts)),
    \+ is_prolog_token(Conflict),
    info_provides(Planned, Provides),
    satisfies_req(Provides, Conflict),
    !.

info_provides(Info, Provides) :-
    (   Provides = Info.pack@Info.version
    ;   member(Provides, Info.get(provides))
    ).

%!  pack_satisfies(+Pack, +Version, +Info0, -Info, +Options) is semidet.
%
%   True if Pack@Version  with  Info   satisfies  the  pack installation
%   options provided by Options.

pack_satisfies(_Pack, _Version, Info0, Info, Options) :-
    option(commit('HEAD'), Options),
    !,
    Info0.get(git) == true,
    Info = Info0.put(commit, 'HEAD').
pack_satisfies(_Pack, _Version, Info, Info, Options) :-
    option(commit(Commit), Options),
    !,
    Commit == Info.get(hash).
pack_satisfies(Pack, Version, Info, Info, Options) :-
    option(version(ReqVersion), Options),
    !,
    satisfies_version(Pack, Version, ReqVersion).
pack_satisfies(_Pack, _Version, Info, Info, _Options).

%!  satisfies_version(+Pack, +PackVersion, +RequiredVersion) is semidet.

satisfies_version(Pack, Version, ReqVersion) :-
    catch(require_version(pack(Pack), Version, ReqVersion),
          error(version_error(pack(Pack), Version, ReqVersion),_),
          fail).

%!  satisfies_req(+Provides, +Required) is semidet.
%
%   Check a token requirements.

satisfies_req(Token, Token) => true.
satisfies_req(@(Token,_), Token) => true.
satisfies_req(@(Token,PrvVersion), Req), cmp(Req, Token, Cmp, ReqVersion) =>
	cmp_versions(Cmp, PrvVersion, ReqVersion).
satisfies_req(_,_) => fail.

cmp(Token  < Version, Token, <,	 Version).
cmp(Token =< Version, Token, =<, Version).
cmp(Token =  Version, Token, =,	 Version).
cmp(Token == Version, Token, ==, Version).
cmp(Token >= Version, Token, >=, Version).
cmp(Token >  Version, Token, >,	 Version).

%!  pack_options_to_versions(+PackOptionsPair, -Versions) is det.
%
%   Create an available  package  term  from   Pack  and  Options  if it
%   contains a url(URL) option. This allows installing packages that are
%   not known to the server. In most cases, the URL will be a git URL or
%   the URL to download an archive. It can  also be a ``file://`` url to
%   install from a local archive.
%
%   The   first   clause   deals    with     a    wildcard    URL.   See
%   pack_default_options/4, case (7).

:- det(pack_options_to_versions/2).
pack_options_to_versions(Pack-PackOptions, Pack-Versions) :-
    option(versions(Available), PackOptions), !,
    maplist(version_url_info(Pack, PackOptions), Available, Versions).
pack_options_to_versions(Pack-PackOptions, Pack-[Version-[Info]]) :-
    option(url(URL), PackOptions),
    findall(Prop, option_info_prop(PackOptions, Prop), Pairs),
    dict_create(Info, #,
                [ pack-Pack,
                  url-URL
                | Pairs
                ]),
    Version = Info.get(version, '0.0.0').

version_url_info(Pack, PackOptions, Version-URL, Version-[Info]) :-
    findall(Prop,
            ( option_info_prop(PackOptions, Prop),
              Prop \= version-_
            ),
            Pairs),
    dict_create(Info, #,
                [ pack-Pack,
                  url-URL,
                  version-Version
                | Pairs
                ]).

option_info_prop(PackOptions, Prop-Value) :-
    option_info(Prop),
    Opt =.. [Prop,Value],
    option(Opt, PackOptions).

option_info(git).
option_info(hash).
option_info(version).
option_info(branch).
option_info(link).

%!  compatible_version(+Pack, +Version, +Options) is semidet.
%
%   Fails if Options demands a  version   and  Version is not compatible
%   with Version.

compatible_version(Pack, Version, PackOptions) :-
    option(version(ReqVersion), PackOptions),
    !,
    satisfies_version(Pack, Version, ReqVersion).
compatible_version(_, _, _).

%!  pack_options_compatible_with_info(+Info, +PackOptions) is semidet.
%
%   Ignore information from the server  that   is  incompatible with the
%   request.

pack_options_compatible_with_info(Info, PackOptions) :-
    findall(Prop, option_info_prop(PackOptions, Prop), Pairs),
    dict_create(Dict, _, Pairs),
    Dict >:< Info.

%!  download_plan(+Targets, +Plan, +Options) is semidet.
%
%   Download or update all packages from Plan. We   need to do this as a
%   first  step  because  we  may    not  have  (up-to-date)  dependency
%   information about all packs. For example, a pack may be installed at
%   the git HEAD revision that is not yet   know to the server or it may
%   be installed from a url that is not known at all at the server.

download_plan(_Targets, Plan, Plan, _Options) :-
    exclude(installed, Plan, []),
    !.
download_plan(Targets, Plan0, Plan, Options) :-
    confirm(download_plan(Plan0), yes, Options),
    maplist(download_from_info(Options), Plan0, Plan1),
    plan_unsatisfied_dependencies(Plan1, Deps),
    (   Deps == []
    ->  Plan = Plan1
    ;   print_message(informational, pack(new_dependencies(Deps))),
        prolog_description(Properties),
        query_pack_server(versions(Deps, Properties), Result, []),
        (   Result = true(Versions)
        ->  pack_resolve(Targets, Plan1, Versions, Plan2, Options),
            !,
            download_plan(Targets, Plan2, Plan, Options)
        ;   print_message(error, pack(query_failed(Result))),
            fail
        )
    ).

%!  plan_unsatisfied_dependencies(+Plan, -Deps) is det.
%
%   True when Deps is a list of dependency   tokens  in Plan that is not
%   satisfied.

plan_unsatisfied_dependencies(Plan, Deps) :-
    phrase(plan_unsatisfied_dependencies(Plan, Plan), Deps).

plan_unsatisfied_dependencies([], _) -->
    [].
plan_unsatisfied_dependencies([Info|Infos], Plan) -->
    { Deps = Info.get(requires) },
    plan_unsatisfied_requirements(Deps, Plan),
    plan_unsatisfied_dependencies(Infos, Plan).

plan_unsatisfied_requirements([], _) -->
    [].
plan_unsatisfied_requirements([H|T], Plan) -->
    { is_prolog_token(H),           % Can this fail?
      prolog_satisfies(H)
    },
    !,
    plan_unsatisfied_requirements(T, Plan).
plan_unsatisfied_requirements([H|T], Plan) -->
    { member(Info, Plan),
      (   (   Version = Info.get(version)
          ->  Provides = @(Info.get(pack), Version)
          ;   Provides = Info.get(pack)
          )
      ;   member(Provides, Info.get(provides))
      ),
      satisfies_req(Provides, H)
    }, !,
    plan_unsatisfied_requirements(T, Plan).
plan_unsatisfied_requirements([H|T], Plan) -->
    [H],
    plan_unsatisfied_requirements(T, Plan).


%!  build_plan(+Plan, -Built, +Options) is det.
%
%   Run post installation steps.  We   build  dependencies  before their
%   dependents, so we first do a topological  sort on the packs based on
%   the pack dependencies.

build_plan(Plan, Ordered, Options) :-
    maplist(decide_autoload_pack(Options), Plan, Plan1),
    partition(needs_rebuild_from_info(Options), Plan1, ToBuild, NoBuild),
    maplist(attach_from_info(Options), NoBuild),
    (   ToBuild == []
    ->  post_install_autoload(NoBuild),
        Ordered = []
    ;   order_builds(ToBuild, Ordered),
        confirm(build_plan(Ordered), yes, Options),
        maplist(exec_plan_rebuild_step(Options), Ordered)
    ).

%!  needs_rebuild_from_info(+Options, +Info) is semidet.
%
%   True when we need to rebuilt the pack.

needs_rebuild_from_info(Options, Info) :-
    PackDir = Info.installed,
    is_foreign_pack(PackDir, _),
    \+ is_built(PackDir, Options).

%!  is_built(+PackDir, +Options) is semidet.
%
%   True if the pack in PackDir has been built.
%
%   @tbd We now verify it was built by   the exact same version. That is
%   normally an overkill.

is_built(PackDir, _Options) :-
    current_prolog_flag(arch, Arch),
    prolog_version_dotted(Version), % Major.Minor.Patch
    pack_status_dir(PackDir, built(Arch, Version, _)).

%!  order_builds(+ToBuild, -Ordered) is det.
%
%   Order the build  processes  by   building  dependencies  before  the
%   packages that rely on them as they may need them during the build.

order_builds(ToBuild, Ordered) :-
    findall(Pack-Dependent, dep_edge(ToBuild, Pack, Dependent), Edges),
    maplist(get_dict(pack), ToBuild, Packs),
    vertices_edges_to_ugraph(Packs, Edges, Graph),
    ugraph_layers(Graph, Layers),
    append(Layers, PackNames),
    maplist(pack_info_from_name(ToBuild), PackNames, Ordered).

%!  dep_edge(+Infos, -Pack, -Dependent) is nondet.
%
%   True when Pack needs to be installed   as a dependency of Dependent.
%   Both Pack and Dependent are pack _names_. I.e., this implies that we
%   must build Pack _before_ Dependent.

dep_edge(Infos, Pack, Dependent) :-
    member(Info, Infos),
    Pack = Info.pack,
    member(Dependent, Info.get(dependency_for)),
    (   member(DepInfo, Infos),
        DepInfo.pack == Dependent
    ->  true
    ).

:- det(pack_info_from_name/3).
pack_info_from_name(Infos, Pack, Info) :-
    member(Info, Infos),
    Info.pack == Pack,
    !.

%!  exec_plan_rebuild_step(+Options, +Info) is det.
%
%   Execute the rebuild steps for the given Info.

exec_plan_rebuild_step(Options, Info) :-
    print_message(informational, pack(build(Info.pack, Info.installed))),
    pack_post_install(Info, Options),
    attach_from_info(Options, Info).

%!  attach_from_info(+Options, +Info) is det.
%
%   Make the package visible.

attach_from_info(_Options, Info) :-
    Info.get(keep) == true,
    !.
attach_from_info(Options, Info) :-
    (   option(pack_directory(_Parent), Options)
    ->  pack_attach(Info.installed, [duplicate(replace)])
    ;   pack_attach(Info.installed, [])
    ).

%!  download_from_info(+Options, +Info0, -Info) is det.
%
%   Download a package guided by Info. Note   that this does __not__ run
%   any scripts. This implies that dependencies do not matter and we can
%   proceed in any order. This is important  because we may use packages
%   at their git HEAD, which implies  that requirements may be different
%   from what is in the Info terms.

download_from_info(Options, Info0, Info), option(dryrun(true), Options) =>
    print_term(Info0, [nl(true)]),
    Info = Info0.
download_from_info(_Options, Info0, Info), installed(Info0) =>
    Info = Info0.
download_from_info(_Options, Info0, Info),
    _{upgrade:OldInfo, git:true} :< Info0,
    is_git_directory(OldInfo.installed) =>
    PackDir = OldInfo.installed,
    git_checkout_version(PackDir, [commit(Info0.hash)]),
    reload_info(PackDir, Info0, Info).
download_from_info(Options, Info0, Info),
    _{upgrade:OldInfo} :< Info0 =>
    PackDir = OldInfo.installed,
    detach_pack(OldInfo.pack, PackDir),
    delete_directory_and_contents(PackDir),
    del_dict(upgrade, Info0, _, Info1),
    download_from_info(Options, Info1, Info).
download_from_info(Options, Info0, Info),
    _{url:URL, git:true} :< Info0, \+ have_git =>
    git_archive_url(URL, Archive, Options),
    download_from_info([git_url(URL)|Options],
                       Info0.put(_{ url:Archive,
                                    git:false,
                                    git_url:URL
                                  }),
                       Info1),
                                % restore the hash to register the download.
    (   Info1.get(version) == Info0.get(version),
        Hash = Info0.get(hash)
    ->  Info = Info1.put(hash, Hash)
    ;   Info = Info1
    ).
download_from_info(Options, Info0, Info),
    _{url:URL} :< Info0 =>
    select_option(pack_directory(Dir), Options, Options1),
    select_option(version(_), Options1, Options2, _),
    download_info_extra(Info0, InstallOptions, Options2),
    pack_download_from_url(URL, Dir, Info0.pack,
                           [ interactive(false),
                             pack_dir(PackDir)
                           | InstallOptions
                           ]),
    reload_info(PackDir, Info0, Info).

download_info_extra(Info, [git(true),commit(Hash)|Options], Options) :-
    Info.get(git) == true,
    !,
    Hash = Info.get(commit, 'HEAD').
download_info_extra(Info, [link(true)|Options], Options) :-
    Info.get(link) == true,
    !.
download_info_extra(_, Options, Options).

installed(Info) :-
    _ = Info.get(installed).

detach_pack(Pack, PackDir) :-
    (   current_pack(Pack, PackDir)
    ->  '$pack_detach'(Pack, PackDir)
    ;   true
    ).

%!  reload_info(+PackDir, +Info0, -Info) is det.
%
%   Update the requires and provides metadata. Info0 is what we got from
%   the server, but the package may be   different  as we may have asked
%   for the git HEAD or the package URL   may not have been known by the
%   server at all.

reload_info(_PackDir, Info, Info) :-
    _ = Info.get(installed),	% we read it from the package
    !.
reload_info(PackDir, Info0, Info) :-
    local_pack_info(PackDir, Info1),
    Info = Info0.put(installed, PackDir)
                .put(downloaded, Info0.url)
                .put(Info1).

%!  work_done(+Targets, +Plan, +PlanB, +Built, +Options) is det.
%
%   Targets has successfully been installed  and   the  packs Built have
%   successfully ran their build scripts.

work_done(_, _, _, _, Options),
    option(silent(true), Options) =>
    true.
work_done(Targets, Plan, Plan, [], _Options) =>
    convlist(can_upgrade_target(Plan), Targets, CanUpgrade),
    (   CanUpgrade == []
    ->  pairs_keys(Targets, Packs),
        print_message(informational, pack(up_to_date(Packs)))
    ;   print_message(informational, pack(installed_can_upgrade(CanUpgrade)))
    ).
work_done(_, _, _, _, _) =>
    true.

can_upgrade_target(Plan, Pack-_, Info) =>
    member(Info, Plan),
    Info.pack == Pack,
    !,
    _ = Info.get(latest_version).

%!  local_packs(+Dir, -Packs) is det.
%
%   True when Packs  is  a  list   with  information  for  all installed
%   packages.

local_packs(Dir, Packs) :-
    findall(Pack, pack_in_subdir(Dir, Pack), Packs).

pack_in_subdir(Dir, Info) :-
    directory_member(Dir, PackDir,
                     [ file_type(directory),
                       hidden(false)
                     ]),
    local_pack_info(PackDir, Info).

local_pack_info(PackDir,
                #{ pack: Pack,
                   version: Version,
                   title: Title,
                   hash: Hash,
                   url: URL,
                   git: IsGit,
                   requires: Requires,
                   provides: Provides,
                   conflicts: Conflicts,
                   installed: PackDir
                 }) :-
    directory_file_path(PackDir, 'pack.pl', MetaFile),
    exists_file(MetaFile),
    file_base_name(PackDir, DirName),
    findall(Term, pack_dir_info(PackDir, _, Term), Info),
    option(pack(Pack), Info, DirName),
    option(title(Title), Info, '<no title>'),
    option(version(Version), Info, '<no version>'),
    option(download(URL), Info, '<no download url>'),
    findall(Req, member(requires(Req), Info), Requires),
    findall(Prv, member(provides(Prv), Info), Provides),
    findall(Cfl, member(conflicts(Cfl), Info), Conflicts),
    (   have_git,
        is_git_directory(PackDir)
    ->  git_hash(Hash, [directory(PackDir)]),
        IsGit = true
    ;   Hash = '-',
        IsGit = false
    ).


		 /*******************************
		 *        PROLOG VERSIONS	*
		 *******************************/

%!  prolog_description(-Description) is det.
%
%   Provide a description of the running Prolog system. Version terms:
%
%     - prolog(Dialect, Version)
%
%   @tbd:   establish   a   language    for     features.    Sync   with
%   library(prolog_versions)

prolog_description([prolog(swi(Version))]) :-
    prolog_version(Version).

prolog_version(Version) :-
    current_prolog_flag(version_git, Version),
    !.
prolog_version(Version) :-
    prolog_version_dotted(Version).

prolog_version_dotted(Version) :-
    current_prolog_flag(version_data, swi(Major, Minor, Patch, _)),
    VNumbers = [Major, Minor, Patch],
    atomic_list_concat(VNumbers, '.', Version).

%!  is_prolog_token(+Token) is semidet.
%
%   True when Token describes a property of the target Prolog
%   system.

is_prolog_token(Token), cmp(Token, prolog, _Cmp, _Version) => true.
is_prolog_token(prolog:Feature), atom(Feature) => true.
is_prolog_token(prolog:Feature), flag_value_feature(Feature, _Flag, _Value) =>
    true.
is_prolog_token(_) => fail.

%!  prolog_satisfies(+Token) is semidet.
%
%   True when the  running  Prolog   system  satisfies  token. Processes
%   requires(Token) terms for
%
%     - prolog Cmp Version
%       Demand a Prolog version (range).
%     - prolog:Flag
%     - prolog:Flag(Value)
%     - prolog:library(Lib)
%
%   @see require_prolog_version/2.

prolog_satisfies(Token), cmp(Token, prolog, Cmp, ReqVersion) =>
    prolog_version(CurrentVersion),
    cmp_versions(Cmp, CurrentVersion, ReqVersion).
prolog_satisfies(prolog:library(Lib)), atom(Lib) =>
    exists_source(library(Lib)).
prolog_satisfies(prolog:Feature), atom(Feature) =>
    current_prolog_flag(Feature, true).
prolog_satisfies(prolog:Feature), flag_value_feature(Feature, Flag, Value) =>
    current_prolog_flag(Flag, Value).

flag_value_feature(Feature, Flag, Value) :-
    compound(Feature),
    compound_name_arguments(Feature, Flag, [Value]),
    atom(Flag).


                 /*******************************
                 *             INFO             *
                 *******************************/

%!  pack_archive_info(+Archive, +Pack, -Info, -Strip)
%
%   True when Archive archives Pack. Info  is unified with the terms
%   from pack.pl in the  pack  and   Strip  is  the strip-option for
%   archive_extract/3.
%
%   Requires library(archive), which is lazily loaded when needed.
%
%   @error  existence_error(pack_file, 'pack.pl') if the archive
%           doesn't contain pack.pl
%   @error  Syntax errors if pack.pl cannot be parsed.

:- if(exists_source(library(archive))).
ensure_loaded_archive :-
    current_predicate(archive_open/3),
    !.
ensure_loaded_archive :-
    use_module(library(archive)).

pack_archive_info(Archive, Pack, [archive_size(Bytes)|Info], Strip) :-
    ensure_loaded_archive,
    size_file(Archive, Bytes),
    setup_call_cleanup(
        archive_open(Archive, Handle, []),
        (   repeat,
            (   archive_next_header(Handle, InfoFile)
            ->  true
            ;   !, fail
            )
        ),
        archive_close(Handle)),
    file_base_name(InfoFile, 'pack.pl'),
    atom_concat(Prefix, 'pack.pl', InfoFile),
    strip_option(Prefix, Pack, Strip),
    setup_call_cleanup(
        archive_open_entry(Handle, Stream),
        read_stream_to_terms(Stream, Info),
        close(Stream)),
    !,
    must_be(ground, Info),
    maplist(valid_term(pack_info_term), Info).
:- else.
pack_archive_info(_, _, _, _) :-
    existence_error(library, archive).
:- endif.
pack_archive_info(_, _, _, _) :-
    existence_error(pack_file, 'pack.pl').

strip_option('', _, []) :- !.
strip_option('./', _, []) :- !.
strip_option(Prefix, Pack, [remove_prefix(Prefix)]) :-
    atom_concat(PrefixDir, /, Prefix),
    file_base_name(PrefixDir, Base),
    (   Base == Pack
    ->  true
    ;   pack_version_file(Pack, _, Base)
    ->  true
    ;   \+ sub_atom(PrefixDir, _, _, _, /)
    ).

read_stream_to_terms(Stream, Terms) :-
    read(Stream, Term0),
    read_stream_to_terms(Term0, Stream, Terms).

read_stream_to_terms(end_of_file, _, []) :- !.
read_stream_to_terms(Term0, Stream, [Term0|Terms]) :-
    read(Stream, Term1),
    read_stream_to_terms(Term1, Stream, Terms).


%!  pack_git_info(+GitDir, -Hash, -Info) is det.
%
%   Retrieve info from a cloned git   repository  that is compatible
%   with pack_archive_info/4.

pack_git_info(GitDir, Hash, [git(true), installed_size(Bytes)|Info]) :-
    exists_directory(GitDir),
    !,
    git_ls_tree(Entries, [directory(GitDir)]),
    git_hash(Hash, [directory(GitDir)]),
    maplist(arg(4), Entries, Sizes),
    sum_list(Sizes, Bytes),
    dir_metadata(GitDir, Info).

dir_metadata(GitDir, Info) :-
    directory_file_path(GitDir, 'pack.pl', InfoFile),
    read_file_to_terms(InfoFile, Info, [encoding(utf8)]),
    maplist(valid_term(pack_info_term), Info).

%!  download_file_sanity_check(+Archive, +Pack, +Info) is semidet.
%
%   Perform basic sanity checks on DownloadFile

download_file_sanity_check(Archive, Pack, Info) :-
    info_field(name(PackName), Info),
    info_field(version(PackVersion), Info),
    pack_version_file(PackFile, FileVersion, Archive),
    must_match([Pack, PackName, PackFile], name),
    must_match([PackVersion, FileVersion], version).

info_field(Field, Info) :-
    memberchk(Field, Info),
    ground(Field),
    !.
info_field(Field, _Info) :-
    functor(Field, FieldName, _),
    print_message(error, pack(missing(FieldName))),
    fail.

must_match(Values, _Field) :-
    sort(Values, [_]),
    !.
must_match(Values, Field) :-
    print_message(error, pack(conflict(Field, Values))),
    fail.


                 /*******************************
                 *         INSTALLATION         *
                 *******************************/

%!  prepare_pack_dir(+Dir, +Options)
%
%   Prepare for installing the package into  Dir. This
%
%     - If the directory exist and is empty, done.
%     - Else if the directory exists, remove the directory and recreate
%       it. Note that if the directory is a symlink this just deletes
%       the link.
%     - Else if some entry (file, link, ...) exists, delete it and
%       create a new directory.
%     - Else create the directory.

prepare_pack_dir(Dir, Options) :-
    exists_directory(Dir),
    !,
    (   empty_directory(Dir)
    ->  true
    ;   remove_existing_pack(Dir, Options)
    ->  make_directory(Dir)
    ).
prepare_pack_dir(Dir, _) :-
    (   read_link(Dir, _, _)
    ;   access_file(Dir, exist)
    ),
    !,
    delete_file(Dir),
    make_directory(Dir).
prepare_pack_dir(Dir, _) :-
    make_directory(Dir).

%!  empty_directory(+Directory) is semidet.
%
%   True if Directory is empty (holds no files or sub-directories).

empty_directory(Dir) :-
    \+ ( directory_files(Dir, Entries),
         member(Entry, Entries),
         \+ special(Entry)
       ).

special(.).
special(..).

%!  remove_existing_pack(+PackDir, +Options) is semidet.
%
%   Remove  a  possible  existing   pack    directory   if   the  option
%   upgrade(true) is present. This is used to remove an old installation
%   before unpacking a new archive, copy or   link  a directory with the
%   new contents.

remove_existing_pack(PackDir, Options) :-
    exists_directory(PackDir),
    !,
    (   (   option(upgrade(true), Options)
        ;   confirm(remove_existing_pack(PackDir), yes, Options)
        )
    ->  delete_directory_and_contents(PackDir)
    ;   print_message(error, pack(directory_exists(PackDir))),
        fail
    ).
remove_existing_pack(_, _).

%!  pack_download_from_url(+URL, +PackDir, +Pack, +Options)
%
%   Download a package from a remote   source.  For git repositories, we
%   simply clone. Archives are downloaded. Options:
%
%     - git(true)
%       Assume URL refers to a git repository.
%     - pack_dir(-Dir)
%       Dir is unified with the location where the pack is installed.
%
%   @tbd We currently  use  the  built-in   HTTP  client.  For  complete
%   coverage, we should consider using  an   external  (e.g., `curl`) if
%   available.

pack_download_from_url(URL, PackTopDir, Pack, Options) :-
    option(git(true), Options),
    !,
    directory_file_path(PackTopDir, Pack, PackDir),
    prepare_pack_dir(PackDir, Options),
    (   option(branch(Branch), Options)
    ->  Extra = ['--branch', Branch]
    ;   Extra = []
    ),
    run_process(path(git), [clone, URL, PackDir|Extra], []),
    git_checkout_version(PackDir, [update(false)|Options]),
    option(pack_dir(PackDir), Options, _).
pack_download_from_url(URL0, PackTopDir, Pack, Options) :-
    download_url(URL0),
    !,
    hsts(URL0, URL, Options),
    directory_file_path(PackTopDir, Pack, PackDir),
    prepare_pack_dir(PackDir, Options),
    pack_download_dir(PackTopDir, DownLoadDir),
    download_file(URL, Pack, DownloadBase, Options),
    directory_file_path(DownLoadDir, DownloadBase, DownloadFile),
    (   option(insecure(true), Options, false)
    ->  TLSOptions = [cert_verify_hook(ssl_verify)]
    ;   TLSOptions = []
    ),
    print_message(informational, pack(download(begin, Pack, URL, DownloadFile))),
    setup_call_cleanup(
        http_open(URL, In, TLSOptions),
        setup_call_cleanup(
            open(DownloadFile, write, Out, [type(binary)]),
            copy_stream_data(In, Out),
            close(Out)),
        close(In)),
    print_message(informational, pack(download(end, Pack, URL, DownloadFile))),
    pack_archive_info(DownloadFile, Pack, Info, _),
    (   option(git_url(GitURL), Options)
    ->  Origin = GitURL                 % implicit download from git.
    ;   download_file_sanity_check(DownloadFile, Pack, Info),
        Origin = URL
    ),
    pack_unpack_from_local(DownloadFile, PackTopDir, Pack, PackDir, Options),
    pack_assert(PackDir, archive(DownloadFile, Origin)),
    option(pack_dir(PackDir), Options, _).
pack_download_from_url(URL, PackTopDir, Pack, Options) :-
    local_uri_file_name(URL, File),
    !,
    pack_unpack_from_local(File, PackTopDir, Pack, PackDir, Options),
    pack_assert(PackDir, archive(File, URL)),
    option(pack_dir(PackDir), Options, _).
pack_download_from_url(URL, _PackTopDir, _Pack, _Options) :-
    domain_error(url, URL).

%!  git_checkout_version(+PackDir, +Options) is det.
%
%   Given a checked out version of a repository, put the repo at the
%   desired version.  Options:
%
%     - commit(+Commit)
%       Target commit or `'HEAD'`.  If `'HEAD'`, get the HEAD of the
%       explicit (option branch(Branch)), current or default branch. If
%       the commit is a hash and it is the tip of a branch, checkout
%       this branch. Else simply checkout the hash.
%     - branch(+Branch)
%       Used with commit('HEAD').
%     - version(+Version)
%       Checkout a tag.  If there is a tag matching Version use that,
%       otherwise try to find a tag that ends with Version and demand
%       the prefix to be letters, optionally followed by a dash or
%       underscore.  Examples: 2.1, V2.1, v_2.1.
%     - update(true)
%       If none of the above is given update the repo.  If it is on
%       a branch, _pull_.  Else, put it on the default branch and
%       pull.

git_checkout_version(PackDir, Options) :-
    option(commit('HEAD'), Options),
    option(branch(Branch), Options),
    !,
    git_ensure_on_branch(PackDir, Branch),
    run_process(path(git), ['-C', PackDir, pull], []).
git_checkout_version(PackDir, Options) :-
    option(commit('HEAD'), Options),
    git_current_branch(_, [directory(PackDir)]),
    !,
    run_process(path(git), ['-C', PackDir, pull], []).
git_checkout_version(PackDir, Options) :-
    option(commit('HEAD'), Options),
    !,
    git_default_branch(Branch, [directory(PackDir)]),
    git_ensure_on_branch(PackDir, Branch),
    run_process(path(git), ['-C', PackDir, pull], []).
git_checkout_version(PackDir, Options) :-
    option(commit(Hash), Options),
    run_process(path(git), ['-C', PackDir, fetch], []),
    git_branches(Branches, [contains(Hash), directory(PackDir)]),
    git_process_output(['-C', PackDir, 'rev-parse' | Branches],
                       read_lines_to_atoms(Commits),
                       []),
    nth1(I, Commits, Hash),
    nth1(I, Branches, Branch),
    !,
    git_ensure_on_branch(PackDir, Branch).
git_checkout_version(PackDir, Options) :-
    option(commit(Hash), Options),
    !,
    run_process(path(git), ['-C', PackDir, checkout, '--quiet', Hash], []).
git_checkout_version(PackDir, Options) :-
    option(version(Version), Options),
    !,
    git_tags(Tags, [directory(PackDir)]),
    (   memberchk(Version, Tags)
    ->  Tag = Version
    ;   member(Tag, Tags),
        sub_atom(Tag, B, _, 0, Version),
        sub_atom(Tag, 0, B, _, Prefix),
        version_prefix(Prefix)
    ->  true
    ;   existence_error(version_tag, Version)
    ),
    run_process(path(git), ['-C', PackDir, checkout, Tag], []).
git_checkout_version(_PackDir, Options) :-
    option(fresh(true), Options),
    !.
git_checkout_version(PackDir, _Options) :-
    git_current_branch(_, [directory(PackDir)]),
    !,
    run_process(path(git), ['-C', PackDir, pull], []).
git_checkout_version(PackDir, _Options) :-
    git_default_branch(Branch, [directory(PackDir)]),
    git_ensure_on_branch(PackDir, Branch),
    run_process(path(git), ['-C', PackDir, pull], []).

%!  git_ensure_on_branch(+PackDir, +Branch) is det.
%
%   Ensure PackDir is on Branch.

git_ensure_on_branch(PackDir, Branch) :-
    git_current_branch(Branch, [directory(PackDir)]),
    !.
git_ensure_on_branch(PackDir, Branch) :-
    run_process(path(git), ['-C', PackDir, checkout, Branch], []).

read_lines_to_atoms(Atoms, In) :-
    read_line_to_string(In, Line),
    (   Line == end_of_file
    ->  Atoms = []
    ;   atom_string(Atom, Line),
        Atoms = [Atom|T],
        read_lines_to_atoms(T, In)
    ).

version_prefix(Prefix) :-
    atom_codes(Prefix, Codes),
    phrase(version_prefix, Codes).

version_prefix -->
    [C],
    { code_type(C, alpha) },
    !,
    version_prefix.
version_prefix -->
    "-".
version_prefix -->
    "_".
version_prefix -->
    "".

%!  download_file(+URL, +Pack, -File, +Options) is det.
%
%   Determine the file into which  to   download  URL. The second clause
%   deals with GitHub downloads from a release tag.

download_file(URL, Pack, File, Options) :-
    option(version(Version), Options),
    !,
    file_name_extension(_, Ext, URL),
    format(atom(File), '~w-~w.~w', [Pack, Version, Ext]).
download_file(URL, Pack, File, _) :-
    file_base_name(URL,Basename),
    no_int_file_name_extension(Tag,Ext,Basename),
    tag_version(Tag,Version),
    !,
    format(atom(File0), '~w-~w', [Pack, Version]),
    file_name_extension(File0, Ext, File).
download_file(URL, _, File, _) :-
    file_base_name(URL, File).

%!  pack_url_file(+URL, -File) is det.
%
%   True if File is a unique  id   for  the referenced pack and version.
%   Normally, that is simply the base  name, but GitHub archives destroy
%   this picture. Needed by the pack manager in the web server.

:- public pack_url_file/2.
pack_url_file(URL, FileID) :-
    github_release_url(URL, Pack, Version),
    !,
    download_file(URL, Pack, FileID, [version(Version)]).
pack_url_file(URL, FileID) :-
    file_base_name(URL, FileID).

%   ssl_verify(+SSL, +ProblemCert, +AllCerts, +FirstCert, +Error)
%
%   Used if insecure(true)  is  given   to  pack_install/2.  Accepts any
%   certificate.

:- public ssl_verify/5.
ssl_verify(_SSL,
           _ProblemCertificate, _AllCertificates, _FirstCertificate,
           _Error).

pack_download_dir(PackTopDir, DownLoadDir) :-
    directory_file_path(PackTopDir, 'Downloads', DownLoadDir),
    (   exists_directory(DownLoadDir)
    ->  true
    ;   make_directory(DownLoadDir)
    ),
    (   access_file(DownLoadDir, write)
    ->  true
    ;   permission_error(write, directory, DownLoadDir)
    ).

%!  download_url(@URL) is semidet.
%
%   True if URL looks like a URL we   can  download from. Noet that urls
%   like ``ftp://`` are also download  URLs,   but  _we_ cannot download
%   from them.

download_url(URL) :-
    url_scheme(URL, Scheme),
    download_scheme(Scheme).

url_scheme(URL, Scheme) :-
    atom(URL),
    uri_components(URL, Components),
    uri_data(scheme, Components, Scheme0),
    atom(Scheme0),
    Scheme = Scheme0.

download_scheme(http).
download_scheme(https).

%!  hsts(+URL0, -URL, +Options) is det.
%
%   HSTS (HTTP Strict Transport Security) is   standard by which means a
%   site asks to always use HTTPS. For  SWI-Prolog packages we now force
%   using HTTPS for all  downloads.  This   may  be  overrules using the
%   option insecure(true), which  may  also  be   used  to  disable  TLS
%   certificate  checking.  Note  that  the   pack  integrity  is  still
%   protected by its SHA1 hash.

hsts(URL0, URL, Options) :-
    option(insecure(true), Options, false),
    !,
    URL = URL0.
hsts(URL0, URL, _Options) :-
    url_scheme(URL0, http),
    !,
    uri_edit(scheme(https), URL0, URL).
hsts(URL, URL, _Options).


%!  pack_post_install(+Info, +Options) is det.
%
%   Process post installation work.  Steps:
%
%     - Create foreign resources
%     - Register directory as autoload library
%     - Attach the package

pack_post_install(Info, Options) :-
    Pack = Info.pack,
    PackDir = Info.installed,
    post_install_foreign(Pack, PackDir, Options),
    post_install_autoload(Info),
    pack_attach(PackDir, [duplicate(warning)]).

%!  pack_rebuild is det.
%!  pack_rebuild(+Pack) is det.
%
%   Rebuild  possible  foreign  components  of    Pack.   The  predicate
%   pack_rebuild/0 rebuilds all registered packs.

pack_rebuild :-
    forall(current_pack(Pack),
           ( print_message(informational, pack(rebuild(Pack))),
             pack_rebuild(Pack)
           )).

pack_rebuild(Pack) :-
    current_pack(Pack, PackDir),
    !,
    post_install_foreign(Pack, PackDir, [rebuild(true)]).
pack_rebuild(Pack) :-
    unattached_pack(Pack, PackDir),
    !,
    post_install_foreign(Pack, PackDir, [rebuild(true)]).
pack_rebuild(Pack) :-
    existence_error(pack, Pack).

unattached_pack(Pack, BaseDir) :-
    directory_file_path(Pack, 'pack.pl', PackFile),
    absolute_file_name(pack(PackFile), PackPath,
                       [ access(read),
                         file_errors(fail)
                       ]),
    file_directory_name(PackPath, BaseDir).



%!  post_install_foreign(+Pack, +PackDir, +Options) is det.
%
%   Install foreign parts of the package.  Options:
%
%     - rebuild(When)
%       Determine when to rebuild.  Possible values:
%       - if_absent
%         Only rebuild if we have no existing foreign library.  This
%         is the default.
%       - true
%         Always rebuild.

post_install_foreign(Pack, PackDir, Options) :-
    is_foreign_pack(PackDir, _),
    !,
    (   pack_info_term(PackDir, pack_version(Version))
    ->  true
    ;   Version = 1
    ),
    option(rebuild(Rebuild), Options, if_absent),
    current_prolog_flag(arch, Arch),
    prolog_version_dotted(PrologVersion),
    (   Rebuild == if_absent,
        foreign_present(PackDir, Arch)
    ->  print_message(informational, pack(kept_foreign(Pack, Arch))),
        (   pack_status_dir(PackDir, built(Arch, _, _))
        ->  true
        ;   pack_assert(PackDir, built(Arch, PrologVersion, downloaded))
        )
    ;   BuildSteps0 = [[dependencies], [configure], build, install, [test]],
        (   Rebuild == true
        ->  BuildSteps1 = [distclean|BuildSteps0]
        ;   BuildSteps1 = BuildSteps0
        ),
        (   option(test(false), Options)
        ->  delete(BuildSteps1, [test], BuildSteps2)
        ;   BuildSteps2 = BuildSteps1
        ),
        (   option(clean(true), Options)
        ->  append(BuildSteps2, [[clean]], BuildSteps)
        ;   BuildSteps = BuildSteps2
        ),
        build_steps(BuildSteps, PackDir, [pack_version(Version)|Options]),
        pack_assert(PackDir, built(Arch, PrologVersion, built))
    ).
post_install_foreign(_, _, _).


%!  foreign_present(+PackDir, +Arch) is semidet.
%
%   True if we find one or more modules  in the pack `lib` directory for
%   the current architecture.
%
%   @tbd Does not check that  these  can   be  loaded,  nor  whether all
%   required modules are present.

foreign_present(PackDir, Arch) :-
    atomic_list_concat([PackDir, '/lib'], ForeignBaseDir),
    exists_directory(ForeignBaseDir),
    !,
    atomic_list_concat([PackDir, '/lib/', Arch], ForeignDir),
    exists_directory(ForeignDir),
    current_prolog_flag(shared_object_extension, Ext),
    atomic_list_concat([ForeignDir, '/*.', Ext], Pattern),
    expand_file_name(Pattern, Files),
    Files \== [].

%!  is_foreign_pack(+PackDir, -Type) is nondet.
%
%   True when PackDir contains  files  that   indicate  the  need  for a
%   specific class of build tools indicated by Type.

is_foreign_pack(PackDir, Type) :-
    foreign_file(File, Type),
    directory_file_path(PackDir, File, Path),
    exists_file(Path).

foreign_file('CMakeLists.txt', cmake).
foreign_file('configure',      configure).
foreign_file('configure.in',   autoconf).
foreign_file('configure.ac',   autoconf).
foreign_file('Makefile.am',    automake).
foreign_file('Makefile',       make).
foreign_file('makefile',       make).
foreign_file('conanfile.txt',  conan).
foreign_file('conanfile.py',   conan).


                 /*******************************
                 *           AUTOLOAD           *
                 *******************************/

%!  post_install_autoload(+InfoOrList) is det.
%
%   Create an autoload index if the package demands such.

post_install_autoload(List), is_list(List) =>
    maplist(post_install_autoload, List).
post_install_autoload(Info),
    _{installed:PackDir, autoload:true} :< Info =>
    directory_file_path(PackDir, prolog, PrologLibDir),
    make_library_index(PrologLibDir).
post_install_autoload(Info) =>
    directory_file_path(Info.installed, 'prolog/INDEX.pl', IndexFile),
    (   exists_file(IndexFile)
    ->  E = error(_,_),
        print_message(warning, pack(delete_autoload_index(Info.pack, IndexFile))),
        catch(delete_file(IndexFile), E,
              print_message(warning, E))
    ;   true
    ).

%!  decide_autoload_pack(+Options, +Info0, -Info) is det.
%
%   Add autoload:true to Info if the  pack   needs  to be configured for
%   autoloading.

decide_autoload_pack(Options, Info0, Info) :-
    is_autoload_pack(Info0.pack, Info0.installed, Options),
    !,
    Info = Info0.put(autoload, true).
decide_autoload_pack(_, Info, Info).

is_autoload_pack(_Pack, _PackDir, Options) :-
    option(autoload(true), Options),
    !.
is_autoload_pack(Pack, PackDir, Options) :-
    pack_info_term(PackDir, autoload(true)),
    confirm(autoload(Pack), no, Options).


                 /*******************************
                 *            UPGRADE           *
                 *******************************/

%!  pack_upgrade(+Pack) is semidet.
%
%   Upgrade Pack.  Shorthand for pack_install(Pack, [upgrade(true)]).

pack_upgrade(Pack) :-
    pack_install(Pack, [upgrade(true)]).


                 /*******************************
                 *            REMOVE            *
                 *******************************/

%!  pack_remove(+Name) is det.
%!  pack_remove(+Name, +Options) is det.
%
%   Remove the indicated package.  If   packages  depend (indirectly) on
%   this pack, ask to remove these as well.  Options:
%
%     - interactive(false)
%       Do not prompt the user.
%     - dependencies(Boolean)
%       If `true` delete dependencies without asking.

pack_remove(Pack) :-
    pack_remove(Pack, []).

pack_remove(Pack, Options) :-
    option(dependencies(false), Options),
    !,
    pack_remove_forced(Pack).
pack_remove(Pack, Options) :-
    (   dependents(Pack, Deps)
    ->  (   option(dependencies(true), Options)
        ->  true
        ;   confirm_remove(Pack, Deps, Delete, Options)
        ),
        forall(member(P, Delete), pack_remove_forced(P))
    ;   pack_remove_forced(Pack)
    ).

pack_remove_forced(Pack) :-
    catch('$pack_detach'(Pack, BaseDir),
          error(existence_error(pack, Pack), _),
          fail),
    !,
    (   read_link(BaseDir, _, Target)
    ->  What = link(Target)
    ;   What = directory
    ),
    print_message(informational, pack(remove(What, BaseDir))),
    delete_directory_and_contents(BaseDir).
pack_remove_forced(Pack) :-
    unattached_pack(Pack, BaseDir),
    !,
    delete_directory_and_contents(BaseDir).
pack_remove_forced(Pack) :-
    print_message(informational, error(existence_error(pack, Pack),_)).

confirm_remove(Pack, Deps, Delete, Options) :-
    print_message(warning, pack(depends(Pack, Deps))),
    menu(pack(resolve_remove),
         [ [Pack]      = remove_only(Pack),
           [Pack|Deps] = remove_deps(Pack, Deps),
           []          = cancel
         ], [], Delete, Options),
    Delete \== [].


		 /*******************************
		 *           PUBLISH		*
		 *******************************/

%!  pack_publish(+Spec, +Options) is det.
%
%   Publish a package. There are two ways  typical ways to call this. We
%   recommend developing a pack in a   GIT  repository. In this scenario
%   the pack can be published using
%
%       ?- pack_publish('.', []).
%
%   Alternatively, an archive  file  has  been   uploaded  to  a  public
%   location. In this scenario we can publish the pack using
%
%       ?- pack_publish(URL, [])
%
%   In both scenarios, pack_publish/2  by   default  creates an isolated
%   environment and installs the package  in   this  directory  from the
%   public URL. On success it triggers the   pack server to register the
%   URL as a new pack or a new release of a pack.
%
%   Packs may also be published using the _app_ `pack`, e.g.
%
%       swipl pack publish .
%
%   Options:
%
%     - git(Boolean)
%       If `true`, and Spec is a git managed directory, install using
%       the remote repo.
%     - sign(Boolean)
%       Sign the repository with the current version.  This runs
%       ``git tag -s <tag>``.
%     - force(Boolean)
%       Force the git tag.  This runs ``git tag -f <tag>``.
%     - branch(+Branch)
%       Branch used for releases.  Defined by git_default_branch/2
%       if not specified.
%     - register(+Boolean)
%       If `false` (default `true`), perform the installation, but do
%       not upload to the server. This can be used for testing.
%     - isolated(+Boolean)
%       If `true` (default), install and build all packages in an
%       isolated package directory.  If `false`, use other packages
%       installed for the environment.   The latter may be used to
%       speedup debugging.
%     - pack_directory(+Dir)
%       Install the temporary packages in Dir. If omitted pack_publish/2
%       creates a temporary directory and deletes this directory after
%       completion. An explict target Dir is created if it does not
%       exist and is not deleted on completion.
%     - clean(+Boolean)
%       If `true` (default), clean the destination directory first

pack_publish(Dir, Options) :-
    \+ download_url(Dir),
    is_git_directory(Dir), !,
    pack_git_info(Dir, _Hash, Metadata),
    prepare_repository(Dir, Metadata, Options),
    (   memberchk(download(URL), Metadata),
        git_url(URL, _)
    ->  true
    ;   option(remote(Remote), Options, origin),
        git_remote_url(Remote, RemoteURL, [directory(Dir)]),
        git_to_https_url(RemoteURL, URL)
    ),
    memberchk(version(Version), Metadata),
    pack_publish_(URL,
                  [ version(Version)
                  | Options
                  ]).
pack_publish(Spec, Options) :-
    pack_publish_(Spec, Options).

pack_publish_(Spec, Options) :-
    pack_default_options(Spec, Pack, Options, DefOptions),
    option(url(URL), DefOptions),
    valid_publish_url(URL, Options),
    prepare_build_location(Pack, Dir, Clean, Options),
    (   option(register(false), Options)
    ->  InstallOptions = DefOptions
    ;   InstallOptions = [publish(Pack)|DefOptions]
    ),
    call_cleanup(pack_install(Pack,
                              [ pack(Pack)
                              | InstallOptions
                              ]),
                 cleanup_publish(Clean, Dir)).

cleanup_publish(true, Dir) :-
    !,
    delete_directory_and_contents(Dir).
cleanup_publish(_, _).

valid_publish_url(URL, Options) :-
    option(register(Register), Options, true),
    (   Register == false
    ->  true
    ;   download_url(URL)
    ->  true
    ;   permission_error(publish, pack, URL)
    ).

prepare_build_location(Pack, Dir, Clean, Options) :-
    (   option(pack_directory(Dir), Options)
    ->  ensure_directory(Dir),
        (   option(clean(true), Options, true)
        ->  delete_directory_contents(Dir)
        ;   true
        )
    ;   tmp_file(pack, Dir),
        make_directory(Dir),
        Clean = true
    ),
    (   option(isolated(false), Options)
    ->  detach_pack(Pack, _),
        attach_packs(Dir, [search(first)])
    ;   attach_packs(Dir, [replace(true)])
    ).



%!  prepare_repository(+Dir, +Metadata, +Options) is semidet.
%
%   Prepare the git repository. If register(false)  is provided, this is
%   a test run and therefore we do   not  need this. Otherwise we demand
%   the working directory to be clean,  we   tag  the current commit and
%   push the current branch.

prepare_repository(_Dir, _Metadata, Options) :-
    option(register(false), Options),
    !.
prepare_repository(Dir, Metadata, Options) :-
    git_dir_must_be_clean(Dir),
    git_must_be_on_default_branch(Dir, Options),
    tag_git_dir(Dir, Metadata, Action, Options),
    confirm(git_push, yes, Options),
    run_process(path(git), ['-C', file(Dir), push ], []),
    (   Action = push_tag(Tag)
    ->  run_process(path(git), ['-C', file(Dir), push, origin, Tag ], [])
    ;   true
    ).

git_dir_must_be_clean(Dir) :-
    git_describe(Description, [directory(Dir)]),
    (   sub_atom(Description, _, _, 0, '-DIRTY')
    ->  print_message(error, pack(git_not_clean(Dir))),
        fail
    ;   true
    ).

git_must_be_on_default_branch(Dir, Options) :-
    (   option(branch(Default), Options)
    ->  true
    ;   git_default_branch(Default, [directory(Dir)])
    ),
    git_current_branch(Current, [directory(Dir)]),
    (   Default == Current
    ->  true
    ;   print_message(error,
                      pack(git_branch_not_default(Dir, Default, Current))),
        fail
    ).


%!  tag_git_dir(+Dir, +Metadata, -Action, +Options) is semidet.
%
%   Add a version tag to the git repository.
%
%   @arg Action is one of push_tag(Tag) or `none`

tag_git_dir(Dir, Metadata, Action, Options) :-
    memberchk(version(Version), Metadata),
    atom_concat('V', Version, Tag),
    git_tags(Tags, [directory(Dir)]),
    (   memberchk(Tag, Tags)
    ->  git_tag_is_consistent(Dir, Tag, Action, Options)
    ;   format(string(Message), 'Release ~w', [Version]),
        findall(Opt, git_tag_option(Opt, Options), Argv,
                [ '-m', Message, Tag ]),
        confirm(git_tag(Tag), yes, Options),
        run_process(path(git), ['-C', file(Dir), tag | Argv ], []),
        Action = push_tag(Tag)
    ).

git_tag_option('-s', Options) :- option(sign(true), Options, true).
git_tag_option('-f', Options) :- option(force(true), Options, true).

git_tag_is_consistent(Dir, Tag, Action, Options) :-
    format(atom(TagRef), 'refs/tags/~w', [Tag]),
    format(atom(CommitRef), 'refs/tags/~w^{}', [Tag]),
    option(remote(Remote), Options, origin),
    git_ls_remote(Dir, LocalTags, [tags(true)]),
    memberchk(CommitHash-CommitRef, LocalTags),
    (   git_hash(CommitHash, [directory(Dir)])
    ->  true
    ;   print_message(error, pack(git_release_tag_not_at_head(Tag))),
        fail
    ),
    memberchk(TagHash-TagRef, LocalTags),
    git_ls_remote(Remote, RemoteTags, [tags(true)]),
    (   memberchk(RemoteCommitHash-CommitRef, RemoteTags),
        memberchk(RemoteTagHash-TagRef, RemoteTags)
    ->  (   RemoteCommitHash == CommitHash,
            RemoteTagHash == TagHash
        ->  Action = none
        ;   print_message(error, pack(git_tag_out_of_sync(Tag))),
            fail
        )
    ;   Action = push_tag(Tag)
    ).

%!  git_to_https_url(+GitURL, -HTTP_URL) is semidet.
%
%   Get the HTTP(s) URL for a git repository, given a git url.
%   Whether or not this is available and how to translate the
%   one into the other depends in the server software.

git_to_https_url(URL, URL) :-
    download_url(URL),
    !.
git_to_https_url(GitURL, URL) :-
    atom_concat('git@github.com:', Repo, GitURL),
    !,
    atom_concat('https://github.com/', Repo, URL).
git_to_https_url(GitURL, _) :-
    print_message(error, pack(git_no_https(GitURL))),
    fail.


                 /*******************************
                 *           PROPERTIES         *
                 *******************************/

%!  pack_property(?Pack, ?Property) is nondet.
%
%   True when Property  is  a  property   of  an  installed  Pack.  This
%   interface is intended for programs that   wish  to interact with the
%   package manager. Defined properties are:
%
%     - directory(Directory)
%     Directory into which the package is installed
%     - version(Version)
%     Installed version
%     - title(Title)
%     Full title of the package
%     - author(Author)
%     Registered author
%     - download(URL)
%     Official download URL
%     - readme(File)
%     Package README file (if present)
%     - todo(File)
%     Package TODO file (if present)

pack_property(Pack, Property) :-
    findall(Pack-Property, pack_property_(Pack, Property), List),
    member(Pack-Property, List).            % make det if applicable

pack_property_(Pack, Property) :-
    pack_info(Pack, _, Property).
pack_property_(Pack, Property) :-
    \+ \+ info_file(Property, _),
    '$pack':pack(Pack, BaseDir),
    access_file(BaseDir, read),
    directory_files(BaseDir, Files),
    member(File, Files),
    info_file(Property, Pattern),
    downcase_atom(File, Pattern),
    directory_file_path(BaseDir, File, InfoFile),
    arg(1, Property, InfoFile).

info_file(readme(_), 'readme.txt').
info_file(readme(_), 'readme').
info_file(todo(_),   'todo.txt').
info_file(todo(_),   'todo').


                 /*******************************
                 *         VERSION LOGIC        *
                 *******************************/

%!  pack_version_file(-Pack, -Version:atom, +File) is semidet.
%
%   True if File is the  name  of  a   file  or  URL  of a file that
%   contains Pack at Version. File must   have  an extension and the
%   basename  must  be  of   the    form   <pack>-<n>{.<m>}*.  E.g.,
%   =|mypack-1.5|=.

pack_version_file(Pack, Version, GitHubRelease) :-
    atomic(GitHubRelease),
    github_release_url(GitHubRelease, Pack, Version),
    !.
pack_version_file(Pack, Version, Path) :-
    atomic(Path),
    file_base_name(Path, File),
    no_int_file_name_extension(Base, _Ext, File),
    atom_codes(Base, Codes),
    (   phrase(pack_version(Pack, Version), Codes),
        safe_pack_name(Pack)
    ->  true
    ).

no_int_file_name_extension(Base, Ext, File) :-
    file_name_extension(Base0, Ext0, File),
    \+ atom_number(Ext0, _),
    !,
    Base = Base0,
    Ext = Ext0.
no_int_file_name_extension(File, '', File).

%!  safe_pack_name(+Name:atom) is semidet.
%
%   Verifies that Name is a valid   pack  name. This avoids trickery
%   with pack file names to make shell commands behave unexpectly.

safe_pack_name(Name) :-
    atom_length(Name, Len),
    Len >= 3,                               % demand at least three length
    atom_codes(Name, Codes),
    maplist(safe_pack_char, Codes),
    !.

safe_pack_char(C) :- between(0'a, 0'z, C), !.
safe_pack_char(C) :- between(0'A, 0'Z, C), !.
safe_pack_char(C) :- between(0'0, 0'9, C), !.
safe_pack_char(0'_).

%!  pack_version(-Pack:atom, -Version:atom)// is semidet.
%
%   True when the input statifies <pack>-<version>

pack_version(Pack, Version) -->
    string(Codes), "-",
    version(Parts),
    !,
    { atom_codes(Pack, Codes),
      atomic_list_concat(Parts, '.', Version)
    }.

version([H|T]) -->
    version_part(H),
    (   "."
    ->  version(T)
    ;   {T=[]}
    ).

version_part(*) --> "*", !.
version_part(Int) --> integer(Int).


		 /*******************************
		 *           GIT LOGIC		*
		 *******************************/

have_git :-
    process_which(path(git), _).


%!  git_url(+URL, -Pack) is semidet.
%
%   True if URL describes a git url for Pack

git_url(URL, Pack) :-
    uri_components(URL, Components),
    uri_data(scheme, Components, Scheme),
    nonvar(Scheme),                         % must be full URL
    uri_data(path, Components, Path),
    (   Scheme == git
    ->  true
    ;   git_download_scheme(Scheme),
        file_name_extension(_, git, Path)
    ;   git_download_scheme(Scheme),
        catch(git_ls_remote(URL, _, [refs(['HEAD']), error(_)]), _, fail)
    ->  true
    ),
    file_base_name(Path, PackExt),
    (   file_name_extension(Pack, git, PackExt)
    ->  true
    ;   Pack = PackExt
    ),
    (   safe_pack_name(Pack)
    ->  true
    ;   domain_error(pack_name, Pack)
    ).

git_download_scheme(http).
git_download_scheme(https).

%!  github_release_url(+URL, -Pack, -Version:atom) is semidet.
%
%   True when URL is the URL of a GitHub release.  Such releases are
%   accessible as
%
%       https:/github.com/<owner>/<pack>/archive/[vV]?<version>.zip'

github_release_url(URL, Pack, Version) :-
    uri_components(URL, Components),
    uri_data(authority, Components, 'github.com'),
    uri_data(scheme, Components, Scheme),
    download_scheme(Scheme),
    uri_data(path, Components, Path),
    github_archive_path(Archive,Pack,File),
    atomic_list_concat(Archive, /, Path),
    file_name_extension(Tag, Ext, File),
    github_archive_extension(Ext),
    tag_version(Tag, Version),
    !.

github_archive_path(['',_User,Pack,archive,File],Pack,File).
github_archive_path(['',_User,Pack,archive,refs,tags,File],Pack,File).

github_archive_extension(tgz).
github_archive_extension(zip).

%!  tag_version(+GitTag, -Version) is semidet.
%
%   True when a GIT tag describes version Version.  GitTag must
%   satisfy ``[vV]?int(\.int)*``.

tag_version(Tag, Version) :-
    version_tag_prefix(Prefix),
    atom_concat(Prefix, Version, Tag),
    is_version(Version).

version_tag_prefix(v).
version_tag_prefix('V').
version_tag_prefix('').


%!  git_archive_url(+URL, -Archive, +Options) is semidet.
%
%   If we do not have git installed, some git services offer downloading
%   the code as  an  archive  using   HTTP.  This  predicate  makes this
%   translation.

git_archive_url(URL, Archive, Options) :-
    uri_components(URL, Components),
    uri_data(authority, Components, 'github.com'),
    uri_data(path, Components, Path),
    atomic_list_concat(['', User, RepoGit], /, Path),
    $,
    remove_git_ext(RepoGit, Repo),
    git_archive_version(Version, Options),
    atomic_list_concat(['', User, Repo, zip, Version], /, ArchivePath),
    uri_edit([ path(ArchivePath),
               host('codeload.github.com')
             ],
             URL, Archive).
git_archive_url(URL, _, _) :-
    print_message(error, pack(no_git(URL))),
    fail.

remove_git_ext(RepoGit, Repo) :-
    file_name_extension(Repo, git, RepoGit),
    !.
remove_git_ext(Repo, Repo).

git_archive_version(Version, Options) :-
    option(commit(Version), Options),
    !.
git_archive_version(Version, Options) :-
    option(branch(Version), Options),
    !.
git_archive_version(Version, Options) :-
    option(version(Version), Options),
    !.
git_archive_version('HEAD', _).

                 /*******************************
                 *       QUERY CENTRAL DB       *
                 *******************************/

%!  publish_download(+Infos, +Options) is semidet.
%!  register_downloads(+Infos, +Options) is det.
%
%   Register our downloads with the  pack server. The publish_download/2
%   version is used to  register  a   specific  pack  after successfully
%   installing the pack.  In this scenario, we
%
%     1. call register_downloads/2 with publish(Pack) that must be
%        a no-op.
%     2. build and test the pack
%     3. call publish_download/2, which calls register_downloads/2
%        after replacing publish(Pack) by do_publish(Pack).

register_downloads(_, Options) :-
    option(register(false), Options),
    !.
register_downloads(_, Options) :-
    option(publish(_), Options),
    !.
register_downloads(Infos, Options) :-
    convlist(download_data, Infos, Data),
    (   Data == []
    ->  true
    ;   query_pack_server(downloaded(Data), Reply, Options),
        (   option(do_publish(Pack), Options)
        ->  (   member(Info, Infos),
                Info.pack == Pack
            ->  true
            ),
            (   Reply = true(Actions),
                memberchk(Pack-Result, Actions)
            ->  (   registered(Result)
                ->  print_message(informational, pack(published(Info, Result)))
                ;   print_message(error, pack(publish_failed(Info, Result))),
                    fail
                )
            ;   print_message(error, pack(publish_failed(Info, false)))
            )
        ;   true
        )
    ).

registered(git(_URL)).
registered(file(_URL)).

publish_download(Infos, Options) :-
    select_option(publish(Pack), Options, Options1),
    !,
    register_downloads(Infos, [do_publish(Pack)|Options1]).
publish_download(_Infos, _Options).

%!  download_data(+Info, -Data) is semidet.
%
%   If we downloaded and installed Info, unify Data with the information
%   that we share with the pack registry. That is a term
%
%       download(URL, Hash, Metadata).
%
%   Where URL is location of the GIT   repository or URL of the download
%   archive. Hash is either the  GIT  commit   hash  or  the SHA1 of the
%   archive file.

download_data(Info, Data),
    Info.get(git) == true =>                % Git clone
    Data = download(URL, Hash, Metadata),
    URL = Info.get(downloaded),
    pack_git_info(Info.installed, Hash, Metadata).
download_data(Info, Data),
    _{git_url:URL,hash:Hash} :< Info, Hash \== (-) =>
    Data = download(URL, Hash, Metadata),   % Git downloaded as zip
    dir_metadata(Info.installed, Metadata).
download_data(Info, Data) =>                % Archive download.
    Data = download(URL, Hash, Metadata),
    URL = Info.get(downloaded),
    download_url(URL),
    pack_status_dir(Info.installed, archive(Archive, URL)),
    file_sha1(Archive, Hash),
    pack_archive_info(Archive, _Pack, Metadata, _).

%!  query_pack_server(+Query, -Result, +Options)
%
%   Send a Prolog query  to  the   package  server  and  process its
%   results.

query_pack_server(Query, Result, Options) :-
    (   option(server(ServerOpt), Options)
    ->  server_url(ServerOpt, ServerBase)
    ;   setting(server, ServerBase),
        ServerBase \== ''
    ),
    atom_concat(ServerBase, query, Server),
    format(codes(Data), '~q.~n', Query),
    info_level(Informational, Options),
    print_message(Informational, pack(contacting_server(Server))),
    setup_call_cleanup(
        http_open(Server, In,
                  [ post(codes(application/'x-prolog', Data)),
                    header(content_type, ContentType)
                  ]),
        read_reply(ContentType, In, Result),
        close(In)),
    message_severity(Result, Level, Informational),
    print_message(Level, pack(server_reply(Result))).

server_url(URL0, URL) :-
    uri_components(URL0, Components),
    uri_data(scheme, Components, Scheme),
    var(Scheme),
    !,
    atom_concat('https://', URL0, URL1),
    server_url(URL1, URL).
server_url(URL0, URL) :-
    uri_components(URL0, Components),
    uri_data(path, Components, ''),
    !,
    uri_edit([path('/pack/')], URL0, URL).
server_url(URL, URL).

read_reply(ContentType, In, Result) :-
    sub_atom(ContentType, 0, _, _, 'application/x-prolog'),
    !,
    set_stream(In, encoding(utf8)),
    read(In, Result).
read_reply(ContentType, In, _Result) :-
    read_string(In, 500, String),
    print_message(error, pack(no_prolog_response(ContentType, String))),
    fail.

info_level(Level, Options) :-
    option(silent(true), Options),
    !,
    Level = silent.
info_level(informational, _).

message_severity(true(_), Informational, Informational).
message_severity(false, warning, _).
message_severity(exception(_), error, _).


                 /*******************************
                 *        WILDCARD URIs         *
                 *******************************/

%!  available_download_versions(+URL, -Versions:list(atom), +Options) is det.
%
%   Deal with wildcard URLs, returning a  list of Version-URL pairs,
%   sorted by version.
%
%   @tbd    Deal with protocols other than HTTP

available_download_versions(URL, Versions, _Options) :-
    wildcard_pattern(URL),
    github_url(URL, User, Repo),            % demands https
    !,
    findall(Version-VersionURL,
            github_version(User, Repo, Version, VersionURL),
            Versions).
available_download_versions(URL0, Versions, Options) :-
    wildcard_pattern(URL0),
    !,
    hsts(URL0, URL, Options),
    file_directory_name(URL, DirURL0),
    ensure_slash(DirURL0, DirURL),
    print_message(informational, pack(query_versions(DirURL))),
    setup_call_cleanup(
        http_open(DirURL, In, []),
        load_html(stream(In), DOM,
                  [ syntax_errors(quiet)
                  ]),
        close(In)),
    findall(MatchingURL,
            absolute_matching_href(DOM, URL, MatchingURL),
            MatchingURLs),
    (   MatchingURLs == []
    ->  print_message(warning, pack(no_matching_urls(URL)))
    ;   true
    ),
    versioned_urls(MatchingURLs, VersionedURLs),
    sort_version_pairs(VersionedURLs, Versions),
    print_message(informational, pack(found_versions(Versions))).
available_download_versions(URL, [Version-URL], _Options) :-
    (   pack_version_file(_Pack, Version0, URL)
    ->  Version = Version0
    ;   Version = '0.0.0'
    ).

%!  sort_version_pairs(+Pairs, -Sorted) is det.
%
%   Sort a list of Version-Data by decreasing version.

sort_version_pairs(Pairs, Sorted) :-
    map_list_to_pairs(version_pair_sort_key_, Pairs, Keyed),
    sort(1, @>=, Keyed, SortedKeyed),
    pairs_values(SortedKeyed, Sorted).

version_pair_sort_key_(Version-_Data, Key) :-
    version_sort_key(Version, Key).

version_sort_key(Version, Key) :-
    split_string(Version, ".", "", Parts),
    maplist(number_string, Key, Parts),
    !.
version_sort_key(Version, _) :-
    domain_error(version, Version).

%!  github_url(+URL, -User, -Repo) is semidet.
%
%   True when URL refers to a github repository.

github_url(URL, User, Repo) :-
    uri_components(URL, uri_components(https,'github.com',Path,_,_)),
    atomic_list_concat(['',User,Repo|_], /, Path).


%!  github_version(+User, +Repo, -Version, -VersionURI) is nondet.
%
%   True when Version is a release version and VersionURI is the
%   download location for the zip file.

github_version(User, Repo, Version, VersionURI) :-
    atomic_list_concat(['',repos,User,Repo,tags], /, Path1),
    uri_components(ApiUri, uri_components(https,'api.github.com',Path1,_,_)),
    setup_call_cleanup(
      http_open(ApiUri, In,
                [ request_header('Accept'='application/vnd.github.v3+json')
                ]),
      json_read_dict(In, Dicts),
      close(In)),
    member(Dict, Dicts),
    atom_string(Tag, Dict.name),
    tag_version(Tag, Version),
    atom_string(VersionURI, Dict.zipball_url).

wildcard_pattern(URL) :- sub_atom(URL, _, _, _, *).
wildcard_pattern(URL) :- sub_atom(URL, _, _, _, ?).

ensure_slash(Dir, DirS) :-
    (   sub_atom(Dir, _, _, 0, /)
    ->  DirS = Dir
    ;   atom_concat(Dir, /, DirS)
    ).

remove_slash(Dir0, Dir) :-
    Dir0 \== '/',
    atom_concat(Dir1, /, Dir0),
    !,
    remove_slash(Dir1, Dir).
remove_slash(Dir, Dir).

absolute_matching_href(DOM, Pattern, Match) :-
    xpath(DOM, //a(@href), HREF),
    uri_normalized(HREF, Pattern, Match),
    wildcard_match(Pattern, Match).

versioned_urls([], []).
versioned_urls([H|T0], List) :-
    file_base_name(H, File),
    (   pack_version_file(_Pack, Version, File)
    ->  List = [Version-H|T]
    ;   List = T
    ),
    versioned_urls(T0, T).


                 /*******************************
                 *          DEPENDENCIES        *
                 *******************************/

%!  pack_provides(?Pack, -Provides) is multi.
%!  pack_requires(?Pack, -Requires) is nondet.
%!  pack_conflicts(?Pack, -Conflicts) is nondet.
%
%   Provide logical access to pack dependency relations.

pack_provides(Pack, Pack@Version) :-
    current_pack(Pack),
    once(pack_info(Pack, version, version(Version))).
pack_provides(Pack, Provides) :-
    findall(Prv, pack_info(Pack, dependency, provides(Prv)), PrvList),
    member(Provides, PrvList).

pack_requires(Pack, Requires) :-
    current_pack(Pack),
    findall(Req, pack_info(Pack, dependency, requires(Req)), ReqList),
    member(Requires, ReqList).

pack_conflicts(Pack, Conflicts) :-
    current_pack(Pack),
    findall(Cfl, pack_info(Pack, dependency, conflicts(Cfl)), CflList),
    member(Conflicts, CflList).

%!  pack_depends_on(?Pack, ?Dependency) is nondet.
%
%   True when Pack depends on pack   Dependency. This predicate does not
%   deal with transitive dependency.

pack_depends_on(Pack, Dependency) :-
    ground(Pack),
    !,
    pack_requires(Pack, Requires),
    \+ is_prolog_token(Requires),
    pack_provides(Dependency, Provides),
    satisfies_req(Provides, Requires).
pack_depends_on(Pack, Dependency) :-
    ground(Dependency),
    !,
    pack_provides(Dependency, Provides),
    pack_requires(Pack, Requires),
    satisfies_req(Provides, Requires).
pack_depends_on(Pack, Dependency) :-
    current_pack(Pack),
    pack_depends_on(Pack, Dependency).

%!  dependents(+Pack, -Dependents) is semidet.
%
%   True when Dependents is a list of  packs that (indirectly) depend on
%   Pack.

dependents(Pack, Deps) :-
    setof(Dep, dependent(Pack, Dep, []), Deps).

dependent(Pack, Dep, Seen) :-
    pack_depends_on(Dep0, Pack),
    \+ memberchk(Dep0, Seen),
    (   Dep = Dep0
    ;   dependent(Dep0, Dep, [Dep0|Seen])
    ).

%!  validate_dependencies is det.
%
%   Validate all dependencies, reporting on failures

validate_dependencies :-
    setof(Issue, pack_dependency_issue(_, Issue), Issues),
    !,
    print_message(warning, pack(dependency_issues(Issues))).
validate_dependencies.

%!  pack_dependency_issue(?Pack, -Issue) is nondet.
%
%   True when Issue is a dependency issue   regarding Pack. Issue is one
%   of
%
%     - unsatisfied(Pack, Requires)
%       The requirement Requires of Pack is not fulfilled.
%     - conflicts(Pack, Conflict)
%       Pack conflicts with Conflict.

pack_dependency_issue(Pack, Issue) :-
    current_pack(Pack),
    pack_dependency_issue_(Pack, Issue).

pack_dependency_issue_(Pack, unsatisfied(Pack, Requires)) :-
    pack_requires(Pack, Requires),
    (   is_prolog_token(Requires)
    ->  \+ prolog_satisfies(Requires)
    ;   \+ ( pack_provides(_, Provides),
             satisfies_req(Provides, Requires) )
    ).
pack_dependency_issue_(Pack, conflicts(Pack, Conflicts)) :-
    pack_conflicts(Pack, Conflicts),
    (   is_prolog_token(Conflicts)
    ->  prolog_satisfies(Conflicts)
    ;   pack_provides(_, Provides),
        satisfies_req(Provides, Conflicts)
    ).


		 /*******************************
		 *      RECORD PACK FACTS	*
		 *******************************/

%!  pack_assert(+PackDir, ++Fact) is det.
%
%   Add/update  a  fact  about  packs.  These    facts   are  stored  in
%   PackDir/status.db. Known facts are:
%
%     - built(Arch, Version, How)
%       Pack has been built by SWI-Prolog Version for Arch.  How is one
%       of `built` if we built it or `downloaded` if it was downloaded.
%     - automatic(Boolean)
%       If `true`, pack was installed as dependency.
%     - archive(Archive, URL)
%       Available when the pack was installed by unpacking Archive that
%       was retrieved from URL.

pack_assert(PackDir, Fact) :-
    must_be(ground, Fact),
    findall(Term, pack_status_dir(PackDir, Term), Facts0),
    update_facts(Facts0, Fact, Facts),
    OpenOptions = [encoding(utf8), lock(exclusive)],
    status_file(PackDir, StatusFile),
    (   Facts == Facts0
    ->  true
    ;   Facts0 \== [],
        append(Facts0, New, Facts)
    ->  setup_call_cleanup(
            open(StatusFile, append, Out, OpenOptions),
            maplist(write_fact(Out), New),
            close(Out))
    ;   setup_call_cleanup(
            open(StatusFile, write, Out, OpenOptions),
            ( write_facts_header(Out),
              maplist(write_fact(Out), Facts)
            ),
            close(Out))
    ).

update_facts([], Fact, [Fact]) :-
    !.
update_facts([H|T], Fact, [Fact|T]) :-
    general_pack_fact(Fact, GenFact),
    general_pack_fact(H, GenTerm),
    GenFact =@= GenTerm,
    !.
update_facts([H|T0], Fact, [H|T]) :-
    update_facts(T0, Fact, T).

general_pack_fact(built(Arch, _Version, _How), General) =>
    General = built(Arch, _, _).
general_pack_fact(Term, General), compound(Term) =>
    compound_name_arity(Term, Name, Arity),
    compound_name_arity(General, Name, Arity).
general_pack_fact(Term, General) =>
    General = Term.

write_facts_header(Out) :-
    format(Out, '% Fact status file.  Managed by package manager.~n', []).

write_fact(Out, Term) :-
    format(Out, '~q.~n', [Term]).

%!  pack_status(?Pack, ?Fact).
%!  pack_status_dir(+PackDir, ?Fact)
%
%   True when Fact is true about the package in PackDir.  Facts
%   are asserted a file `status.db`.

pack_status(Pack, Fact) :-
    current_pack(Pack, PackDir),
    pack_status_dir(PackDir, Fact).

pack_status_dir(PackDir, Fact) :-
    det_if(ground(Fact), pack_status_(PackDir, Fact)).

pack_status_(PackDir, Fact) :-
    status_file(PackDir, StatusFile),
    catch(term_in_file(valid_term(pack_status_term), StatusFile, Fact),
          error(existence_error(source_sink, StatusFile), _),
          fail).

pack_status_term(built(atom, version, oneof([built,downloaded]))).
pack_status_term(automatic(boolean)).
pack_status_term(archive(atom, atom)).


%!  update_automatic(+Info) is det.
%
%   Update the _automatic_ status of a package.  If we install it has no
%   automatic status and we install it  as   a  dependency we mark it as
%   _automatic_. Else, we mark  it  as   non-automatic  as  it  has been
%   installed explicitly.

update_automatic(Info) :-
    _ = Info.get(dependency_for),
    \+ pack_status(Info.installed, automatic(_)),
    !,
    pack_assert(Info.installed, automatic(true)).
update_automatic(Info) :-
    pack_assert(Info.installed, automatic(false)).

status_file(PackDir, StatusFile) :-
    directory_file_path(PackDir, 'status.db', StatusFile).

                 /*******************************
                 *        USER INTERACTION      *
                 *******************************/

:- multifile prolog:message//1.

%!  menu(Question, +Alternatives, +Default, -Selection, +Options)

menu(_Question, _Alternatives, Default, Selection, Options) :-
    option(interactive(false), Options),
    !,
    Selection = Default.
menu(Question, Alternatives, Default, Selection, _) :-
    length(Alternatives, N),
    between(1, 5, _),
       print_message(query, Question),
       print_menu(Alternatives, Default, 1),
       print_message(query, pack(menu(select))),
       read_selection(N, Choice),
    !,
    (   Choice == default
    ->  Selection = Default
    ;   nth1(Choice, Alternatives, Selection=_)
    ->  true
    ).

print_menu([], _, _).
print_menu([Value=Label|T], Default, I) :-
    (   Value == Default
    ->  print_message(query, pack(menu(default_item(I, Label))))
    ;   print_message(query, pack(menu(item(I, Label))))
    ),
    I2 is I + 1,
    print_menu(T, Default, I2).

read_selection(Max, Choice) :-
    get_single_char(Code),
    (   answered_default(Code)
    ->  Choice = default
    ;   code_type(Code, digit(Choice)),
        between(1, Max, Choice)
    ->  true
    ;   print_message(warning, pack(menu(reply(1,Max)))),
        fail
    ).

%!  confirm(+Question, +Default, +Options) is semidet.
%
%   Ask for confirmation.
%
%   @arg Default is one of `yes`, `no` or `none`.

confirm(_Question, Default, Options) :-
    Default \== none,
    option(interactive(false), Options, true),
    !,
    Default == yes.
confirm(Question, Default, _) :-
    between(1, 5, _),
       print_message(query, pack(confirm(Question, Default))),
       read_yes_no(YesNo, Default),
    !,
    format(user_error, '~N', []),
    YesNo == yes.

read_yes_no(YesNo, Default) :-
    get_single_char(Code),
    code_yes_no(Code, Default, YesNo),
    !.

code_yes_no(0'y, _, yes).
code_yes_no(0'Y, _, yes).
code_yes_no(0'n, _, no).
code_yes_no(0'N, _, no).
code_yes_no(_, none, _) :- !, fail.
code_yes_no(C, Default, Default) :-
    answered_default(C).

answered_default(0'\r).
answered_default(0'\n).
answered_default(0'\s).


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile prolog:message//1.

prolog:message(pack(Message)) -->
    message(Message).

:- discontiguous
    message//1,
    label//1.

message(invalid_term(pack_info_term, Term)) -->
    [ 'Invalid package meta data: ~q'-[Term] ].
message(invalid_term(pack_status_term, Term)) -->
    [ 'Invalid package status data: ~q'-[Term] ].
message(directory_exists(Dir)) -->
    [ 'Package target directory exists and is not empty:', nl,
      '\t~q'-[Dir]
    ].
message(already_installed(pack(Pack, Version))) -->
    [ 'Pack `~w'' is already installed @~w'-[Pack, Version] ].
message(already_installed(Pack)) -->
    [ 'Pack `~w'' is already installed. Package info:'-[Pack] ].
message(kept_foreign(Pack, Arch)) -->
    [ 'Found foreign libraries for architecture '-[],
      ansi(code, '~q', [Arch]), nl,
      'Use ', ansi(code, '?- pack_rebuild(~q).', [Pack]),
      ' to rebuild from sources'-[]
    ].
message(no_pack_installed(Pack)) -->
    [ 'No pack ~q installed.  Use ?- pack_list(Pattern) to search'-[Pack] ].
message(dependency_issues(Issues)) -->
    [ 'The current set of packs has dependency issues:', nl ],
    dep_issues(Issues).
message(depends(Pack, Deps)) -->
    [ 'The following packs depend on `~w\':'-[Pack], nl ],
    pack_list(Deps).
message(remove(link(To), PackDir)) -->
    [ 'Removing ', url(PackDir), nl, '    as link to ', url(To) ].
message(remove(directory, PackDir)) -->
    [ 'Removing ~q and contents'-[PackDir] ].
message(remove_existing_pack(PackDir)) -->
    [ 'Remove old installation in ~q'-[PackDir] ].
message(delete_autoload_index(Pack, Index)) -->
    [ 'Pack ' ], msg_pack(Pack), [ ': deleting autoload index ', url(Index) ].
message(download_plan(Plan)) -->
    [ ansi(bold, 'Installation plan:', []), nl ],
    install_plan(Plan, Actions),
    install_label(Actions).
message(build_plan(Plan)) -->
    [ ansi(bold, 'The following packs have post install scripts:', []), nl ],
    msg_build_plan(Plan),
    [ nl, ansi(bold, 'Run scripts?', []) ].
message(autoload(Pack)) -->
    [ 'Pack ' ], msg_pack(Pack),
    [ ' prefers to be added as autoload library',
      nl, ansi(bold, 'Allow?', [])
    ].
message(no_meta_data(BaseDir)) -->
    [ 'Cannot find pack.pl inside directory ~q.  Not a package?'-[BaseDir] ].
message(search_no_matches(Name)) -->
    [ 'Search for "~w", returned no matching packages'-[Name] ].
message(rebuild(Pack)) -->
    [ 'Checking pack "~w" for rebuild ...'-[Pack] ].
message(up_to_date([Pack])) -->
    !,
    [ 'Pack ' ], msg_pack(Pack), [' is up-to-date' ].
message(up_to_date(Packs)) -->
    [ 'Packs ' ], sequence(msg_pack, [', '], Packs), [' are up-to-date' ].
message(installed_can_upgrade(List)) -->
    sequence(msg_can_upgrade_target, [nl], List).
message(new_dependencies(Deps)) -->
    [ 'Found new dependencies after downloading (~p).'-[Deps], nl ].
message(query_versions(URL)) -->
    [ 'Querying "~w" to find new versions ...'-[URL] ].
message(no_matching_urls(URL)) -->
    [ 'Could not find any matching URL: ~q'-[URL] ].
message(found_versions([Latest-_URL|More])) -->
    { length(More, Len) },
    [ '    Latest version: ~w (~D older)'-[Latest, Len] ].
message(build(Pack, PackDir)) -->
    [ ansi(bold, 'Building pack ~w in directory ~w', [Pack, PackDir]) ].
message(contacting_server(Server)) -->
    [ 'Contacting server at ~w ...'-[Server], flush ].
message(server_reply(true(_))) -->
    [ at_same_line, ' ok'-[] ].
message(server_reply(false)) -->
    [ at_same_line, ' done'-[] ].
message(server_reply(exception(E))) -->
    [ 'Server reported the following error:'-[], nl ],
    '$messages':translate_message(E).
message(cannot_create_dir(Alias)) -->
    { findall(PackDir,
              absolute_file_name(Alias, PackDir, [solutions(all)]),
              PackDirs0),
      sort(PackDirs0, PackDirs)
    },
    [ 'Cannot find a place to create a package directory.'-[],
      'Considered:'-[]
    ],
    candidate_dirs(PackDirs).
message(conflict(version, [PackV, FileV])) -->
    ['Version mismatch: pack.pl: '-[]], msg_version(PackV),
    [', file claims version '-[]], msg_version(FileV).
message(conflict(name, [PackInfo, FileInfo])) -->
    ['Pack ~w mismatch: pack.pl: ~p'-[PackInfo]],
    [', file claims ~w: ~p'-[FileInfo]].
message(no_prolog_response(ContentType, String)) -->
    [ 'Expected Prolog response.  Got content of type ~p'-[ContentType], nl,
      '~s'-[String]
    ].
message(download(begin, Pack, _URL, _DownloadFile)) -->
    [ 'Downloading ' ], msg_pack(Pack), [ ' ... ', flush ].
message(download(end, _, _, File)) -->
    { size_file(File, Bytes) },
    [ at_same_line, '~D bytes'-[Bytes] ].
message(no_git(URL)) -->
    [ 'Cannot install from git repository ', url(URL), '.', nl,
      'Cannot find git program and do not know how to download the code', nl,
      'from this git service.  Please install git and retry.'
    ].
message(git_no_https(GitURL)) -->
    [ 'Do not know how to get an HTTP(s) URL for ', url(GitURL) ].
message(git_branch_not_default(Dir, Default, Current)) -->
    [ 'GIT current branch on ', url(Dir), ' is not default.', nl,
      '  Current branch: ', ansi(code, '~w', [Current]),
      ' default: ', ansi(code, '~w', [Default])
    ].
message(git_not_clean(Dir)) -->
    [ 'GIT working directory is dirty: ', url(Dir), nl,
      'Your repository must be clean before publishing.'
    ].
message(git_push) -->
    [ 'Push release to GIT origin?' ].
message(git_tag(Tag)) -->
    [ 'Tag repository with release tag ', ansi(code, '~w', [Tag]) ].
message(git_release_tag_not_at_head(Tag)) -->
    [ 'Release tag ', ansi(code, '~w', [Tag]), ' is not at HEAD.', nl,
      'If you want to update the tag, please run ',
      ansi(code, 'git tag -d ~w', [Tag])
    ].
message(git_tag_out_of_sync(Tag)) -->
    [ 'Release tag ', ansi(code, '~w', [Tag]),
      ' differs from this tag at the origin'
    ].

message(published(Info, At)) -->
    [ 'Published pack ' ], msg_pack(Info), msg_info_version(Info),
    [' to be installed from '],
    msg_published_address(At).
message(publish_failed(Info, Reason)) -->
    [ 'Pack ' ], msg_pack(Info), [ ' at version ~w'-[Info.version] ],
    msg_publish_failed(Reason).

msg_publish_failed(throw(error(permission_error(register,
                                                pack(_),_URL),_))) -->
    [ ' is already registered with a different URL'].
msg_publish_failed(download) -->
    [' was already published?'].
msg_publish_failed(Status) -->
    [ ' failed for unknown reason (~p)'-[Status] ].

msg_published_address(git(URL)) -->
    msg_url(URL, _).
msg_published_address(file(URL)) -->
    msg_url(URL, _).

candidate_dirs([]) --> [].
candidate_dirs([H|T]) --> [ nl, '    ~w'-[H] ], candidate_dirs(T).
                                                % Questions
message(resolve_remove) -->
    [ nl, 'Please select an action:', nl, nl ].
message(create_pack_dir) -->
    [ nl, 'Create directory for packages', nl ].
message(menu(item(I, Label))) -->
    [ '~t(~d)~6|   '-[I] ],
    label(Label).
message(menu(default_item(I, Label))) -->
    [ '~t(~d)~6| * '-[I] ],
    label(Label).
message(menu(select)) -->
    [ nl, 'Your choice? ', flush ].
message(confirm(Question, Default)) -->
    message(Question),
    confirm_default(Default),
    [ flush ].
message(menu(reply(Min,Max))) -->
    (  { Max =:= Min+1 }
    -> [ 'Please enter ~w or ~w'-[Min,Max] ]
    ;  [ 'Please enter a number between ~w and ~w'-[Min,Max] ]
    ).

                                                % support predicates
dep_issues(Issues) -->
    sequence(dep_issue, [nl], Issues).

dep_issue(unsatisfied(Pack, Requires)) -->
    [ ' - Pack ' ], msg_pack(Pack), [' requires ~p'-[Requires]].
dep_issue(conflicts(Pack, Conflict)) -->
    [ ' - Pack ' ], msg_pack(Pack), [' conflicts with ~p'-[Conflict]].

%!  install_plan(+Plan, -Actions)// is det.
%!  install_label(+Actions)// is det.
%
%   Describe the overall installation plan before downloading.

install_label([link]) -->
    !,
    [ ansi(bold, 'Activate pack?', []) ].
install_label([unpack]) -->
    !,
    [ ansi(bold, 'Unpack archive?', []) ].
install_label(_) -->
    [ ansi(bold, 'Download packs?', []) ].


install_plan(Plan, Actions) -->
    install_plan(Plan, Actions, Sec),
    sec_warning(Sec).

install_plan([], [], _) -->
    [].
install_plan([H|T], [AH|AT], Sec) -->
    install_step(H, AH, Sec), [nl],
    install_plan(T, AT, Sec).

install_step(Info, keep, _Sec) -->
    { Info.get(keep) == true },
    !,
    [ '  Keep ' ], msg_pack(Info), [ ' at version ~w'-[Info.version] ],
    msg_can_upgrade(Info).
install_step(Info, Action, Sec) -->
    { From = Info.get(upgrade),
      VFrom = From.version,
      VTo = Info.get(version),
      (   cmp_versions(>=, VTo, VFrom)
      ->  Label = ansi(bold,    '  Upgrade ',   [])
      ;   Label = ansi(warning, '  Downgrade ', [])
      )
    },
    [ Label ], msg_pack(Info),
    [ ' from version ~w to ~w'- [From.version, Info.get(version)] ],
    install_from(Info, Action, Sec).
install_step(Info, Action, Sec) -->
    { _From = Info.get(upgrade) },
    [ '  Upgrade '  ], msg_pack(Info),
    install_from(Info, Action, Sec).
install_step(Info, Action, Sec) -->
    { Dep = Info.get(dependency_for) },
    [ '  Install ' ], msg_pack(Info),
    [ ' at version ~w as dependency for '-[Info.version],
      ansi(code, '~w', [Dep])
    ],
    install_from(Info, Action, Sec),
    msg_downloads(Info).
install_step(Info, Action, Sec) -->
    { Info.get(commit) == 'HEAD' },
    !,
    [ '  Install ' ], msg_pack(Info), [ ' at current GIT HEAD'-[] ],
    install_from(Info, Action, Sec),
    msg_downloads(Info).
install_step(Info, link, _Sec) -->
    { Info.get(link) == true,
      uri_file_name(Info.get(url), Dir)
    },
    !,
    [ '  Install ' ], msg_pack(Info), [ ' as symlink to ', url(Dir) ].
install_step(Info, Action, Sec) -->
    [ '  Install ' ], msg_pack(Info), [ ' at version ~w'-[Info.get(version)] ],
    install_from(Info, Action, Sec),
    msg_downloads(Info).
install_step(Info, Action, Sec) -->
    [ '  Install ' ], msg_pack(Info),
    install_from(Info, Action, Sec),
    msg_downloads(Info).

install_from(Info, download, Sec) -->
    { download_url(Info.url) },
    !,
    [ ' from '  ], msg_url(Info.url, Sec).
install_from(Info, unpack, Sec) -->
    [ ' from '  ], msg_url(Info.url, Sec).

msg_url(URL, unsafe) -->
    { atomic(URL),
      atom_concat('http://', Rest, URL)
    },
    [ ansi(error, '~w', ['http://']), '~w'-[Rest] ].
msg_url(URL, _) -->
    [ url(URL) ].

sec_warning(Sec) -->
    { var(Sec) },
    !.
sec_warning(unsafe) -->
    [ ansi(warning, '  WARNING: The installation plan includes downloads \c
                                from insecure HTTP servers.', []), nl
    ].

msg_downloads(Info) -->
    { Downloads = Info.get(all_downloads),
      Downloads > 0
    },
    [ ansi(comment, ' (downloaded ~D times)', [Downloads]) ],
    !.
msg_downloads(_) -->
    [].

msg_pack(Pack) -->
    { atom(Pack) },
    !,
    [ ansi(code, '~w', [Pack]) ].
msg_pack(Info) -->
    msg_pack(Info.pack).

msg_info_version(Info) -->
    [ ansi(code, '@~w', [Info.get(version)]) ],
    !.
msg_info_version(_Info) -->
    [].

%!  msg_build_plan(+Plan)//
%
%   Describe the build plan before running the build steps.

msg_build_plan(Plan) -->
    sequence(build_step, [nl], Plan).

build_step(Info) -->
    [ '  Build ' ], msg_pack(Info), [' in directory ', url(Info.installed) ].

msg_can_upgrade_target(Info) -->
    [ '  Pack ' ], msg_pack(Info),
    [ ' is installed at version ~w'-[Info.version] ],
    msg_can_upgrade(Info).

pack_list([]) --> [].
pack_list([H|T]) -->
    [ '    - Pack ' ],  msg_pack(H), [nl],
    pack_list(T).

label(remove_only(Pack)) -->
    [ 'Only remove package ~w (break dependencies)'-[Pack] ].
label(remove_deps(Pack, Deps)) -->
    { length(Deps, Count) },
    [ 'Remove package ~w and ~D dependencies'-[Pack, Count] ].
label(create_dir(Dir)) -->
    [ '~w'-[Dir] ].
label(install_from(git(URL))) -->
    !,
    [ 'GIT repository at ~w'-[URL] ].
label(install_from(URL)) -->
    [ '~w'-[URL] ].
label(cancel) -->
    [ 'Cancel' ].

confirm_default(yes) -->
    [ ' Y/n? ' ].
confirm_default(no) -->
    [ ' y/N? ' ].
confirm_default(none) -->
    [ ' y/n? ' ].

msg_version(Version) -->
    [ '~w'-[Version] ].

msg_can_upgrade(Info) -->
    { Latest = Info.get(latest_version) },
    [ ansi(warning, ' (can be upgraded to ~w)', [Latest]) ].
msg_can_upgrade(_) -->
    [].


		 /*******************************
		 *              MISC		*
		 *******************************/

local_uri_file_name(URL, FileName) :-
    uri_file_name(URL, FileName),
    !.
local_uri_file_name(URL, FileName) :-
    uri_components(URL, Components),
    uri_data(scheme, Components, File), File == file,
    uri_data(authority, Components, FileNameEnc),
    uri_data(path, Components, ''),
    uri_encoded(path, FileName, FileNameEnc).

det_if(Cond, Goal) :-
    (   Cond
    ->  Goal,
        !
    ;   Goal
    ).

member_nonvar(_, Var) :-
    var(Var),
    !,
    fail.
member_nonvar(E, [E|_]).
member_nonvar(E, [_|T]) :-
    member_nonvar(E, T).

