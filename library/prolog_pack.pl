/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        J.Wielemaker@vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2012-2019, VU University Amsterdam
                              CWI, Amsterdam
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
            pack_search/1,              % +Keyword
            pack_install/1,             % +Name
            pack_install/2,             % +Name, +Options
            pack_upgrade/1,             % +Name
            pack_rebuild/1,             % +Name
            pack_rebuild/0,             % All packages
            pack_remove/1,              % +Name
            pack_property/2,            % ?Name, ?Property

            pack_url_file/2             % +URL, -File
          ]).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(process)).
:- use_module(library(option)).
:- use_module(library(readutil)).
:- use_module(library(lists)).
:- use_module(library(filesex)).
:- use_module(library(xpath)).
:- use_module(library(settings)).
:- use_module(library(uri)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/http_client), []).   % plugin for POST support
:- use_module(library(prolog_config)).

/** <module> A package manager for Prolog

The library(prolog_pack) provides the SWI-Prolog   package manager. This
library lets you inspect installed   packages,  install packages, remove
packages, etc. It is complemented by   the  built-in attach_packs/0 that
makes installed packages available as libaries.

@see    Installed packages can be inspected using =|?- doc_browser.|=
@tbd    Version logic
@tbd    Find and resolve conflicts
@tbd    Upgrade git packages
@tbd    Validate git packages
@tbd    Test packages: run tests from directory `test'.
*/

:- multifile
    environment/2.                          % Name, Value

:- dynamic
    pack_requires/2,                        % Pack, Requirement
    pack_provides_db/2.                     % Pack, Provided


                 /*******************************
                 *          CONSTANTS           *
                 *******************************/

:- setting(server, atom, 'https://www.swi-prolog.org/pack/',
           'Server to exchange pack information').


                 /*******************************
                 *         PACKAGE INFO         *
                 *******************************/

%!  current_pack(?Pack) is nondet.
%
%   True if Pack is a currently installed pack.

current_pack(Pack) :-
    '$pack':pack(Pack, _).

%!  pack_list_installed is det.
%
%   List currently installed  packages.   Unlike  pack_list/1,  only
%   locally installed packages are displayed   and  no connection is
%   made to the internet.
%
%   @see Use pack_list/1 to find packages.

pack_list_installed :-
    findall(Pack, current_pack(Pack), Packages0),
    Packages0 \== [],
    !,
    sort(Packages0, Packages),
    length(Packages, Count),
    format('Installed packages (~D):~n~n', [Count]),
    maplist(pack_info(list), Packages),
    validate_dependencies.
pack_list_installed :-
    print_message(informational, pack(no_packages_installed)).

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
    update_dependency_db(Name, Infos0),
    findall(Def,  pack_default(Level, Infos, Def), Defs),
    append(Infos0, Defs, Infos1),
    sort(Infos1, Infos),
    show_info(Name, Infos, [info(Level)]).


show_info(_Name, _Properties, Options) :-
    option(silent(true), Options),
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


pvalue_column(24).
print_property_value(Prop-Fmt, Values) :-
    !,
    pvalue_column(C),
    atomic_list_concat(['~w:~t~*|', Fmt, '~n'], Format),
    format(Format, [Prop,C|Values]).

pack_info(Name, Level, Info) :-
    '$pack':pack(Name, BaseDir),
    (   Info = directory(BaseDir)
    ;   pack_info_term(BaseDir, Info)
    ),
    pack_level_info(Level, Info, _Format, _Default).

:- public pack_level_info/4.                    % used by web-server

pack_level_info(_,    title(_),         'Title',                   '<no title>').
pack_level_info(_,    version(_),       'Installed version',       '<unknown>').
pack_level_info(info, directory(_),     'Installed in directory',  -).
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
        setup_call_cleanup(
            open(InfoFile, read, In),
            term_in_stream(In, Info),
            close(In)),
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
    member(Lib, Libs).

base_name(File, Base) :-
    file_name_extension(Base, pl, File).

term_in_stream(In, Term) :-
    repeat,
        read_term(In, Term0, []),
        (   Term0 == end_of_file
        ->  !, fail
        ;   Term = Term0,
            valid_info_term(Term0)
        ).

valid_info_term(Term) :-
    Term =.. [Name|Args],
    same_length(Args, Types),
    Decl =.. [Name|Types],
    (   pack_info_term(Decl)
    ->  maplist(valid_info_arg, Types, Args)
    ;   print_message(warning, pack(invalid_info(Term))),
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
pack_info_term(author(atom, email_or_url)).     % Persons
pack_info_term(maintainer(atom, email_or_url)).
pack_info_term(packager(atom, email_or_url)).
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
    version_data(Version, _Data).
error:has_type(email_or_url, Address) :-
    atom(Address),
    (   sub_atom(Address, _, _, _, @)
    ->  true
    ;   uri_is_global(Address)
    ).
error:has_type(dependency, Value) :-
    is_dependency(Value, _Token, _Version).

version_data(Version, version(Data)) :-
    atomic_list_concat(Parts, '.', Version),
    maplist(atom_number, Parts, Data).

is_dependency(Token, Token, *) :-
    atom(Token).
is_dependency(Term, Token, VersionCmp) :-
    Term =.. [Op,Token,Version],
    cmp(Op, _),
    version_data(Version, _),
    VersionCmp =.. [Op,Version].

cmp(<,  @<).
cmp(=<, @=<).
cmp(==, ==).
cmp(>=, @>=).
cmp(>,  @>).


                 /*******************************
                 *            SEARCH            *
                 *******************************/

%!  pack_search(+Query) is det.
%!  pack_list(+Query) is det.
%
%   Query package server and installed packages and display results.
%   Query is matches case-insensitively against   the name and title
%   of known and installed packages. For   each  matching package, a
%   single line is displayed that provides:
%
%     - Installation status
%       - *p*: package, not installed
%       - *i*: installed package; up-to-date with public version
%       - *U*: installed package; can be upgraded
%       - *A*: installed package; newer than publically available
%       - *l*: installed package; not on server
%     - Name@Version
%     - Name@Version(ServerVersion)
%     - Title
%
%   Hint: =|?- pack_list('').|= lists all packages.
%
%   The predicates pack_list/1 and pack_search/1  are synonyms. Both
%   contact the package server at  http://www.swi-prolog.org to find
%   available packages.
%
%   @see    pack_list_installed/0 to list installed packages without
%           contacting the server.

pack_list(Query) :-
    pack_search(Query).

pack_search(Query) :-
    query_pack_server(search(Query), Result, []),
    (   Result == false
    ->  (   local_search(Query, Packs),
            Packs \== []
        ->  forall(member(pack(Pack, Stat, Title, Version, _), Packs),
                   format('~w ~w@~w ~28|- ~w~n',
                          [Stat, Pack, Version, Title]))
        ;   print_message(warning, pack(search_no_matches(Query)))
        )
    ;   Result = true(Hits),
        local_search(Query, Local),
        append(Hits, Local, All),
        sort(All, Sorted),
        list_hits(Sorted)
    ).

list_hits([]).
list_hits([ pack(Pack, i, Title, Version, _),
            pack(Pack, p, Title, Version, _)
          | More
          ]) :-
    !,
    format('i ~w@~w ~28|- ~w~n', [Pack, Version, Title]),
    list_hits(More).
list_hits([ pack(Pack, i, Title, VersionI, _),
            pack(Pack, p, _,     VersionS, _)
          | More
          ]) :-
    !,
    version_data(VersionI, VDI),
    version_data(VersionS, VDS),
    (   VDI @< VDS
    ->  Tag = ('U')
    ;   Tag = ('A')
    ),
    format('~w ~w@~w(~w) ~28|- ~w~n', [Tag, Pack, VersionI, VersionS, Title]),
    list_hits(More).
list_hits([ pack(Pack, i, Title, VersionI, _)
          | More
          ]) :-
    !,
    format('l ~w@~w ~28|- ~w~n', [Pack, VersionI, Title]),
    list_hits(More).
list_hits([pack(Pack, Stat, Title, Version, _)|More]) :-
    format('~w ~w@~w ~28|- ~w~n', [Stat, Pack, Version, Title]),
    list_hits(More).


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
%
%   Install a package.  Spec is one of
%
%     * Archive file name
%     * HTTP URL of an archive file name.  This URL may contain a
%       star (*) for the version.  In this case pack_install asks
%       for the directory content and selects the latest version.
%     * GIT URL (not well supported yet)
%     * A local directory name given as =|file://|= URL.
%     * A package name.  This queries the package repository
%       at http://www.swi-prolog.org
%
%   After resolving the type of package,   pack_install/2 is used to
%   do the actual installation.

pack_install(Spec) :-
    pack_default_options(Spec, Pack, [], Options),
    pack_install(Pack, [pack(Pack)|Options]).

%!  pack_default_options(+Spec, -Pack, +OptionsIn, -Options) is det.
%
%   Establish  the  pack  name  (Pack)  and    install  options  from  a
%   specification and options (OptionsIn) provided by the user.

pack_default_options(_Spec, Pack, OptsIn, Options) :-
    option(already_installed(pack(Pack,_Version)), OptsIn),
    !,
    Options = OptsIn.
pack_default_options(_Spec, Pack, OptsIn, Options) :-
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
pack_default_options(Archive, Pack, _, Options) :-      % Install from archive
    must_be(atom, Archive),
    \+ uri_is_global(Archive),
    expand_file_name(Archive, [File]),
    exists_file(File),
    !,
    pack_version_file(Pack, Version, File),
    uri_file_name(FileURL, File),
    Options = [url(FileURL), version(Version)].
pack_default_options(URL, Pack, _, Options) :-
    git_url(URL, Pack),
    !,
    Options = [git(true), url(URL)].
pack_default_options(FileURL, Pack, _, Options) :-      % Install from directory
    uri_file_name(FileURL, Dir),
    exists_directory(Dir),
    pack_info_term(Dir, name(Pack)),
    !,
    (   pack_info_term(Dir, version(Version))
    ->  uri_file_name(DirURL, Dir),
        Options = [url(DirURL), version(Version)]
    ;   throw(error(existence_error(key, version, Dir),_))
    ).
pack_default_options(URL, Pack, _, Options) :-          % Install from URL
    pack_version_file(Pack, Version, URL),
    download_url(URL),
    !,
    available_download_versions(URL, [URLVersion-LatestURL|_]),
    Options = [url(LatestURL)|VersionOptions],
    version_options(Version, URLVersion, VersionOptions).
pack_default_options(Pack, Pack, OptsIn, Options) :-    % Install from name
    \+ uri_is_global(Pack),                             % ignore URLs
    query_pack_server(locate(Pack), Reply, OptsIn),
    (   Reply = true(Results)
    ->  pack_select_candidate(Pack, Results, OptsIn, Options)
    ;   print_message(warning, pack(no_match(Pack))),
        fail
    ).

version_options(Version, Version, [version(Version)]) :- !.
version_options(Version, _, [version(Version)]) :-
    Version = version(List),
    maplist(integer, List),
    !.
version_options(_, _, []).

%!  pack_select_candidate(+Pack, +AvailableVersions, +OptionsIn, -Options)
%
%   Select from available packages.

pack_select_candidate(Pack, [Version-_|_], Options,
                      [already_installed(pack(Pack, Installed))|Options]) :-
    current_pack(Pack),
    pack_info(Pack, _, version(InstalledAtom)),
    atom_version(InstalledAtom, Installed),
    Installed @>= Version,
    !.
pack_select_candidate(Pack, Available, Options, OptsOut) :-
    option(url(URL), Options),
    memberchk(_Version-URLs, Available),
    memberchk(URL, URLs),
    !,
    (   git_url(URL, Pack)
    ->  Extra = [git(true)]
    ;   Extra = []
    ),
    OptsOut = [url(URL), inquiry(true) | Extra].
pack_select_candidate(Pack, [Version-[URL]|_], Options,
                      [url(URL), git(true), inquiry(true)]) :-
    git_url(URL, Pack),
    !,
    confirm(install_from(Pack, Version, git(URL)), yes, Options).
pack_select_candidate(Pack, [Version-[URL]|More], Options,
                      [url(URL), inquiry(true)]) :-
    (   More == []
    ->  !
    ;   true
    ),
    confirm(install_from(Pack, Version, URL), yes, Options),
    !.
pack_select_candidate(Pack, [Version-URLs|_], Options,
                      [url(URL), inquiry(true)|Rest]) :-
    maplist(url_menu_item, URLs, Tagged),
    append(Tagged, [cancel=cancel], Menu),
    Menu = [Default=_|_],
    menu(pack(select_install_from(Pack, Version)),
         Menu, Default, Choice, Options),
    (   Choice == cancel
    ->  fail
    ;   Choice = git(URL)
    ->  Rest = [git(true)]
    ;   Choice = URL,
        Rest = []
    ).

url_menu_item(URL, git(URL)=install_from(git(URL))) :-
    git_url(URL, _),
    !.
url_menu_item(URL, URL=install_from(URL)).


%!  pack_install(+Name, +Options) is det.
%
%   Install package Name.  Processes  the   options  below.  Default
%   options as would be used by  pack_install/1 are used to complete
%   the provided Options.
%
%     * url(+URL)
%     Source for downloading the package
%     * package_directory(+Dir)
%     Directory into which to install the package
%     * interactive(+Boolean)
%     Use default answer without asking the user if there
%     is a default action.
%     * silent(+Boolean)
%     If `true` (default false), suppress informational progress
%     messages.
%     * upgrade(+Boolean)
%     If `true` (default `false`), upgrade package if it is already
%     installed.
%     * git(+Boolean)
%     If `true` (default `false` unless `URL` ends with =.git=),
%     assume the URL is a GIT repository.
%
%   Non-interactive installation can be established using the option
%   interactive(false). It is adviced to   install from a particular
%   _trusted_ URL instead of the  plain   pack  name  for unattented
%   operation.

pack_install(Spec, Options) :-
    pack_default_options(Spec, Pack, Options, DefOptions),
    (   option(already_installed(Installed), DefOptions)
    ->  print_message(informational, pack(already_installed(Installed)))
    ;   merge_options(Options, DefOptions, PackOptions),
        update_dependency_db,
        pack_install_dir(PackDir, PackOptions),
        pack_install(Pack, PackDir, PackOptions)
    ).

pack_install_dir(PackDir, Options) :-
    option(package_directory(PackDir), Options),
    !.
pack_install_dir(PackDir, _Options) :-          % TBD: global/user?
    absolute_file_name(pack(.), PackDir,
                       [ file_type(directory),
                         access(write),
                         file_errors(fail)
                       ]),
    !.
pack_install_dir(PackDir, Options) :-           % TBD: global/user?
    pack_create_install_dir(PackDir, Options).

pack_create_install_dir(PackDir, Options) :-
    findall(Candidate = create_dir(Candidate),
            ( absolute_file_name(pack(.), Candidate, [solutions(all)]),
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


%!  pack_install(+Pack, +PackDir, +Options)
%
%   Install package Pack into PackDir.  Options:
%
%     - url(URL)
%     Install from the given URL, URL is either a file://, a git URL
%     or a download URL.
%     - upgrade(Boolean)
%     If Pack is already installed and Boolean is `true`, update the
%     package to the latest version.  If Boolean is `false` print
%     an error and fail.

pack_install(Name, _, Options) :-
    current_pack(Name),
    option(upgrade(false), Options, false),
    print_message(error, pack(already_installed(Name))),
    pack_info(Name),
    print_message(information, pack(remove_with(Name))),
    !,
    fail.
pack_install(Name, PackDir, Options) :-
    option(url(URL), Options),
    uri_file_name(URL, Source),
    !,
    pack_install_from_local(Source, PackDir, Name, Options).
pack_install(Name, PackDir, Options) :-
    option(url(URL), Options),
    uri_components(URL, Components),
    uri_data(scheme, Components, Scheme),
    pack_install_from_url(Scheme, URL, PackDir, Name, Options).

%!  pack_install_from_local(+Source, +PackTopDir, +Name, +Options)
%
%   Install a package from a local media.
%
%   @tbd    Provide an option to install directories using a
%           link (or file-links).

pack_install_from_local(Source, PackTopDir, Name, Options) :-
    exists_directory(Source),
    !,
    directory_file_path(PackTopDir, Name, PackDir),
    prepare_pack_dir(PackDir, Options),
    copy_directory(Source, PackDir),
    pack_post_install(Name, PackDir, Options).
pack_install_from_local(Source, PackTopDir, Name, Options) :-
    exists_file(Source),
    directory_file_path(PackTopDir, Name, PackDir),
    prepare_pack_dir(PackDir, Options),
    pack_unpack(Source, PackDir, Name, Options),
    pack_post_install(Name, PackDir, Options).


%!  pack_unpack(+SourceFile, +PackDir, +Pack, +Options)
%
%   Unpack an archive to the given package dir.

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
    maplist(valid_info_term, Info).
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
    directory_file_path(GitDir, 'pack.pl', InfoFile),
    read_file_to_terms(InfoFile, Info, [encoding(utf8)]),
    must_be(ground, Info),
    maplist(valid_info_term, Info).

%!  download_file_sanity_check(+Archive, +Pack, +Info) is semidet.
%
%   Perform basic sanity checks on DownloadFile

download_file_sanity_check(Archive, Pack, Info) :-
    info_field(name(Name), Info),
    info_field(version(VersionAtom), Info),
    atom_version(VersionAtom, Version),
    pack_version_file(PackA, VersionA, Archive),
    must_match([Pack, PackA, Name], name),
    must_match([Version, VersionA], version).

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
%   Prepare for installing the package into  Dir. This should create
%   Dir if it does not  exist  and   warn  if  the directory already
%   exists, asking to make it empty.

prepare_pack_dir(Dir, Options) :-
    exists_directory(Dir),
    !,
    (   empty_directory(Dir)
    ->  true
    ;   option(upgrade(true), Options)
    ->  delete_directory_contents(Dir)
    ;   confirm(remove_existing_pack(Dir), yes, Options),
        delete_directory_contents(Dir)
    ).
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


%!  pack_install_from_url(+Scheme, +URL, +PackDir, +Pack, +Options)
%
%   Install a package from a remote source. For git repositories, we
%   simply clone. Archives are  downloaded.   We  currently  use the
%   built-in HTTP client. For complete  coverage, we should consider
%   using an external (e.g., curl) if available.

pack_install_from_url(_, URL, PackTopDir, Pack, Options) :-
    option(git(true), Options),
    !,
    directory_file_path(PackTopDir, Pack, PackDir),
    prepare_pack_dir(PackDir, Options),
    run_process(path(git), [clone, URL, PackDir], []),
    pack_git_info(PackDir, Hash, Info),
    pack_inquiry(URL, git(Hash), Info, Options),
    show_info(Pack, Info, Options),
    confirm(git_post_install(PackDir, Pack), yes, Options),
    pack_post_install(Pack, PackDir, Options).
pack_install_from_url(Scheme, URL, PackTopDir, Pack, Options) :-
    download_scheme(Scheme),
    directory_file_path(PackTopDir, Pack, PackDir),
    prepare_pack_dir(PackDir, Options),
    pack_download_dir(PackTopDir, DownLoadDir),
    download_file(URL, Pack, DownloadBase, Options),
    directory_file_path(DownLoadDir, DownloadBase, DownloadFile),
    setup_call_cleanup(
        http_open(URL, In,
                  [ cert_verify_hook(ssl_verify)
                  ]),
        setup_call_cleanup(
            open(DownloadFile, write, Out, [type(binary)]),
            copy_stream_data(In, Out),
            close(Out)),
        close(In)),
    pack_archive_info(DownloadFile, Pack, Info, _),
    download_file_sanity_check(DownloadFile, Pack, Info),
    pack_inquiry(URL, DownloadFile, Info, Options),
    show_info(Pack, Info, Options),
    confirm(install_downloaded(DownloadFile), yes, Options),
    pack_install_from_local(DownloadFile, PackTopDir, Pack, Options).

%!  download_file(+URL, +Pack, -File, +Options) is det.

download_file(URL, Pack, File, Options) :-
    option(version(Version), Options),
    !,
    atom_version(VersionA, Version),
    file_name_extension(_, Ext, URL),
    format(atom(File), '~w-~w.~w', [Pack, VersionA, Ext]).
download_file(URL, Pack, File, _) :-
    file_base_name(URL,Basename),
    no_int_file_name_extension(Tag,Ext,Basename),
    tag_version(Tag,Version),
    !,
    atom_version(VersionA,Version),
    format(atom(File0), '~w-~w', [Pack, VersionA]),
    file_name_extension(File0, Ext, File).
download_file(URL, _, File, _) :-
    file_base_name(URL, File).

%!  pack_url_file(+URL, -File) is det.
%
%   True if File is a unique id for the referenced pack and version.
%   Normally, that is simply the  base   name,  but  GitHub archives
%   destroy this picture. Needed by the pack manager.

pack_url_file(URL, FileID) :-
    github_release_url(URL, Pack, Version),
    !,
    download_file(URL, Pack, FileID, [version(Version)]).
pack_url_file(URL, FileID) :-
    file_base_name(URL, FileID).


:- public ssl_verify/5.

%!  ssl_verify(+SSL, +ProblemCert, +AllCerts, +FirstCert, +Error)
%
%   Currently we accept  all  certificates.   We  organise  our  own
%   security using SHA1 signatures, so  we   do  not  care about the
%   source of the data.

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

%!  download_url(+URL) is det.
%
%   True if URL looks like a URL we can download from.

download_url(URL) :-
    atom(URL),
    uri_components(URL, Components),
    uri_data(scheme, Components, Scheme),
    download_scheme(Scheme).

download_scheme(http).
download_scheme(https) :-
    catch(use_module(library(http/http_ssl_plugin)),
          E, (print_message(warning, E), fail)).

%!  pack_post_install(+Pack, +PackDir, +Options) is det.
%
%   Process post installation work.  Steps:
%
%     - Create foreign resources [TBD]
%     - Register directory as autoload library
%     - Attach the package

pack_post_install(Pack, PackDir, Options) :-
    post_install_foreign(Pack, PackDir,
                         [ build_foreign(if_absent)
                         | Options
                         ]),
    post_install_autoload(PackDir, Options),
    '$pack_attach'(PackDir).

%!  pack_rebuild(+Pack) is det.
%
%   Rebuilt possible foreign components of Pack.

pack_rebuild(Pack) :-
    '$pack':pack(Pack, BaseDir),
    !,
    catch(pack_make(BaseDir, [distclean], []), E,
          print_message(warning, E)),
    post_install_foreign(Pack, BaseDir, []).
pack_rebuild(Pack) :-
    existence_error(pack, Pack).

%!  pack_rebuild is det.
%
%   Rebuild foreign components of all packages.

pack_rebuild :-
    forall(current_pack(Pack),
           ( print_message(informational, pack(rebuild(Pack))),
             pack_rebuild(Pack)
           )).


%!  post_install_foreign(+Pack, +PackDir, +Options) is det.
%
%   Install foreign parts of the package.

post_install_foreign(Pack, PackDir, Options) :-
    is_foreign_pack(PackDir),
    !,
    (   option(build_foreign(if_absent), Options),
        foreign_present(PackDir)
    ->  print_message(informational, pack(kept_foreign(Pack)))
    ;   setup_path,
        save_build_environment(PackDir),
        configure_foreign(PackDir, Options),
        make_foreign(PackDir, Options)
    ).
post_install_foreign(_, _, _).

foreign_present(PackDir) :-
    current_prolog_flag(arch, Arch),
    atomic_list_concat([PackDir, '/lib'], ForeignBaseDir),
    exists_directory(ForeignBaseDir),
    !,
    atomic_list_concat([PackDir, '/lib/', Arch], ForeignDir),
    exists_directory(ForeignDir),
    current_prolog_flag(shared_object_extension, Ext),
    atomic_list_concat([ForeignDir, '/*.', Ext], Pattern),
    expand_file_name(Pattern, Files),
    Files \== [].

is_foreign_pack(PackDir) :-
    foreign_file(File),
    directory_file_path(PackDir, File, Path),
    exists_file(Path),
    !.

foreign_file('configure.in').
foreign_file('configure.ac').
foreign_file('configure').
foreign_file('Makefile').
foreign_file('makefile').


%!  configure_foreign(+PackDir, +Options) is det.
%
%   Run configure if it exists.  If =|configure.ac|= or =|configure.in|=
%   exists, first run =autoheader= and =autoconf=

configure_foreign(PackDir, Options) :-
    make_configure(PackDir, Options),
    directory_file_path(PackDir, configure, Configure),
    exists_file(Configure),
    !,
    build_environment(BuildEnv),
    run_process(path(bash), [Configure],
                [ env(BuildEnv),
                  directory(PackDir)
                ]).
configure_foreign(_, _).

make_configure(PackDir, _Options) :-
    directory_file_path(PackDir, 'configure', Configure),
    exists_file(Configure),
    !.
make_configure(PackDir, _Options) :-
    autoconf_master(ConfigMaster),
    directory_file_path(PackDir, ConfigMaster, ConfigureIn),
    exists_file(ConfigureIn),
    !,
    run_process(path(autoheader), [], [directory(PackDir)]),
    run_process(path(autoconf),   [], [directory(PackDir)]).
make_configure(_, _).

autoconf_master('configure.ac').
autoconf_master('configure.in').


%!  make_foreign(+PackDir, +Options) is det.
%
%   Generate the foreign executable.

make_foreign(PackDir, Options) :-
    pack_make(PackDir, [all, check, install], Options).

pack_make(PackDir, Targets, _Options) :-
    directory_file_path(PackDir, 'Makefile', Makefile),
    exists_file(Makefile),
    !,
    build_environment(BuildEnv),
    ProcessOptions = [ directory(PackDir), env(BuildEnv) ],
    forall(member(Target, Targets),
           run_process(path(make), [Target], ProcessOptions)).
pack_make(_, _, _).

%!  save_build_environment(+PackDir)
%
%   Create  a  shell-script  build.env  that    contains  the  build
%   environment.

save_build_environment(PackDir) :-
    directory_file_path(PackDir, 'buildenv.sh', EnvFile),
    build_environment(Env),
    setup_call_cleanup(
        open(EnvFile, write, Out),
        write_env_script(Out, Env),
        close(Out)).

write_env_script(Out, Env) :-
    format(Out,
           '# This file contains the environment that can be used to\n\c
                # build the foreign pack outside Prolog.  This file must\n\c
                # be loaded into a bourne-compatible shell using\n\c
                #\n\c
                #   $ source buildenv.sh\n\n',
           []),
    forall(member(Var=Value, Env),
           format(Out, '~w=\'~w\'\n', [Var, Value])),
    format(Out, '\nexport ', []),
    forall(member(Var=_, Env),
           format(Out, ' ~w', [Var])),
    format(Out, '\n', []).

build_environment(Env) :-
    findall(Name=Value, environment(Name, Value), UserEnv),
    findall(Name=Value,
            ( def_environment(Name, Value),
              \+ memberchk(Name=_, UserEnv)
            ),
            DefEnv),
    append(UserEnv, DefEnv, Env).


%!  environment(-Name, -Value) is nondet.
%
%   Hook  to  define  the  environment   for  building  packs.  This
%   Multifile hook extends the  process   environment  for  building
%   foreign extensions. A value  provided   by  this  hook overrules
%   defaults provided by def_environment/2. In  addition to changing
%   the environment, this may be used   to pass additional values to
%   the environment, as in:
%
%     ==
%     prolog_pack:environment('USER', User) :-
%         getenv('USER', User).
%     ==
%
%   @param Name is an atom denoting a valid variable name
%   @param Value is either an atom or number representing the
%          value of the variable.


%!  def_environment(-Name, -Value) is nondet.
%
%   True if Name=Value must appear in   the environment for building
%   foreign extensions.

def_environment('PATH', Value) :-
    getenv('PATH', PATH),
    current_prolog_flag(executable, Exe),
    file_directory_name(Exe, ExeDir),
    prolog_to_os_filename(ExeDir, OsExeDir),
    (   current_prolog_flag(windows, true)
    ->  Sep = (;)
    ;   Sep = (:)
    ),
    atomic_list_concat([OsExeDir, Sep, PATH], Value).
def_environment('SWIPL', Value) :-
    current_prolog_flag(executable, Value).
def_environment('SWIPLVERSION', Value) :-
    current_prolog_flag(version, Value).
def_environment('SWIHOME', Value) :-
    current_prolog_flag(home, Value).
def_environment('SWIARCH', Value) :-
    current_prolog_flag(arch, Value).
def_environment('PACKSODIR', Value) :-
    current_prolog_flag(arch, Arch),
    atom_concat('lib/', Arch, Value).
def_environment('SWISOLIB', Value) :-
    current_prolog_flag(c_libplso, Value).
def_environment('SWILIB', '-lswipl').
def_environment('CC', Value) :-
    (   getenv('CC', Value)
    ->  true
    ;   default_c_compiler(Value)
    ->  true
    ;   current_prolog_flag(c_cc, Value)
    ).
def_environment('LD', Value) :-
    (   getenv('LD', Value)
    ->  true
    ;   current_prolog_flag(c_cc, Value)
    ).
def_environment('CFLAGS', Value) :-
    (   getenv('CFLAGS', SystemFlags)
    ->  Extra = [' ', SystemFlags]
    ;   Extra = []
    ),
    current_prolog_flag(c_cflags, Value0),
    current_prolog_flag(home, Home),
    atomic_list_concat([Value0, ' -I"', Home, '/include"' | Extra], Value).
def_environment('LDSOFLAGS', Value) :-
    (   getenv('LDFLAGS', SystemFlags)
    ->  Extra = [SystemFlags|System]
    ;   Extra = System
    ),
    (   current_prolog_flag(windows, true)
    ->  current_prolog_flag(home, Home),
        atomic_list_concat(['-L"', Home, '/bin"'], SystemLib),
        System = [SystemLib]
    ;   apple_bundle_libdir(LibDir)
    ->  atomic_list_concat(['-L"', LibDir, '"'], SystemLib),
        System = [SystemLib]
    ;   current_prolog_flag(c_libplso, '')
    ->  System = []                 % ELF systems do not need this
    ;   prolog_library_dir(SystemLibDir),
        atomic_list_concat(['-L"',SystemLibDir,'"'], SystemLib),
        System = [SystemLib]
    ),
    current_prolog_flag(c_ldflags, LDFlags),
    atomic_list_concat([LDFlags, '-shared' | Extra], ' ', Value).
def_environment('SOEXT', Value) :-
    current_prolog_flag(shared_object_extension, Value).
def_environment(Pass, Value) :-
    pass_env(Pass),
    getenv(Pass, Value).

pass_env('TMP').
pass_env('TEMP').
pass_env('USER').
pass_env('HOME').

:- multifile
    prolog:runtime_config/2.

prolog_library_dir(Dir) :-
    prolog:runtime_config(c_libdir, Dir),
    !.
prolog_library_dir(Dir) :-
    current_prolog_flag(home, Home),
    (   current_prolog_flag(c_libdir, Rel)
    ->  atomic_list_concat([Home, Rel], /, Dir)
    ;   current_prolog_flag(arch, Arch)
    ->  atomic_list_concat([Home, lib, Arch], /, Dir)
    ).

%!  default_c_compiler(-CC) is semidet.
%
%   Try to find a  suitable  C   compiler  for  compiling  packages with
%   foreign code.
%
%   @tbd Needs proper defaults for Windows.  Find MinGW?  Find MSVC?

default_c_compiler(CC) :-
    preferred_c_compiler(CC),
    has_program(path(CC), _),
    !.

preferred_c_compiler(gcc).
preferred_c_compiler(clang).
preferred_c_compiler(cc).


                 /*******************************
                 *             PATHS            *
                 *******************************/

setup_path :-
    has_program(path(make), _),
    has_program(path(gcc), _),
    !.
setup_path :-
    current_prolog_flag(windows, true),
    !,
    (   mingw_extend_path
    ->  true
    ;   print_message(error, pack(no_mingw))
    ).
setup_path.

has_program(Program, Path) :-
    exe_options(ExeOptions),
    absolute_file_name(Program, Path,
                       [ file_errors(fail)
                       | ExeOptions
                       ]).

exe_options(Options) :-
    current_prolog_flag(windows, true),
    !,
    Options = [ extensions(['',exe,com]), access(read) ].
exe_options(Options) :-
    Options = [ access(execute) ].

mingw_extend_path :-
    mingw_root(MinGW),
    directory_file_path(MinGW, bin, MinGWBinDir),
    atom_concat(MinGW, '/msys/*/bin', Pattern),
    expand_file_name(Pattern, MsysDirs),
    last(MsysDirs, MSysBinDir),
    prolog_to_os_filename(MinGWBinDir, WinDirMinGW),
    prolog_to_os_filename(MSysBinDir, WinDirMSYS),
    getenv('PATH', Path0),
    atomic_list_concat([WinDirMSYS, WinDirMinGW, Path0], ';', Path),
    setenv('PATH', Path).

mingw_root(MinGwRoot) :-
    current_prolog_flag(executable, Exe),
    sub_atom(Exe, 1, _, _, :),
    sub_atom(Exe, 0, 1, _, PlDrive),
    Drives = [PlDrive,c,d],
    member(Drive, Drives),
    format(atom(MinGwRoot), '~a:/MinGW', [Drive]),
    exists_directory(MinGwRoot),
    !.


                 /*******************************
                 *           AUTOLOAD           *
                 *******************************/

%!  post_install_autoload(+PackDir, +Options)
%
%   Create an autoload index if the package demands such.

post_install_autoload(PackDir, Options) :-
    option(autoload(true), Options, true),
    pack_info_term(PackDir, autoload(true)),
    !,
    directory_file_path(PackDir, prolog, PrologLibDir),
    make_library_index(PrologLibDir).
post_install_autoload(_, _).


                 /*******************************
                 *            UPGRADE           *
                 *******************************/

%!  pack_upgrade(+Pack) is semidet.
%
%   Try to upgrade the package Pack.
%
%   @tbd    Update dependencies when updating a pack from git?

pack_upgrade(Pack) :-
    pack_info(Pack, _, directory(Dir)),
    directory_file_path(Dir, '.git', GitDir),
    exists_directory(GitDir),
    !,
    print_message(informational, pack(git_fetch(Dir))),
    git([fetch], [ directory(Dir) ]),
    git_describe(V0, [ directory(Dir) ]),
    git_describe(V1, [ directory(Dir), commit('origin/master') ]),
    (   V0 == V1
    ->  print_message(informational, pack(up_to_date(Pack)))
    ;   confirm(upgrade(Pack, V0, V1), yes, []),
        git([merge, 'origin/master'], [ directory(Dir) ]),
        pack_rebuild(Pack)
    ).
pack_upgrade(Pack) :-
    once(pack_info(Pack, _, version(VersionAtom))),
    atom_version(VersionAtom, Version),
    pack_info(Pack, _, download(URL)),
    (   wildcard_pattern(URL)
    ->  true
    ;   github_url(URL, _User, _Repo)
    ),
    !,
    available_download_versions(URL, [Latest-LatestURL|_Versions]),
    (   Latest @> Version
    ->  confirm(upgrade(Pack, Version, Latest), yes, []),
        pack_install(Pack,
                     [ url(LatestURL),
                       upgrade(true),
                       pack(Pack)
                     ])
    ;   print_message(informational, pack(up_to_date(Pack)))
    ).
pack_upgrade(Pack) :-
    print_message(warning, pack(no_upgrade_info(Pack))).


                 /*******************************
                 *            REMOVE            *
                 *******************************/

%!  pack_remove(+Name) is det.
%
%   Remove the indicated package.

pack_remove(Pack) :-
    update_dependency_db,
    (   setof(Dep, pack_depends_on(Dep, Pack), Deps)
    ->  confirm_remove(Pack, Deps, Delete),
        forall(member(P, Delete), pack_remove_forced(P))
    ;   pack_remove_forced(Pack)
    ).

pack_remove_forced(Pack) :-
    catch('$pack_detach'(Pack, BaseDir),
          error(existence_error(pack, Pack), _),
          fail),
    !,
    print_message(informational, pack(remove(BaseDir))),
    delete_directory_and_contents(BaseDir).
pack_remove_forced(Pack) :-
    directory_file_path(Pack, 'pack.pl', PackFile),
    absolute_file_name(pack(PackFile), PackPath,
                       [ access(read),
                         file_errors(fail)
                       ]),
    !,
    file_directory_name(PackPath, BaseDir),
    delete_directory_and_contents(BaseDir).
pack_remove_forced(Pack) :-
    print_message(informational, error(existence_error(pack, Pack),_)).

confirm_remove(Pack, Deps, Delete) :-
    print_message(warning, pack(depends(Pack, Deps))),
    menu(pack(resolve_remove),
         [ [Pack]      = remove_only(Pack),
           [Pack|Deps] = remove_deps(Pack, Deps),
           []          = cancel
         ], [], Delete, []),
    Delete \== [].


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
                 *             GIT              *
                 *******************************/

%!  git_url(+URL, -Pack) is semidet.
%
%   True if URL describes a git url for Pack

git_url(URL, Pack) :-
    uri_components(URL, Components),
    uri_data(scheme, Components, Scheme),
    uri_data(path, Components, Path),
    (   Scheme == git
    ->  true
    ;   git_download_scheme(Scheme),
        file_name_extension(_, git, Path)
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


                 /*******************************
                 *         VERSION LOGIC        *
                 *******************************/

%!  pack_version_file(-Pack, -Version, +File) is semidet.
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



%!  github_release_url(+URL, -Pack, -Version) is semidet.
%
%   True when URL is the URL of a GitHub release.  Such releases are
%   accessible as
%
%     ==
%     https:/github.com/<owner>/<pack>/archive/[vV]?<version>.zip'
%     ==

github_release_url(URL, Pack, Version) :-
    uri_components(URL, Components),
    uri_data(authority, Components, 'github.com'),
    uri_data(scheme, Components, Scheme),
    download_scheme(Scheme),
    uri_data(path, Components, Path),
    atomic_list_concat(['',_Project,Pack,archive,File], /, Path),
    file_name_extension(Tag, Ext, File),
    github_archive_extension(Ext),
    tag_version(Tag, Version),
    !.

github_archive_extension(tgz).
github_archive_extension(zip).

tag_version(Tag, Version) :-
    version_tag_prefix(Prefix),
    atom_concat(Prefix, AtomVersion, Tag),
    atom_version(AtomVersion, Version).

version_tag_prefix(v).
version_tag_prefix('V').
version_tag_prefix('').


:- public
    atom_version/2.

%!  atom_version(?Atom, ?Version)
%
%   Translate   between   atomic   version   representation   and   term
%   representation.  The  term  representation  is  a  list  of  version
%   components as integers and can be compared using `@>`

atom_version(Atom, version(Parts)) :-
    (   atom(Atom)
    ->  atom_codes(Atom, Codes),
        phrase(version(Parts), Codes)
    ;   atomic_list_concat(Parts, '.', Atom)
    ).

pack_version(Pack, version(Parts)) -->
    string(Codes), "-",
    version(Parts),
    !,
    { atom_codes(Pack, Codes)
    }.

version([_|T]) -->
    "*",
    !,
    (   "."
    ->  version(T)
    ;   []
    ).
version([H|T]) -->
    integer(H),
    (   "."
    ->  version(T)
    ;   { T = [] }
    ).

integer(H)    --> digit(D0), digits(L), { number_codes(H, [D0|L]) }.
digit(D)      --> [D], { code_type(D, digit) }.
digits([H|T]) --> digit(H), !, digits(T).
digits([])    --> [].


                 /*******************************
                 *       QUERY CENTRAL DB       *
                 *******************************/

%!  pack_inquiry(+URL, +DownloadFile, +Info, +Options) is semidet.
%
%   Query the status of a package  with   the  central repository. To do
%   this, we POST a Prolog document  containing   the  URL, info and the
%   SHA1 hash to http://www.swi-prolog.org/pack/eval. The server replies
%   using a list of Prolog terms, described  below. The only member that
%   is always included is downloads (with default value 0).
%
%     - alt_hash(Count, URLs, Hash)
%       A file with the same base-name, but a different hash was
%       found at URLs and downloaded Count times.
%     - downloads(Count)
%       Number of times a file with this hash was downloaded.
%     - rating(VoteCount, Rating)
%       User rating (1..5), provided based on VoteCount votes.
%     - dependency(Token, Pack, Version, URLs, SubDeps)
%       Required tokens can be provided by the given provides.

pack_inquiry(_, _, _, Options) :-
    option(inquiry(false), Options),
    !.
pack_inquiry(URL, DownloadFile, Info, Options) :-
    setting(server, ServerBase),
    ServerBase \== '',
    atom_concat(ServerBase, query, Server),
    (   option(inquiry(true), Options)
    ->  true
    ;   confirm(inquiry(Server), yes, Options)
    ),
    !,
    (   DownloadFile = git(SHA1)
    ->  true
    ;   file_sha1(DownloadFile, SHA1)
    ),
    query_pack_server(install(URL, SHA1, Info), Reply, Options),
    inquiry_result(Reply, URL, Options).
pack_inquiry(_, _, _, _).


%!  query_pack_server(+Query, -Result, +Options)
%
%   Send a Prolog query  to  the   package  server  and  process its
%   results.

query_pack_server(Query, Result, Options) :-
    setting(server, ServerBase),
    ServerBase \== '',
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


%!  inquiry_result(+Reply, +File, +Options) is semidet.
%
%   Analyse the results  of  the  inquiry   and  decide  whether  to
%   continue or not.

inquiry_result(Reply, File, Options) :-
    findall(Eval, eval_inquiry(Reply, File, Eval, Options), Evaluation),
    \+ member(cancel, Evaluation),
    select_option(git(_), Options, Options1, _),
    forall(member(install_dependencies(Resolution), Evaluation),
           maplist(install_dependency(Options1), Resolution)).

eval_inquiry(true(Reply), URL, Eval, _) :-
    include(alt_hash, Reply, Alts),
    Alts \== [],
    print_message(warning, pack(alt_hashes(URL, Alts))),
    (   memberchk(downloads(Count), Reply),
        (   git_url(URL, _)
        ->  Default = yes,
            Eval = with_git_commits_in_same_version
        ;   Default = no,
            Eval = with_alt_hashes
        ),
        confirm(continue_with_alt_hashes(Count, URL), Default, [])
    ->  true
    ;   !,                          % Stop other rules
        Eval = cancel
    ).
eval_inquiry(true(Reply), _, Eval, Options) :-
    include(dependency, Reply, Deps),
    Deps \== [],
    select_dependency_resolution(Deps, Eval, Options),
    (   Eval == cancel
    ->  !
    ;   true
    ).
eval_inquiry(true(Reply), URL, true, Options) :-
    file_base_name(URL, File),
    info_level(Informational, Options),
    print_message(Informational, pack(inquiry_ok(Reply, File))).
eval_inquiry(exception(pack(modified_hash(_SHA1-URL, _SHA2-[URL]))),
             URL, Eval, Options) :-
    (   confirm(continue_with_modified_hash(URL), no, Options)
    ->  Eval = true
    ;   Eval = cancel
    ).

alt_hash(alt_hash(_,_,_)).
dependency(dependency(_,_,_,_,_)).


%!  select_dependency_resolution(+Deps, -Eval, +Options)
%
%   Select a resolution.
%
%   @tbd    Exploit backtracking over resolve_dependencies/2.

select_dependency_resolution(Deps, Eval, Options) :-
    resolve_dependencies(Deps, Resolution),
    exclude(local_dep, Resolution, ToBeDone),
    (   ToBeDone == []
    ->  !, Eval = true
    ;   print_message(warning, pack(install_dependencies(Resolution))),
        (   memberchk(_-unresolved, Resolution)
        ->  Default = cancel
        ;   Default = install_deps
        ),
        menu(pack(resolve_deps),
             [ install_deps    = install_deps,
               install_no_deps = install_no_deps,
               cancel          = cancel
             ], Default, Choice, Options),
        (   Choice == cancel
        ->  !, Eval = cancel
        ;   Choice == install_no_deps
        ->  !, Eval = install_no_deps
        ;   !, Eval = install_dependencies(Resolution)
        )
    ).

local_dep(_-resolved(_)).


%!  install_dependency(+Options, +TokenResolution)
%
%   Install dependencies for the given resolution.
%
%   @tbd: Query URI to use

install_dependency(Options,
                   _Token-resolve(Pack, VersionAtom, [_URL|_], SubResolve)) :-
    atom_version(VersionAtom, Version),
    current_pack(Pack),
    pack_info(Pack, _, version(InstalledAtom)),
    atom_version(InstalledAtom, Installed),
    Installed == Version,               % already installed
    !,
    maplist(install_dependency(Options), SubResolve).
install_dependency(Options,
                   _Token-resolve(Pack, VersionAtom, [URL|_], SubResolve)) :-
    !,
    atom_version(VersionAtom, Version),
    merge_options([ url(URL),
                    version(Version),
                    interactive(false),
                    inquiry(false),
                    info(list),
                    pack(Pack)
                  ], Options, InstallOptions),
    pack_install(Pack, InstallOptions),
    maplist(install_dependency(Options), SubResolve).
install_dependency(_, _-_).


                 /*******************************
                 *        WILDCARD URIs         *
                 *******************************/

%!  available_download_versions(+URL, -Versions) is det.
%
%   Deal with wildcard URLs, returning a  list of Version-URL pairs,
%   sorted by version.
%
%   @tbd    Deal with protocols other than HTTP

available_download_versions(URL, Versions) :-
    wildcard_pattern(URL),
    github_url(URL, User, Repo),
    !,
    findall(Version-VersionURL,
            github_version(User, Repo, Version, VersionURL),
            Versions).
available_download_versions(URL, Versions) :-
    wildcard_pattern(URL),
    !,
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
    keysort(VersionedURLs, SortedVersions),
    reverse(SortedVersions, Versions),
    print_message(informational, pack(found_versions(Versions))).
available_download_versions(URL, [Version-URL]) :-
    (   pack_version_file(_Pack, Version0, URL)
    ->  Version = Version0
    ;   Version = unknown
    ).

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

%!  update_dependency_db
%
%   Reload dependency declarations between packages.

update_dependency_db :-
    retractall(pack_requires(_,_)),
    retractall(pack_provides_db(_,_)),
    forall(current_pack(Pack),
           (   findall(Info, pack_info(Pack, dependency, Info), Infos),
               update_dependency_db(Pack, Infos)
           )).

update_dependency_db(Name, Info) :-
    retractall(pack_requires(Name, _)),
    retractall(pack_provides_db(Name, _)),
    maplist(assert_dep(Name), Info).

assert_dep(Pack, provides(Token)) :-
    !,
    assertz(pack_provides_db(Pack, Token)).
assert_dep(Pack, requires(Token)) :-
    !,
    assertz(pack_requires(Pack, Token)).
assert_dep(_, _).

%!  validate_dependencies is det.
%
%   Validate all dependencies, reporting on failures

validate_dependencies :-
    unsatisfied_dependencies(Unsatisfied),
    !,
    print_message(warning, pack(unsatisfied(Unsatisfied))).
validate_dependencies.


unsatisfied_dependencies(Unsatisfied) :-
    findall(Req-Pack, pack_requires(Pack, Req), Reqs0),
    keysort(Reqs0, Reqs1),
    group_pairs_by_key(Reqs1, GroupedReqs),
    exclude(satisfied_dependency, GroupedReqs, Unsatisfied),
    Unsatisfied \== [].

satisfied_dependency(Needed-_By) :-
    pack_provides(_, Needed),
    !.
satisfied_dependency(Needed-_By) :-
    compound(Needed),
    Needed =.. [Op, Pack, ReqVersion],
    (   pack_provides(Pack, Pack)
    ->  pack_info(Pack, _, version(PackVersion)),
        version_data(PackVersion, PackData)
    ;   Pack == prolog
    ->  current_prolog_flag(version_data, swi(Major,Minor,Patch,_)),
        PackData = [Major,Minor,Patch]
    ),
    version_data(ReqVersion, ReqData),
    cmp(Op, Cmp),
    call(Cmp, PackData, ReqData).

%!  pack_provides(?Package, ?Token) is multi.
%
%   True if Pack provides Token.  A package always provides itself.

pack_provides(Pack, Pack) :-
    current_pack(Pack).
pack_provides(Pack, Token) :-
    pack_provides_db(Pack, Token).

%!  pack_depends_on(?Pack, ?Dependency) is nondet.
%
%   True if Pack requires Dependency, direct or indirect.

pack_depends_on(Pack, Dependency) :-
    (   atom(Pack)
    ->  pack_depends_on_fwd(Pack, Dependency, [Pack])
    ;   pack_depends_on_bwd(Pack, Dependency, [Dependency])
    ).

pack_depends_on_fwd(Pack, Dependency, Visited) :-
    pack_depends_on_1(Pack, Dep1),
    \+ memberchk(Dep1, Visited),
    (   Dependency = Dep1
    ;   pack_depends_on_fwd(Dep1, Dependency, [Dep1|Visited])
    ).

pack_depends_on_bwd(Pack, Dependency, Visited) :-
    pack_depends_on_1(Dep1, Dependency),
    \+ memberchk(Dep1, Visited),
    (   Pack = Dep1
    ;   pack_depends_on_bwd(Pack, Dep1, [Dep1|Visited])
    ).

pack_depends_on_1(Pack, Dependency) :-
    atom(Dependency),
    !,
    pack_provides(Dependency, Token),
    pack_requires(Pack, Token).
pack_depends_on_1(Pack, Dependency) :-
    pack_requires(Pack, Token),
    pack_provides(Dependency, Token).


%!  resolve_dependencies(+Dependencies, -Resolution) is multi.
%
%   Resolve dependencies as reported by the remote package server.
%
%   @param  Dependencies is a list of
%           dependency(Token, Pack, Version, URLs, SubDeps)
%   @param  Resolution is a list of items
%           - Token-resolved(Pack)
%           - Token-resolve(Pack, Version, URLs, SubResolve)
%           - Token-unresolved
%   @tbd    Watch out for conflicts
%   @tbd    If there are different packs that resolve a token,
%           make an intelligent choice instead of using the first

resolve_dependencies(Dependencies, Resolution) :-
    maplist(dependency_pair, Dependencies, Pairs0),
    keysort(Pairs0, Pairs1),
    group_pairs_by_key(Pairs1, ByToken),
    maplist(resolve_dep, ByToken, Resolution).

dependency_pair(dependency(Token, Pack, Version, URLs, SubDeps),
                Token-(Pack-pack(Version,URLs, SubDeps))).

resolve_dep(Token-Pairs, Token-Resolution) :-
    (   resolve_dep2(Token-Pairs, Resolution)
    *-> true
    ;   Resolution = unresolved
    ).

resolve_dep2(Token-_, resolved(Pack)) :-
    pack_provides(Pack, Token).
resolve_dep2(_-Pairs, resolve(Pack, VersionAtom, URLs, SubResolves)) :-
    keysort(Pairs, Sorted),
    group_pairs_by_key(Sorted, ByPack),
    member(Pack-Versions, ByPack),
    Pack \== (-),
    maplist(version_pack, Versions, VersionData),
    sort(VersionData, ByVersion),
    reverse(ByVersion, ByVersionLatest),
    member(pack(Version,URLs,SubDeps), ByVersionLatest),
    atom_version(VersionAtom, Version),
    include(dependency, SubDeps, Deps),
    resolve_dependencies(Deps, SubResolves).

version_pack(pack(VersionAtom,URLs,SubDeps),
             pack(Version,URLs,SubDeps)) :-
    atom_version(VersionAtom, Version).


                 /*******************************
                 *          RUN PROCESSES       *
                 *******************************/

%!  run_process(+Executable, +Argv, +Options) is det.
%
%   Run Executable.  Defined options:
%
%     * directory(+Dir)
%     Execute in the given directory
%     * output(-Out)
%     Unify Out with a list of codes representing stdout of the
%     command.  Otherwise the output is handed to print_message/2
%     with level =informational=.
%     * error(-Error)
%     As output(Out), but messages are printed at level =error=.
%     * env(+Environment)
%     Environment passed to the new process.

run_process(Executable, Argv, Options) :-
    \+ option(output(_), Options),
    \+ option(error(_), Options),
    current_prolog_flag(unix, true),
    current_prolog_flag(threads, true),
    !,
    process_create_options(Options, Extra),
    process_create(Executable, Argv,
                   [ stdout(pipe(Out)),
                     stderr(pipe(Error)),
                     process(PID)
                   | Extra
                   ]),
    thread_create(relay_output([output-Out, error-Error]), Id, []),
    process_wait(PID, Status),
    thread_join(Id, _),
    (   Status == exit(0)
    ->  true
    ;   throw(error(process_error(process(Executable, Argv), Status), _))
    ).
run_process(Executable, Argv, Options) :-
    process_create_options(Options, Extra),
    setup_call_cleanup(
        process_create(Executable, Argv,
                       [ stdout(pipe(Out)),
                         stderr(pipe(Error)),
                         process(PID)
                       | Extra
                       ]),
        (   read_stream_to_codes(Out, OutCodes, []),
            read_stream_to_codes(Error, ErrorCodes, []),
            process_wait(PID, Status)
        ),
        (   close(Out),
            close(Error)
        )),
    print_error(ErrorCodes, Options),
    print_output(OutCodes, Options),
    (   Status == exit(0)
    ->  true
    ;   throw(error(process_error(process(Executable, Argv), Status), _))
    ).

process_create_options(Options, Extra) :-
    option(directory(Dir), Options, .),
    (   option(env(Env), Options)
    ->  Extra = [cwd(Dir), env(Env)]
    ;   Extra = [cwd(Dir)]
    ).

relay_output([]) :- !.
relay_output(Output) :-
    pairs_values(Output, Streams),
    wait_for_input(Streams, Ready, infinite),
    relay(Ready, Output, NewOutputs),
    relay_output(NewOutputs).

relay([], Outputs, Outputs).
relay([H|T], Outputs0, Outputs) :-
    selectchk(Type-H, Outputs0, Outputs1),
    (   at_end_of_stream(H)
    ->  close(H),
        relay(T, Outputs1, Outputs)
    ;   read_pending_codes(H, Codes, []),
        relay(Type, Codes),
        relay(T, Outputs0, Outputs)
    ).

relay(error,  Codes) :-
    set_prolog_flag(message_context, []),
    print_error(Codes, []).
relay(output, Codes) :-
    print_output(Codes, []).

print_output(OutCodes, Options) :-
    option(output(Codes), Options),
    !,
    Codes = OutCodes.
print_output(OutCodes, _) :-
    print_message(informational, pack(process_output(OutCodes))).

print_error(OutCodes, Options) :-
    option(error(Codes), Options),
    !,
    Codes = OutCodes.
print_error(OutCodes, _) :-
    phrase(classify_message(Level), OutCodes, _),
    print_message(Level, pack(process_output(OutCodes))).

classify_message(error) -->
    string(_), "fatal:",
    !.
classify_message(error) -->
    string(_), "error:",
    !.
classify_message(warning) -->
    string(_), "warning:",
    !.
classify_message(informational) -->
    [].

string([]) --> [].
string([H|T]) --> [H], string(T).


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
%   @param Default is one of =yes=, =no= or =none=.

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

message(invalid_info(Term)) -->
    [ 'Invalid package description: ~q'-[Term] ].
message(directory_exists(Dir)) -->
    [ 'Package target directory exists and is not empty:', nl,
      '\t~q'-[Dir]
    ].
message(already_installed(pack(Pack, Version))) -->
    { atom_version(AVersion, Version) },
    [ 'Pack `~w'' is already installed @~w'-[Pack, AVersion] ].
message(already_installed(Pack)) -->
    [ 'Pack `~w'' is already installed. Package info:'-[Pack] ].
message(invalid_name(File)) -->
    [ '~w: A package archive must be named <pack>-<version>.<ext>'-[File] ],
    no_tar_gz(File).

no_tar_gz(File) -->
    { sub_atom(File, _, _, 0, '.tar.gz') },
    !,
    [ nl,
      'Package archive files must have a single extension.  E.g., \'.tgz\''-[]
    ].
no_tar_gz(_) --> [].

message(kept_foreign(Pack)) -->
    [ 'Found foreign libraries for target platform.'-[], nl,
      'Use ?- pack_rebuild(~q). to rebuild from sources'-[Pack]
    ].
message(no_pack_installed(Pack)) -->
    [ 'No pack ~q installed.  Use ?- pack_list(Pattern) to search'-[Pack] ].
message(no_packages_installed) -->
    { setting(server, ServerBase) },
    [ 'There are no extra packages installed.', nl,
      'Please visit ~wlist.'-[ServerBase]
    ].
message(remove_with(Pack)) -->
    [ 'The package can be removed using: ?- ~q.'-[pack_remove(Pack)]
    ].
message(unsatisfied(Packs)) -->
    [ 'The following dependencies are not satisfied:', nl ],
    unsatisfied(Packs).
message(depends(Pack, Deps)) -->
    [ 'The following packages depend on `~w\':'-[Pack], nl ],
    pack_list(Deps).
message(remove(PackDir)) -->
    [ 'Removing ~q and contents'-[PackDir] ].
message(remove_existing_pack(PackDir)) -->
    [ 'Remove old installation in ~q'-[PackDir] ].
message(install_from(Pack, Version, git(URL))) -->
    [ 'Install ~w@~w from GIT at ~w'-[Pack, Version, URL] ].
message(install_from(Pack, Version, URL)) -->
    [ 'Install ~w@~w from ~w'-[Pack, Version, URL] ].
message(select_install_from(Pack, Version)) -->
    [ 'Select download location for ~w@~w'-[Pack, Version] ].
message(install_downloaded(File)) -->
    { file_base_name(File, Base),
      size_file(File, Size) },
    [ 'Install "~w" (~D bytes)'-[Base, Size] ].
message(git_post_install(PackDir, Pack)) -->
    (   { is_foreign_pack(PackDir) }
    ->  [ 'Run post installation scripts for pack "~w"'-[Pack] ]
    ;   [ 'Activate pack "~w"'-[Pack] ]
    ).
message(no_meta_data(BaseDir)) -->
    [ 'Cannot find pack.pl inside directory ~q.  Not a package?'-[BaseDir] ].
message(inquiry(Server)) -->
    [ 'Verify package status (anonymously)', nl,
      '\tat "~w"'-[Server]
    ].
message(search_no_matches(Name)) -->
    [ 'Search for "~w", returned no matching packages'-[Name] ].
message(rebuild(Pack)) -->
    [ 'Checking pack "~w" for rebuild ...'-[Pack] ].
message(upgrade(Pack, From, To)) -->
    [ 'Upgrade "~w" from '-[Pack] ],
    msg_version(From), [' to '-[]], msg_version(To).
message(up_to_date(Pack)) -->
    [ 'Package "~w" is up-to-date'-[Pack] ].
message(query_versions(URL)) -->
    [ 'Querying "~w" to find new versions ...'-[URL] ].
message(no_matching_urls(URL)) -->
    [ 'Could not find any matching URL: ~q'-[URL] ].
message(found_versions([Latest-_URL|More])) -->
    { length(More, Len),
      atom_version(VLatest, Latest)
    },
    [ '    Latest version: ~w (~D older)'-[VLatest, Len] ].
message(process_output(Codes)) -->
    { split_lines(Codes, Lines) },
    process_lines(Lines).
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
    { setof(PackDir,
            absolute_file_name(Alias, PackDir, [solutions(all)]),
            PackDirs)
    },
    [ 'Cannot find a place to create a package directory.'-[],
      'Considered:'-[]
    ],
    candidate_dirs(PackDirs).
message(no_match(Name)) -->
    [ 'No registered pack matches "~w"'-[Name] ].
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
message(pack(no_upgrade_info(Pack))) -->
    [ '~w: pack meta-data does not provide an upgradable URL'-[Pack] ].

candidate_dirs([]) --> [].
candidate_dirs([H|T]) --> [ nl, '    ~w'-[H] ], candidate_dirs(T).

message(no_mingw) -->
    [ 'Cannot find MinGW and/or MSYS.'-[] ].

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

% Alternate hashes for found for the same file

message(alt_hashes(URL, _Alts)) -->
    { git_url(URL, _)
    },
    !,
    [ 'GIT repository was updated without updating version' ].
message(alt_hashes(URL, Alts)) -->
    { file_base_name(URL, File)
    },
    [ 'Found multiple versions of "~w".'-[File], nl,
      'This could indicate a compromised or corrupted file', nl
    ],
    alt_hashes(Alts).
message(continue_with_alt_hashes(Count, URL)) -->
    [ 'Continue installation from "~w" (downloaded ~D times)'-[URL, Count] ].
message(continue_with_modified_hash(_URL)) -->
    [ 'Pack may be compromised.  Continue anyway'
    ].
message(modified_hash(_SHA1-URL, _SHA2-[URL])) -->
    [ 'Content of ~q has changed.'-[URL]
    ].

alt_hashes([]) --> [].
alt_hashes([H|T]) --> alt_hash(H), ( {T == []} -> [] ; [nl], alt_hashes(T) ).

alt_hash(alt_hash(Count, URLs, Hash)) -->
    [ '~t~d~8| ~w'-[Count, Hash] ],
    alt_urls(URLs).

alt_urls([]) --> [].
alt_urls([H|T]) -->
    [ nl, '    ~w'-[H] ],
    alt_urls(T).

% Installation dependencies gathered from inquiry server.

message(install_dependencies(Resolution)) -->
    [ 'Package depends on the following:' ],
    msg_res_tokens(Resolution, 1).

msg_res_tokens([], _) --> [].
msg_res_tokens([H|T], L) --> msg_res_token(H, L), msg_res_tokens(T, L).

msg_res_token(Token-unresolved, L) -->
    res_indent(L),
    [ '"~w" cannot be satisfied'-[Token] ].
msg_res_token(Token-resolve(Pack, Version, [URL|_], SubResolves), L) -->
    !,
    res_indent(L),
    [ '"~w", provided by ~w@~w from ~w'-[Token, Pack, Version, URL] ],
    { L2 is L+1 },
    msg_res_tokens(SubResolves, L2).
msg_res_token(Token-resolved(Pack), L) -->
    !,
    res_indent(L),
    [ '"~w", provided by installed pack ~w'-[Token,Pack] ].

res_indent(L) -->
    { I is L*2 },
    [ nl, '~*c'-[I,0'\s] ].

message(resolve_deps) -->
    [ nl, 'What do you wish to do' ].
label(install_deps) -->
    [ 'Install proposed dependencies' ].
label(install_no_deps) -->
    [ 'Only install requested package' ].


message(git_fetch(Dir)) -->
    [ 'Running "git fetch" in ~q'-[Dir] ].

% inquiry is blank

message(inquiry_ok(Reply, File)) -->
    { memberchk(downloads(Count), Reply),
      memberchk(rating(VoteCount, Rating), Reply),
      !,
      length(Stars, Rating),
      maplist(=(0'*), Stars)
    },
    [ '"~w" was downloaded ~D times.  Package rated ~s (~D votes)'-
      [ File, Count, Stars, VoteCount ]
    ].
message(inquiry_ok(Reply, File)) -->
    { memberchk(downloads(Count), Reply)
    },
    [ '"~w" was downloaded ~D times'-[ File, Count ] ].

                                                % support predicates
unsatisfied([]) --> [].
unsatisfied([Needed-[By]|T]) -->
    [ '  - "~w" is needed by package "~w"'-[Needed, By], nl ],
    unsatisfied(T).
unsatisfied([Needed-By|T]) -->
    [ '  - "~w" is needed by the following packages:'-[Needed], nl ],
    pack_list(By),
    unsatisfied(T).

pack_list([]) --> [].
pack_list([H|T]) -->
    [ '    - Package "~w"'-[H], nl ],
    pack_list(T).

process_lines([]) --> [].
process_lines([H|T]) -->
    [ '~s'-[H] ],
    (   {T==[]}
    ->  []
    ;   [nl], process_lines(T)
    ).

split_lines([], []) :- !.
split_lines(All, [Line1|More]) :-
    append(Line1, [0'\n|Rest], All),
    !,
    split_lines(Rest, More).
split_lines(Line, [Line]).

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
    { atom(Version) },
    !,
    [ '~w'-[Version] ].
msg_version(VersionData) -->
    !,
    { atom_version(Atom, VersionData) },
    [ '~w'-[Atom] ].
