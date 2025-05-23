/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Richard O'Keefe
    E-mail:        J.Wielemaker@cs.vu.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2014-2025, VU University Amsterdam
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

:- module(check_installation,
          [ check_installation/0,
            check_installation/1,               % -Issues
            check_config_files/0,
            update_config_files/0,
            test_installation/0,
            test_installation/1                 % +Options
          ]).
:- autoload(library(apply), [maplist/2, maplist/3]).
:- autoload(library(archive), [archive_open/3, archive_close/1]).
:- autoload(library(lists), [append/3, member/2]).
:- autoload(library(occurs), [sub_term/2]).
:- autoload(library(option), [option/2, merge_options/3]).
:- autoload(library(prolog_source), [path_segments_atom/2]).
:- use_module(library(settings), [setting/2]).
:- autoload(library(dcg/high_order), [sequence//2, sequence/4]).
:- autoload(library(error), [must_be/2]).


/** <module> Check installation issues and features

This library performs checks on  the   installed  system to verify which
optional components are available and  whether  all  libraries that load
shared objects/DLLs can be loaded.
*/

%!  component(?Component, -Features) is nondet.
%
%   This predicate describes the test components. Features is a dict
%   with the following components:
%
%     - test:Goal
%     (Additional) test that must succeed for the component to be
%     functional.
%     - url:URL
%     URL with additional information, relative to
%     =|http://www.swi-prolog.org/build/issues/|=.  If not provided,
%     the library file with extension =|.html|= is used.
%     - optional:true
%     If the library does not exist, do not complain.
%     - os:OS
%     One of =windows=, =unix= or =linux=. If present, the component
%     is only checked for if we are running on a version of the
%     specified operating system.
%     - features:Goal
%     After successful evaluation that loading and basic operation
%     of the component succeeds, run this to check additional
%     features.

% Feature tests
component(tcmalloc,
          _{ optional:true,
             test:test_tcmalloc,
             url:'tcmalloc.html',
             os:linux
           }).
component(gmp,
          _{ test:current_prolog_flag(bounded, false),
             url:'gmp.html'
           }).
% Packages that depend on foreign libraries
component(library(archive), _{features:archive_features}).
component(library(cgi), _{}).
component(library(crypt), _{}).
component(library(bdb), _{}).
component(library(double_metaphone), _{}).
component(library(editline), _{os:unix}).
component(library(filesex), _{}).
component(library(http/http_stream), _{}).
component(library(http/json), _{}).
component(library(http/jquery), _{features:jquery_file}).
component(library(isub), _{}).
component(library(janus), _{features:python_version}).
component(library(jpl), _{}).
component(library(memfile), _{}).
component(library(odbc), _{}).
component(library(pce),
          _{pre:use_foreign_library(pce_principal:foreign(pl2xpce)),
            url:'xpce.html'}).
component(library(pcre), _{features:pcre_features}).
component(library(pdt_console), _{}).
component(library(porter_stem), _{}).
component(library(process), _{}).
component(library(protobufs), _{}).
component(library(readline), _{os:unix}).
component(library(readutil), _{}).
component(library(rlimit), _{os:unix}).
component(library(semweb/rdf_db), _{}).
component(library(semweb/rdf_ntriples), _{}).
component(library(semweb/turtle), _{}).
component(library(sgml), _{}).
component(library(sha), _{}).
component(library(snowball), _{}).
component(library(socket), _{}).
component(library(ssl), _{}).
component(library(sweep_link), _{features:sweep_emacs_module}).
component(library(crypto), _{}).
component(library(syslog), _{os:unix}).
component(library(table), _{}).
component(library(time), _{}).
component(library(tipc/tipc), _{os:linux}).
component(library(unicode), _{}).
component(library(uri), _{}).
component(library(uuid), _{}).
component(library(yaml), _{}).
component(library(zlib), _{}).

issue_base('http://www.swi-prolog.org/build/issues/').

:- thread_local
    issue/1.

:- meta_predicate
    run_silent(0, +).

%!  check_installation is det.
%
%   Check features of the installed   system. Performs the following
%   tests:
%
%     1. Test whether features that depend on optional libraries
%        are present (e.g., unbounded arithmetic support)
%     2. Test that all standard libraries that depend on foreign
%        code are present.
%     3. provides a test_installation predicate to run the tests
%        at runtime if the system was built with -DINSTALL_TESTS
%
%   If issues are found it prints a   diagnostic message with a link
%   to a wiki page with additional information about the issue.

check_installation :-
    print_message(informational, installation(checking)),
    check_installation_(InstallIssues),
    check_on_path,
    check_config_files(ConfigIssues),
    check_autoload,
    maplist(print_message(warning), ConfigIssues),
    append(InstallIssues, ConfigIssues, Issues),
    (   Issues == []
    ->  print_message(informational, installation(perfect))
    ;   length(Issues, Count),
        print_message(warning, installation(imperfect(Count)))
    ).

%!  check_installation(-Issues:list(pair)) is det.
%
%   As check_installation/0, but additionally  returns   a  list  of
%   Component-Problem pairs. Problem is  one of `optional_not_found`
%   (optional component is not present),   `not_found` (component is
%   not present) or `failed` (component  is   present  but cannot be
%   loaded).

check_installation(Issues) :-
    check_installation_(Issues0),
    maplist(public_issue, Issues0, Issues).

public_issue(installation(Term), Source-Issue) :-
    functor(Term, Issue, _),
    arg(1, Term, Properties),
    Source = Properties.source.

check_installation_(Issues) :-
    retractall(issue(_)),
    forall(component(Source, _Properties),
           check_component(Source)),
    findall(I, retract(issue(I)), Issues).

check_component(Source) :-
    component(Source, Properties),
    !,
    check_component(Source, Properties.put(source,Source)).

check_component(_Source, Properties) :-
    OS = Properties.get(os),
    \+ current_os(OS),
    !.
check_component(Source, Properties) :-
    compound(Source),
    !,
    check_source(Source, Properties).
check_component(Feature, Properties) :-
    print_message(informational, installation(checking(Feature))),
    (   call(Properties.test)
    ->  print_message(informational, installation(ok))
    ;   print_issue(installation(missing(Properties)))
    ).

check_source(Source, Properties) :-
    exists_source(Source),
    !,
    print_message(informational, installation(loading(Source))),
    (   run_silent(( (   Pre = Properties.get(pre)
                     ->  call(Pre)
                     ;   true
                     ),
                     load_files(Source, [silent(true), if(true)])
                   ),
                   Properties.put(action, load))
    ->  test_component(Properties),
        print_message(informational, installation(ok)),
        check_features(Properties)
    ;   true
    ).
check_source(_Source, Properties) :-
    Properties.get(optional) == true,
    !,
    print_message(silent,
                  installation(optional_not_found(Properties))).
check_source(_Source, Properties) :-
    print_issue(installation(not_found(Properties))).

current_os(unix)    :- current_prolog_flag(unix, true).
current_os(windows) :- current_prolog_flag(windows, true).
current_os(linux)   :- current_prolog_flag(arch, Arch),
                       sub_atom(Arch, _, _, _, linux).

%!  test_component(+Properties) is semidet.
%
%   Run additional tests to see whether the component really works.

test_component(Dict) :-
    Test = Dict.get(test),
    !,
    call(Test).
test_component(_).

%!  check_features(+Properties) is semidet.
%
%   Check for additional features of the components.
%
%   @see check_component/1 should be used for checking that the
%   component works.

check_features(Dict) :-
    Test = Dict.get(features),
    !,
    catch(Test, Error,
          ( print_message(warning, Error),
            fail)).
check_features(_).


%!  run_silent(:Goal, +Properties) is semidet.
%
%   Succeed if Goal succeeds  and  does   not  print  any  errors or
%   warnings.

run_silent(Goal, Properties) :-
    run_collect_messages(Goal, Result, Messages),
    (   Result == true,
        Messages == []
    ->  true
    ;   print_issue(installation(failed(Properties, Result, Messages))),
        fail
    ).

%!  run_collect_messages(Goal, Result, Messages) is det.
%
%   Run Goal, unify Result with  =true=, =false= or exception(Error)
%   and  messages  with  a  list  of  generated  error  and  warning
%   messages. Each message is a term:
%
%       message(Term,Kind,Lines)
%
%   @see message_hook/3.

:- thread_local
    got_message/1.

run_collect_messages(Goal, Result, Messages) :-
    setup_call_cleanup(
        asserta((user:thread_message_hook(Term,Kind,Lines) :-
                    error_kind(Kind),
                    assertz(got_message(message(Term,Kind,Lines)))), Ref),
        (   catch(Goal, E, true)
        ->  (   var(E)
            ->  Result0 = true
            ;   Result0 = exception(E)
            )
        ;   Result0 = false
        ),
        erase(Ref)),
    findall(Msg, retract(got_message(Msg)), Messages),
    Result = Result0.

error_kind(warning).
error_kind(error).


                 /*******************************
                 *         SPECIAL TESTS        *
                 *******************************/

%!  test_tcmalloc

:- if(current_predicate(malloc_property/1)).
test_tcmalloc :-
    malloc_property('generic.current_allocated_bytes'(Bytes)),
    Bytes > 1 000 000.
:- else.
test_tcmalloc :-
    fail.
:- endif.

%!  archive_features
%
%   Report features supported by library(archive).

archive_features :-
    tmp_file_stream(utf8, Name, Out),
    close(Out),
    findall(F, archive_filter(F, Name), Filters),
    print_message(informational, installation(archive(filters, Filters))),
    findall(F, archive_format(F, Name), Formats),
    print_message(informational, installation(archive(formats, Formats))),
    delete_file(Name).

archive_filter(F, Name) :-
    a_filter(F),
    catch(archive_open(Name, A, [filter(F)]), E, true),
    (   var(E)
    ->  archive_close(A)
    ;   true
    ),
    \+ subsumes_term(error(domain_error(filter, _),_), E).

archive_format(F, Name) :-
    a_format(F),
    catch(archive_open(Name, A, [format(F)]), E, true),
    (   var(E)
    ->  archive_close(A)
    ;   true
    ),
    \+ subsumes_term(error(domain_error(format, _),_), E).

a_filter(bzip2).
a_filter(compress).
a_filter(gzip).
a_filter(grzip).
a_filter(lrzip).
a_filter(lzip).
a_filter(lzma).
a_filter(lzop).
a_filter(none).
a_filter(rpm).
a_filter(uu).
a_filter(xz).

a_format('7zip').
a_format(ar).
a_format(cab).
a_format(cpio).
a_format(empty).
a_format(gnutar).
a_format(iso9660).
a_format(lha).
a_format(mtree).
a_format(rar).
a_format(raw).
a_format(tar).
a_format(xar).
a_format(zip).

%!  pcre_features

pcre_features :-
    findall(X, pcre_missing(X), Missing),
    (   Missing == []
    ->  true
    ;   print_message(warning, installation(pcre_missing(Missing)))
    ),
    (   re_config(compiled_widths(Widths)),
        1 =:= Widths /\ 1
    ->  true
    ;   print_message(warning, installation(pcre_missing('8-bit support')))
    ).

pcre_missing(X) :-
    pcre_must_have(X),
    Term =.. [X,true],
    \+ catch(re_config(Term), _, fail).

pcre_must_have(unicode).

%!  jquery_file
%
%   Test whether jquery.js can be found

jquery_file :-
    setting(jquery:version, File),
    (   absolute_file_name(js(File), Path, [access(read), file_errors(fail)])
    ->  print_message(informational, installation(jquery(found(Path))))
    ;   print_message(warning, installation(jquery(not_found(File))))
    ).

sweep_emacs_module :-
    with_output_to(string(S), write_sweep_module_location),
    split_string(S, "\n", "\n", [VersionInfo|Modules]),
    must_be(oneof(["V 1"]), VersionInfo),
    (   maplist(check_sweep_lib, Modules)
    ->  print_message(informational, installation(sweep(found(Modules))))
    ;   print_message(warning, installation(sweep(not_found(Modules))))
    ).

check_sweep_lib(Line) :-
    sub_atom(Line, B, _, A, ' '),
    sub_atom(Line, 0, B, _, Type),
    must_be(oneof(['L', 'M']), Type),
    sub_atom(Line, _, A, 0, Lib),
    exists_file(Lib).

python_version :-
    py_call(sys:version, Version),
    print_message(informational, installation(janus(Version))).


%!  check_on_path
%
%   Validate that Prolog is installed in   $PATH.  Only performed if the
%   running executable is  a  normal   executable  file,  assuming  some
%   special installation such as the WASM version otherwise.

check_on_path :-
    current_prolog_flag(executable, EXEFlag),
    prolog_to_os_filename(EXE, EXEFlag),
    file_base_name(EXE, Prog),
    absolute_file_name(EXE, AbsExe,
                       [ access(execute),
                         file_errors(fail)
                       ]),
    !,
    prolog_to_os_filename(AbsExe, OsExe),
    (   absolute_file_name(path(Prog), OnPath,
                           [ access(execute),
                             file_errors(fail)
                           ])
    ->  (   same_file(EXE, OnPath)
        ->  true
        ;   absolute_file_name(path(Prog), OnPathAny,
                               [ access(execute),
                                 file_errors(fail),
                                 solutions(all)
                               ]),
            same_file(EXE, OnPathAny)
        ->  print_message(warning, installation(not_first_on_path(OsExe, OnPath)))
        ;   print_message(warning, installation(not_same_on_path(OsExe, OnPath)))
        )
    ;   print_message(warning, installation(not_on_path(OsExe, Prog)))
    ).
check_on_path.


		 /*******************************
		 *           RUN TESTS		*
		 *******************************/

%!  test_installation is semidet.
%!  test_installation(+Options) is semidet.
%
%   Run regression tests in the installed system. Requires the system to
%   be built using
%
%	cmake -DINSTALL_TESTS=ON
%
%   Options processed:
%
%     - packages(+Boolean)
%       When `false`, do not test the packages
%     - package(+Package)
%       Only test package package.

test_installation :-
    test_installation([]).

test_installation(Options) :-
    absolute_file_name(swi(test/test),
                       TestFile,
                       [ access(read),
                         file_errors(fail),
                         file_type(prolog)
                       ]),
    !,
    test_installation_run(TestFile, Options).
test_installation(_Options) :-
    print_message(warning, installation(testing(no_installed_tests))).

test_installation_run(TestFile, Options) :-
    (   option(package(_), Options)
    ->  merge_options(Options,
                      [ core(false),
                        subdirs(false)
                      ], TestOptions)
    ;   merge_options(Options,
                      [ packages(true)
                      ], TestOptions)
    ),
    load_files(user:TestFile),
    current_prolog_flag(verbose, Old),
    setup_call_cleanup(
        set_prolog_flag(verbose, silent),
        user:test([], TestOptions),
        set_prolog_flag(verbose, Old)).


                 /*******************************
                 *            MESSAGES          *
                 *******************************/

:- multifile
    prolog:message//1.

print_issue(Term) :-
    assertz(issue(Term)),
    print_message(warning, Term).

issue_url(Properties, URL) :-
    Local = Properties.get(url),
    !,
    issue_base(Base),
    atom_concat(Base, Local, URL).
issue_url(Properties, URL) :-
    Properties.get(source) = library(Segments),
    !,
    path_segments_atom(Segments, Base),
    file_name_extension(Base, html, URLFile),
    issue_base(Issues),
    atom_concat(Issues, URLFile, URL).

prolog:message(installation(Message)) -->
    message(Message).

message(checking) -->
    { current_prolog_flag(address_bits, Bits) },
    { current_prolog_flag(arch, Arch) },
    { current_prolog_flag(home, Home) },
    { current_prolog_flag(cpu_count, Cores) },
    [ 'Checking your SWI-Prolog kit for common issues ...'-[], nl, nl ],
    [ 'Version: ~`.t~24| '-[] ], '$messages':prolog_message(version), [nl],
    [ 'Address bits: ~`.t~24| ~d'-[Bits] ], [nl],
    [ 'Architecture: ~`.t~24| ~w'-[Arch] ], [nl],
    [ 'Installed at: ~`.t~24| ~w'-[Home] ], [nl],
    [ 'Cores: ~`.t~24| ~w'-[Cores] ], [nl],
    [ nl ].
message(perfect) -->
    [ nl, 'Congratulations, your kit seems sound and complete!'-[] ].
message(imperfect(N)) -->
    [ 'Found ~w issues.'-[N] ].
message(checking(Feature)) -->
    [ 'Checking ~w ...'-[Feature], flush ].
message(missing(Properties)) -->
    [ at_same_line, '~`.t~48| not present'-[] ],
    details(Properties).
message(loading(Source)) -->
    [ 'Loading ~q ...'-[Source], flush ].
message(ok) -->
    [ at_same_line, '~`.t~48| ok'-[] ].
message(optional_not_found(Properties)) -->
    [ 'Optional ~q ~`.t~48| not present'-[Properties.source] ].
message(not_found(Properties)) -->
    [ '~q ~`.t~48| NOT FOUND'-[Properties.source] ],
    details(Properties).
message(failed(Properties, false, [])) -->
    !,
    [ at_same_line, '~`.t~48| FAILED'-[] ],
    details(Properties).
message(failed(Properties, exception(Ex0), [])) -->
    !,
    { strip_stack(Ex0, Ex),
      message_to_string(Ex, Msg) },
    [ '~w'-[Msg] ],
    details(Properties).
message(failed(Properties, true, Messages)) -->
    [ at_same_line, '~`.t~48| FAILED'-[] ],
    explain(Messages),
    details(Properties).
message(archive(What, Names)) -->
    [ '  Supported ~w: '-[What] ],
    list_names(Names).
message(pcre_missing(Features)) -->
    [ 'Missing libpcre features: '-[] ],
    list_names(Features).
message(not_first_on_path(EXE, OnPath)) -->
    { public_executable(EXE, PublicEXE),
      file_base_name(EXE, Prog)
    },
    [ 'The first ~w on '-[Prog] ], 'PATH', [ ' is ~p, while '-[OnPath], nl ],
    [ 'this version is ~p.'-[PublicEXE] ].
message(not_same_on_path(EXE, OnPath)) -->
    { public_executable(EXE, PublicEXE),
      file_base_name(EXE, Prog)
    },
    [ 'The ~w on '-[Prog] ], 'PATH', [ ' is ~p, while '-[OnPath], nl ],
    [ 'this version is ~p.'-[PublicEXE] ].
message(not_on_path(EXE, Prog)) -->
    { public_bin_dir(EXE, Dir),
      prolog_to_os_filename(Dir, OSDir)
    },
    [ 'Could not find ~w on '-[Prog] ], 'PATH', [ '. '-[], nl ],
    [ 'You may wish to add ~p to '-[OSDir] ], 'PATH', [ '. '-[], nl ].
message(jquery(found(Path))) -->
    [ '  jQuery from ~w'-[Path] ].
message(jquery(not_found(File))) -->
    [ '  Cannot find jQuery (~w)'-[File] ].
message(sweep(found(Paths))) -->
    [ '  GNU-Emacs plugin loads'-[] ],
    sequence(list_file, Paths).
message(sweep(not_found(Paths))) -->
    [ '  Could not find all GNU-Emacs libraries'-[] ],
    sequence(list_file, Paths).
message(testing(no_installed_tests)) -->
    [ '  Runtime testing is not enabled.', nl],
    [ '  Please recompile the system with INSTALL_TESTS enabled.' ].
message(janus(Version)) -->
    [ '  Python version ~w'-[Version] ].
message(ambiguous_autoload(PI, Paths)) -->
    [ 'The predicate ~p can be autoloaded from multiple libraries:'-[PI]],
    sequence(list_file, Paths).

public_executable(EXE, PublicProg) :-
    file_base_name(EXE, Prog),
    file_directory_name(EXE, ArchDir),
    file_directory_name(ArchDir, BinDir),
    file_directory_name(BinDir, Home),
    file_directory_name(Home, Lib),
    file_directory_name(Lib, Prefix),
    atomic_list_concat([Prefix, bin, Prog], /, PublicProg),
    exists_file(PublicProg),
    same_file(EXE, PublicProg),
    !.
public_executable(EXE, EXE).

public_bin_dir(EXE, Dir) :-
    public_executable(EXE, PublicEXE),
    file_directory_name(PublicEXE, Dir).



'PATH' -->
    { current_prolog_flag(windows, true) },
    !,
    [ '%PATH%'-[] ].
'PATH' -->
    [ '$PATH'-[] ].

strip_stack(error(Error, context(prolog_stack(S), Msg)),
            error(Error, context(_, Msg))) :-
    nonvar(S).
strip_stack(Error, Error).

details(Properties) -->
    { issue_url(Properties, URL), !
    },
    [ nl, 'See '-[], url(URL) ].
details(_) --> [].

explain(Messages) -->
    { shared_object_error(Messages) },
    !,
    [nl],
    (   { current_prolog_flag(windows, true) }
    ->  [ 'Cannot load required DLL'-[] ]
    ;   [ 'Cannot load required shared library'-[] ]
    ).
explain(Messages) -->
    print_messages(Messages).

shared_object_error(Messages) :-
    sub_term(Term, Messages),
    subsumes_term(error(shared_object(open, _Message), _), Term),
    !.

print_messages([]) --> [].
print_messages([message(_Term, _Kind, Lines)|T]) -->
    Lines, [nl],
    print_messages(T).

list_names([]) --> [].
list_names([H|T]) -->
    [ '~w'-[H] ],
    (   {T==[]}
    ->  []
    ;   [ ', '-[] ],
        list_names(T)
    ).

list_file(File) -->
    [ nl, '    '-[], url(File) ].


		 /*******************************
		 *          CONFIG FILES	*
		 *******************************/

%!  check_config_files
%
%   Examines the locations of config files.  The config files have moved
%   in version 8.1.15

check_config_files :-
    check_config_files(Issues),
    maplist(print_message(warning), Issues).

check_config_files(Issues) :-
    findall(Issue, check_config_file(Issue), Issues).

check_config_file(config(Id, move(Type, OldFile, NewFile))) :-
    old_config(Type, Id, OldFile),
    access_file(OldFile, exist),
    \+ ( new_config(Type, Id, NewFile),
         access_file(NewFile, exist)
       ),
    once(new_config(Type, Id, NewFile)).
check_config_file(config(Id, different(Type, OldFile, NewFile))) :-
    old_config(Type, Id, OldFile),
    access_file(OldFile, exist),
    new_config(Type, Id, NewFile),
    access_file(NewFile, exist),
    \+ same_file(OldFile, NewFile).

%!  update_config_files
%
%   Move config files from their old location to  the new if the file or
%   directory exists in the old location but not in the new.

update_config_files :-
    old_config(Type, Id, OldFile),
    access_file(OldFile, exist),
    \+ ( new_config(Type, Id, NewFile),
         access_file(NewFile, exist)
       ),
    (   new_config(Type, Id, NewFile),
        \+ same_file(OldFile, NewFile),
        create_parent_dir(NewFile)
    ->  catch(rename_file(OldFile, NewFile), E,
              print_message(warning, E)),
        print_message(informational, config(Id, moved(Type, OldFile, NewFile)))
    ),
    fail.
update_config_files.

old_config(file, init, File) :-
    current_prolog_flag(windows, true),
    win_folder(appdata, Base),
    atom_concat(Base, '/SWI-Prolog/swipl.ini', File).
old_config(file, init, File) :-
    expand_file_name('~/.swiplrc', [File]).
old_config(directory, lib, Dir) :-
    expand_file_name('~/lib/prolog', [Dir]).
old_config(directory, xpce, Dir) :-
    expand_file_name('~/.xpce', [Dir]).
old_config(directory, history, Dir) :-
    expand_file_name('~/.swipl-dir-history', [Dir]).
old_config(directory, pack, Dir) :-
    (   catch(expand_file_name('~/lib/swipl/pack', [Dir]), _, fail)
    ;   absolute_file_name(swi(pack), Dir,
                           [ file_type(directory), solutions(all) ])
    ).

new_config(file, init, File) :-
    absolute_file_name(user_app_config('init.pl'), File,
                       [ solutions(all) ]).
new_config(directory, lib, Dir) :-
    config_dir(user_app_config(lib), Dir).
new_config(directory, xpce, Dir) :-
    config_dir(user_app_config(xpce), Dir).
new_config(directory, history, Dir) :-
    config_dir(user_app_config('dir-history'), Dir).
new_config(directory, pack, Dir) :-
    config_dir([app_data(pack), swi(pack)], Dir).

config_dir(Aliases, Dir) :-
    is_list(Aliases),
    !,
    (   member(Alias, Aliases),
        absolute_file_name(Alias, Dir,
                           [ file_type(directory), solutions(all) ])
    *-> true
    ;   member(Alias, Aliases),
        absolute_file_name(Alias, Dir,
                           [ solutions(all) ])
    ).
config_dir(Alias, Dir) :-
    (   absolute_file_name(Alias, Dir,
                           [ file_type(directory), solutions(all) ])
    *-> true
    ;   absolute_file_name(Alias, Dir,
                           [ solutions(all) ])
    ).

create_parent_dir(NewFile) :-
    file_directory_name(NewFile, Dir),
    create_parent_dir_(Dir).

create_parent_dir_(Dir) :-
    exists_directory(Dir),
    '$my_file'(Dir),
    !.
create_parent_dir_(Dir) :-
    file_directory_name(Dir, Parent),
    Parent \== Dir,
    create_parent_dir_(Parent),
    make_directory(Dir).

prolog:message(config(Id, Issue)) -->
    [ 'Config: '-[] ],
    config_description(Id),
    config_issue(Issue).

config_description(init) -->
    [ '(user initialization file) '-[], nl ].
config_description(lib) -->
    [ '(user library) '-[], nl ].
config_description(pack) -->
    [ '(add-ons) '-[], nl ].
config_description(history) -->
    [ '(command line history) '-[], nl ].
config_description(xpce) -->
    [ '(gui) '-[], nl ].

config_issue(move(Type, Old, New)) -->
    [ '  found ~w "~w"'-[Type, Old], nl ],
    [ '  new location is "~w"'-[New] ].
config_issue(moved(Type, Old, New)) -->
    [ '  found ~w "~w"'-[Type, Old], nl ],
    [ '  moved to new location "~w"'-[New] ].
config_issue(different(Type, Old, New)) -->
    [ '  found different ~w "~w"'-[Type, Old], nl ],
    [ '  new location is "~w"'-[New] ].

		 /*******************************
		 *         AUTO LOADING		*
		 *******************************/

%!  check_autoload
%
%   Find possible ambiguous predicates in the autoload index.

check_autoload :-
    findall(Name/Arity, '$in_library'(Name, Arity, _Path), PIs),
    msort(PIs, Sorted),
    clumped(Sorted, Clumped),
    sort(2, >=, Clumped, ClumpedS),
    ambiguous_autoload(ClumpedS).

ambiguous_autoload([PI-N|T]) :-
    N > 1,
    !,
    warn_ambiguous_autoload(PI),
    ambiguous_autoload(T).
ambiguous_autoload(_).

warn_ambiguous_autoload(PI) :-
    PI = Name/Arity,
    findall(PlFile,
            ( '$in_library'(Name, Arity, File),
              file_name_extension(File, pl, PlFile)
            ), PlFiles),
    print_message(warning, installation(ambiguous_autoload(PI, PlFiles))).
