/*  Workflow experiment: solve several cyc150 cases against one KB,
    keeping the tables between them.  Based on ErgoAI's
    scripts/ergoaip_cases.pl.  See ../ergoai-cyc150.md.

    cd ~/src/prolog/ErgoAI
    swipl --stack-limit=16g -g run -t halt <this file>
*/
:- use_module(library(filesex)).
:- use_module(library(readutil)).
:- use_module(library(lists)).

root('/home/jan/src/prolog/ErgoAI').

setup :-
    root(Root),
    format(atom(P), '~w/ErgoEngine/ErgoAI/pkgs:~w/ErgoEngine/ErgoAI/ergo_lib', [Root, Root]),
    setenv('ERGOAIP_LIB_PATH', P),
    set_prolog_flag(table_space, 40 000 000 000),
    atom_concat(Root, '/ergoAIstudio2/ergoAIp/src/ergoaip_compiler', Compiler),
    use_module(Compiler),
    lexer_bridge:ensure_lexer.

cyc_dir(Dir) :- root(R), atom_concat(R, '/ErgoEngine/ErgoAI-performance/cyc150', Dir).

header_split(S, Header, Rest) :-
    Marker = "rmt-removal-lookup-pos-1'].",
    sub_string(S, B, L, _, Marker), !,
    E is B+L,
    sub_string(S, 0, E, _, Header0),
    string_concat(Header0, "\n", Header),
    sub_string(S, E, _, 0, Rest).

write_file(F, S) :- setup_call_cleanup(open(F, write, Out), write(Out, S), close(Out)).

load_base(First) :-
    cyc_dir(Dir), working_directory(_, Dir),
    directory_file_path(Dir, First, F),
    read_file_to_string(F, S, []), header_split(S, H, _),
    directory_file_path(Dir, '.wf_base.flr', Base),
    write_file(Base, H),
    t(load_base, ergoaip_compiler:load_ergo(Base, kb, _)),
    delete_file(Base).

case_file(Name, CaseFile) :-
    cyc_dir(Dir), directory_file_path(Dir, Name, F),
    read_file_to_string(F, S, []), header_split(S, _, Rest),
    string_concat(":- iriprefix{silk = \"http://vulcan.com/2008/silk#\"}.\n", Rest, Case),
    tmp_file(wfcase, Tmp), file_name_extension(Tmp, flr, CaseFile),
    write_file(CaseFile, Case).

add_case(Name) :-
    case_file(Name, CF),
    format(atom(L), 'add ~w', [Name]),
    t(L, ergoaip_compiler:load_spec(kb, +CF)).

solve(Label) :-
    t(Label, with_output_to(string(Out), (catch(once(kb:pretest), E, true) -> true ; true))),
    ( nonvar(E) -> print_message(error, E) ; true ),
    split_string(Out, "\n", " ", Ls), include(success_line, Ls, Ss),
    length(Ss, NS), format(user_error, "   ~d SUCCESS line(s)~n", [NS]).

success_line(Line) :-
    sub_string(Line, 0, _, _, "SUCCESS").

t(Label, G) :-
    garbage_collect,
    statistics(cputime, T0), statistics(inferences, I0),
    ( call(G) -> R = true ; R = false ),
    statistics(cputime, T1), statistics(inferences, I1),
    statistics(table_space_used, TS),
    T is T1-T0, I is I1-I0,
    format(user_error, "~w: ~w ~2f s ~D inf, table space ~D~n", [Label, R, T, I, TS]).

run :-
    setup,
    A = 'kbq-cpttest-diploidnumberfordogs-2.flr',
    B = 'kbq-cpttest-whatkindofthingisahaploidcell-4.flr',
    C = 'kbq-cpttest-whatphasesalternateintheeukaryoticcellcycle-2.flr',
    load_base(A),
    add_case(A), solve('solve A (cold)'),
    solve('solve A (again)'),
    add_case(B), solve('solve B (after A, tables kept)'),
    add_case(C), solve('solve C (after A,B, tables kept)'),
    t(abolish, abolish_all_tables),
    solve('solve C (cold, same KB)').
