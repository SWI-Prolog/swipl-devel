:- use_module(library(apply)).
:- use_module(library(dcg/basics)).

not_covered(Regex, Macros) :-
    words(Regex, Used),
    defined(Regex, Defined),
    exclude(done(Defined), Used, Macros).

not_covered(Regex) :-
    not_covered(Regex, Macros),
    forall(member(W-F, Macros), format('~w ~t~w~30|~n', [W,F])).

not_covered_config(Regex) :-
    not_covered(Regex, Pairs),
    pairs_keys(Pairs, Macros),
    partition(is_header_macro, Macros, HMacros0, FMacros0),
    sort(HMacros0, HMacros),
    maplist(config_header, HMacros),
    nl,
    sort(FMacros0, FMacros),
    maplist(config_function, FMacros).

is_header_macro(M) :-
    sub_atom(M, _, _, 0, '_H').

config_header(M) :-
    atom_concat('HAVE_', HeaderR, M),
    atom_concat(HeaderU, '_H', HeaderR),
    downcase_atom(HeaderU, Header0),
    (   atom_concat(sys_, SubDir, Header0)
    ->  atom_concat('sys/', SubDir, Header)
    ;   Header = Header0
    ),
    format('check_include_file(~w.h ~w)~n', [Header, M]).

config_function(M) :-
    atom_concat('HAVE_', FunctionU, M),
    downcase_atom(FunctionU, Function),
    format('check_function_exists(~w ~w)~n', [Function, M]).


done(Defined, Used-_Count) :-
    memberchk(Used, Defined).

words(Regex, Pairs) :-
    src_words(AllWords),
    length(AllWords, Count),
    debug(stats, 'Got ~D words~n', [Count]),
    include(re_match(Regex), AllWords, Words),
    words_freq(Words, Pairs).

src_words(AllWords) :-
    directory_files('.', '*.[ch]', Files),
    maplist(file_words, Files, NestedWords),
    append(NestedWords, AllWords).

defined(Regex, Macros) :-
    directory_files(cmake, '*.cmake', CMakeFiles),
    maplist(file_words, ['CMakeLists.txt'|CMakeFiles], AllWordsNested),
    append(AllWordsNested, AllWords),
    sort(AllWords, Sorted),
    include(re_match(Regex), Sorted, Macros).

%!  words_freq(+Words, -FreqPairs)
%
%   Create from a word list an ordered set of Word-Count

words_freq(List, Pairs) :-
    msort(List, Sorted),
    count_words(Sorted, Pairs0),
    sort(2, >=, Pairs0, Pairs).

count_words([], []).
count_words([H|T0], [H-C|T]) :-
    count_same(T0, H, 1, C, T1),
    count_words(T1, T).

count_same([H|T0], H, C0, C, T) :-
    !,
    C1 is C0+1,
    count_same(T0, H, C1, C, T).
count_same(T, _, C, C, T).

%!  directory_files(+Dir, +Pattern, -Files)
%
%   Get all files in Dir and its subdirs that match Pattern.

directory_files(Dir, Pattern, Files) :-
    phrase(directory_files(Pattern, Dir), Files).

directory_files(Pattern, Dir) -->
    { directory_file_path(Dir, Pattern, DirPattern),
      expand_file_name(DirPattern, Files0),
      include(exists_file, Files0, Files),
      directory_files(Dir, All),
      convlist(subdir(Dir), All, SubDirs)
    },
    string(Files),
    foldl(directory_files(Pattern), SubDirs).

subdir(Dir, Entry, SubDir) :-
    \+ sub_atom(Entry, 0, _, _, '.'),
    directory_file_path(Dir, Entry, SubDir),
    exists_directory(SubDir).

%!  file_words(+File, -Words)
%
%   Get all identifiers from File.

file_words(File, Words) :-
    read_file_to_string(File, String, []),
    re_split('[a-zA-Z_]+', String, Tokens),
    include(word, Tokens, WordStrings),
    maplist(atom_string, Words, WordStrings).

word(Atom) :-
    sub_atom(Atom, 0, 1, _, First),
    char_type(First, alnum).
