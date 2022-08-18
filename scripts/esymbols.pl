:- use_module(library(process)).
:- use_module(library(apply)).

:- meta_predicate
    code_symbols(+, 1, +).

error_code_symbols :-
    format('#ifdef __WINDOWS__~n~n'),
    wsa_errno_symbols,
    format('~n#else /*__WINDOWS__*/~n~n'),
    errno_symbols,
    nl,
    gai_esymbols,
    nl,
    h_errno_symbols,
    format('~n#endif /*__WINDOWS__*/~n~n').

errno_symbols :-
    setup_call_cleanup(
        process_create(
            path(errno), ['-l'],
            [ stdout(pipe(Out))
            ]),
        read_string(Out, _, String),
        close(Out)),
    split_string(String, "\n", "\n", Lines),
    code_symbols(errno_symbols, errno_line, Lines).

errno_line(Line) :-
    split_string(Line, " \t", " \t", [Macro|_]),
    sub_string(Macro, 0, _, _, "E"),
    esymbol(Macro).

gai_esymbols :-
    setup_call_cleanup(
        process_create(
            path(man), [gai_strerror],
            [ stdout(pipe(Out))
            ]),
        read_string(Out, _, String),
        close(Out)),
    split_string(String, "\n", "\n", Lines),
    code_symbols(gai_errno_symbols, gai_errno_line, Lines).

gai_errno_line(Line) :-
    split_string(Line, " \t", " \t", [Macro]),
    sub_string(Macro, 0, _, _, "EAI_"),
    !,
    esymbol(Macro).
gai_errno_line(_).

h_errno_symbols :-
    code_symbols(h_errno_symbols, esymbol,
                 [ 'HOST_NOT_FOUND',
                   'NO_DATA',
                   'NO_RECOVERY',
                   'TRY_AGAIN'
                 ]).

wsa_errno_symbols :-
    code_symbols(wsa_errno_symbols, esymbol,
                 [ 'WSAEACCES',
                   'WSAEADDRINUSE',
                   'WSAEADDRNOTAVAIL',
                   'WSAEAFNOSUPPORT',
                   'WSAEALREADY',
                   'WSAECONNABORTED',
                   'WSAECONNREFUSED',
                   'WSAECONNRESET',
                   'WSAEDESTADDRREQ',
                   'WSAEFAULT',
                   'WSAEHOSTDOWN',
                   'WSAEHOSTUNREACH',
                   'WSAEINPROGRESS',
                   'WSAEINTR',
                   'WSAEINVAL',
                   'WSAEISCONN',
                   'WSAEMFILE',
                   'WSAEMSGSIZE',
                   'WSAENETDOWN',
                   'WSAENETRESET',
                   'WSAENETUNREACH',
                   'WSAENOBUFS',
                   'WSAENOPROTOOPT',
                   'WSAENOTCONN',
                   'WSAENOTSOCK',
                   'WSAEOPNOTSUPP',
                   'WSAEPFNOSUPPORT',
                   'WSAEPROCLIM',
                   'WSAEPROTONOSUPPORT',
                   'WSAEPROTOTYPE',
                   'WSAESHUTDOWN',
                   'WSAESOCKTNOSUPPORT',
                   'WSAETIMEDOUT',
                   'WSAEWOULDBLOCK',
                   'WSAEDISCON',
                   'WSANOTINITIALISED',
                   'WSAHOST_NOT_FOUND',
                   'WSANO_DATA'
                 ]).

code_symbols(Var, One, List) :-
    format('static const error_symbol_t ~w[] =~n', [Var]),
    format('{~n'),
    maplist(One, List),
    format('  { 0, NULL }~n'),
    format('};~n').

esymbol(S) :-
    string_lower(S, Lwr),
    format('#ifdef ~w~n', [S]),
    format('  { ~w, "~w" },~n', [S,Lwr]),
    format('#endif~n').
