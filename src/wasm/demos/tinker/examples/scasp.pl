% This demo downloads  s(CASP) from Github and shows  a little s(CASP)
% program.  Note that this is quite a lot of code and may take several
% seconds to process.  After loading you  can run e.g. (not the second
% `?`, which calls scasp/2.
%
%    ?- ? opera(Day).

:- use_module('https://raw.githubusercontent.com/SWI-Prolog/sCASP/master/prolog/scasp.pl').

opera(D) :- not home(D).
home(D):- not opera(D).
home(monday).

false :- baby(D), opera(D).

baby(tuesday).

