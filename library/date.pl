/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Willem Robert van Hage
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2014, University of Amsterdam
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

:- module(date,
          [ date_time_value/3,          % ?Field, ?DaTime, ?Value
            parse_time/2,               % +Date, -Stamp
            parse_time/3,               % +Date, ?Format, -Stamp
            day_of_the_week/2,          % +Date, -DayOfTheWeek
            day_of_the_year/2           % +Date, -DayOfTheYear
          ]).

/** <module> Process dates and times
*/

%!  date_time_value(?Field:atom, +Struct:datime, -Value) is nondet.
%
%   Extract values from a date-time structure.  Provided fields are
%
%           | year | integer | |
%           | month | 1..12 | |
%           | day | 1..31 | |
%           | hour | 0..23 | |
%           | minute | 0..59 | |
%           | second | 0.0..60.0 | |
%           | utc_offset | integer | Offset to UTC in seconds (positive is west) |
%           | daylight_saving | bool | Name of timezone; fails if unknown |
%           | date | date(Y,M,D) | |
%           | time | time(H,M,S) | |

date_time_value(year,            date(Y,_,_,_,_,_,_,_,_), Y).
date_time_value(month,           date(_,M,_,_,_,_,_,_,_), M).
date_time_value(day,             date(_,_,D,_,_,_,_,_,_), D).
date_time_value(hour,            date(_,_,_,H,_,_,_,_,_), H).
date_time_value(minute,          date(_,_,_,_,M,_,_,_,_), M).
date_time_value(second,          date(_,_,_,_,_,S,_,_,_), S).
date_time_value(utc_offset,      date(_,_,_,_,_,_,O,_,_), O).
date_time_value(time_zone,       date(_,_,_,_,_,_,_,Z,_), Z) :- Z \== (-).
date_time_value(daylight_saving, date(_,_,_,_,_,_,_,_,D), D) :- D \== (-).

date_time_value(date,            date(Y,M,D,_,_,_,_,_,_), date(Y,M,D)).
date_time_value(time,            date(_,_,_,H,M,S,_,_,_), time(H,M,S)).

%!  parse_time(+Text, -Stamp) is semidet.
%!  parse_time(+Text, ?Format, -Stamp) is semidet.
%
%   Stamp is a  timestamp  created  from   parsing  Text  using  the
%   representation Format. Currently supported formats are:
%
%       * rfc_1123
%       Used for the HTTP protocol to represent time-stamps
%       * iso_8601
%       Commonly used in XML documents.

parse_time(Text, Stamp) :-
    parse_time(Text, _Format, Stamp).

parse_time(Text, Format, Stamp) :-
    atom_codes(Text, Codes),
    phrase(date(Format, Y,Mon,D,H,Min,S,UTCOffset), Codes),
    !,
    date_time_stamp(date(Y,Mon,D,H,Min,S,UTCOffset,-,-), Stamp).

date(iso_8601, Yr, Mon, D, H, Min, S, 0) --> % BC
    "-", date(iso_8601, Y, Mon, D, H, Min, S, 0),
    { Yr is -1 * Y }.
date(iso_8601, Y, Mon, D, H, Min, S, 0) -->
    year(Y),
    iso_8601_rest(Y, Mon, D, H, Min, S).
date(rfc_1123, Y, Mon, D, Hr, Min, Sec, 0) --> % RFC 1123: "Fri, 08 Dec 2006 15:29:44 GMT"
    day_name(_), ", ", ws,
    day_of_the_month(D), ws,
    month_name(Mon), ws,
    year(Y), ws,
    hour(H), ":", minute(M), ":", second(S), ws,
    timezone(DH, DM, DS),
    { Hr is H + DH, Min is M + DM, Sec is S + DS }.


%!  iso_8601_rest(+Year:int, -Mon, -Day, -H, -M, -S)
%
%   Process ISO 8601 time-values after parsing the 4-digit year.

iso_8601_rest(_, Mon, D, H, Min, S) -->
    "-", month(Mon), "-", day(D),
    opt_time(H, Min, S).
iso_8601_rest(_, Mon, 0, 0, 0, 0) -->
    "-", month(Mon).
iso_8601_rest(_, Mon, D, H, Min, S) -->
    month(Mon), day(D),
    opt_time(H, Min, S).
iso_8601_rest(_, 1, D, H, Min, S) -->
    "-", ordinal(D),
    opt_time(H, Min, S).
iso_8601_rest(Yr, 1, D, H, Min, S) -->
    "-W", week(W), "-", day_of_the_week(DW),
    opt_time(H, Min, S),
    { week_ordinal(Yr, W, DW, D) }.
iso_8601_rest(Yr, 1, D, H, Min, S) -->
    "W", week(W), day_of_the_week(DW),
    opt_time(H, Min, S),
    { week_ordinal(Yr, W, DW, D) }.
iso_8601_rest(Yr, 1, D, 0, 0, 0) -->
    "W", week(W),
    { week_ordinal(Yr, W, 1, D) }.

opt_time(Hr, Min, Sec) -->
    ("T";" "), !, iso_time(Hr, Min, Sec).
opt_time(0, 0, 0) --> "".


% TIMEX2 ISO: "2006-12-08T15:29:44 UTC" or "20061208T"
iso_time(Hr, Min, Sec) -->
    hour(H), ":", minute(M), ":", second(S),
    timezone(DH, DM, DS),
    { Hr is H + DH, Min is M + DM, Sec is S + DS }.
iso_time(Hr, Min, Sec) -->
    hour(H), ":", minute(M),
    timezone(DH, DM, DS),
    { Hr is H + DH, Min is M + DM, Sec is DS }.
iso_time(Hr, Min, Sec) -->
    hour(H), minute(M), second(S),
    timezone(DH, DM, DS),
    { Hr is H + DH, Min is M + DM, Sec is S + DS }.
iso_time(Hr, Min, Sec) -->
    hour(H), minute(M),
    timezone(DH, DM, DS),
    { Hr is H + DH, Min is M + DM, Sec is DS }.
iso_time(Hr, Min, Sec) -->
    hour(H),
    timezone(DH, DM, DS),
    { Hr is H + DH, Min is DM, Sec is DS }.

% FIXME: deal with leap seconds
timezone(Hr, Min, 0) -->
    "+", hour(H), ":", minute(M), { Hr is -1 * H, Min is -1 * M }.
timezone(Hr, Min, 0) -->
    "+", hour(H), minute(M), { Hr is -1 * H, Min is -1 * M }.
timezone(Hr, 0, 0) -->
    "+", hour(H), { Hr is -1 * H }.
timezone(Hr, Min, 0) -->
    "-", hour(H), ":", minute(M), { Hr is H, Min is M }.
timezone(Hr, Min, 0) -->
    "-", hour(H), minute(M), { Hr is H, Min is M }.
timezone(Hr, 0, 0) -->
    "-", hour(H), { Hr is H }.
timezone(0, 0, 0) -->
    "Z".
timezone(0, 0, 0) -->
    ws, "UTC".
timezone(0, 0, 0) -->
    ws, "GMT". % remove this?
timezone(0, 0, 0) -->
    [].

day_name(0) --> "Sun".
day_name(1) --> "Mon".
day_name(2) --> "Tue".
day_name(3) --> "Wed".
day_name(4) --> "Thu".
day_name(5) --> "Fri".
day_name(6) --> "Sat".
day_name(7) --> "Sun".

month_name(1) --> "Jan".
month_name(2) --> "Feb".
month_name(3) --> "Mar".
month_name(4) --> "Apr".
month_name(5) --> "May".
month_name(6) --> "Jun".
month_name(7) --> "Jul".
month_name(8) --> "Aug".
month_name(9) --> "Sep".
month_name(10) --> "Oct".
month_name(11) --> "Nov".
month_name(12) --> "Dec".

day_of_the_month(N) --> int2digit(N), { between(1, 31, N) }.
day_of_the_week(N)  --> digit(N),     { between(1,  7, N) }.
month(M)            --> int2digit(M), { between(1, 12, M) }.
week(W)             --> int2digit(W), { between(1, 53, W) }.
day(D)              --> int2digit(D), { between(1, 31, D) }.
hour(N)             --> int2digit(N), { between(0, 23, N) }.
minute(N)           --> int2digit(N), { between(0, 59, N) }.
second(S)           --> int2digit(N), { between(0, 60, N) }, % leap second
    opt_fraction(N, S).

opt_fraction(I, F) -->
    ( "." ; "," ),
    !,
    digits(D),
    { length(D, N),
      N > 0,
      number_codes(FP, D),
      F is I + FP/(10^N)
    }.
opt_fraction(I, I) -->
    [].

int2digit(N) -->
    digit(D0),
    digit(D1),
    { N is D0*10+D1 }.

year(Y) -->
    digit(D0),
    digit(D1),
    digit(D2),
    digit(D3),
    { Y is D0*1000+D1*100+D2*10+D3 }.

ordinal(N) --> % Nth day of the year, jan 1 = 1, dec 31 = 365 or 366
    digit(D0),
    digit(D1),
    digit(D2),
    { N is D0*100+D1*10+D2, between(1, 366, N) }.

digit(D) -->
    [C],
    { code_type(C, digit(D)) }.

digits([C|T]) -->
    [C],
    { code_type(C, digit) },
    !,
    digits(T).
digits([]) --> [].

ws -->
    " ",
    !,
    ws.
ws -->
    [].

%!  day_of_the_week(+Date, -DayOfTheWeek) is det.
%
%   Computes the day of the week for a  given date. Days of the week
%   are numbered from one to seven: monday   =  1, tuesday = 2, ...,
%   sunday = 7.
%
%   @param Date is a term of the form date(+Year, +Month, +Day)

day_of_the_week(date(Year, Mon, Day), DotW) :-
    format_time(atom(A), '%u', date(Year, Mon, Day, 0, 0, 0, 0, -, -)),
    atom_number(A, DotW).

week_ordinal(Year, Week, Day, Ordinal) :-
    format_time(atom(A), '%w', date(Year, 1, 1, 0, 0, 0, 0, -, -)),
    atom_number(A, DotW0),
    Ordinal is ((Week-1) * 7) - DotW0 + Day + 1.

%!  day_of_the_year(+Date, -DayOfTheYear) is det.
%
%   Computes the day of the year for a  given date. Days of the year
%   are numbered from 1 to 365 (366 for a leap year).
%
%   @param Date is a term of the form date(+Year, +Month, +Day)

day_of_the_year(date(Year, Mon, Day), DotY) :-
    format_time(atom(A), '%j', date(Year, Mon, Day, 0, 0, 0, 0, -, -)),
    atom_number(A, DotY).
