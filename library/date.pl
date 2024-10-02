/*  Part of SWI-Prolog

    Author:        Jan Wielemaker and Willem Robert van Hage
    E-mail:        wielemak@science.uva.nl
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2006-2024, University of Amsterdam
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
%       Preferred for the HTTP protocol to represent time-stamps, e.g.
%
%           Fri, 08 Dec 2006 15:29:44 GMT
%
%       All components except for the time zone are obligatory.
%       If the time zone is omitted, the time is interpreted as
%       _local time_.
%
%       * rfc_1036
%       (Outdated) alternative for HTTP Protocol, e.g.
%
%           Sunday, 06-Nov-94 08:49:37 GMT
%
%       * iso_8601
%       Commonly used in XML documents. Actually the XML RFC3339
%       is a _profile_ of ISO8601.  For example
%
%           2006-12-08T15:29:44Z
%
%       The ISO8601 format allows removing components from the
%       right, returning the lowest time stamp in the specified
%       internal. If a time is specified but no time zone, the
%	time stamp is computed for the _local time_.  If only
%	the date components are specified, the stamp uses UTC.
%	To compute the start of a day in local time, use
%	e.g. ``2006-12-08T00``.
%
%       * asctime
%       ANSI C's asctime() format, e.g.
%
%           Sun Nov  6 08:49:37 1994
%
%       This format has no time zone and is interpreted as local
%       time.
%
%   @arg Text is an atom, string or list of character _codes_.
%   @see  xsd_time_string/3  from  library(sgml)    implements   RFC3339
%   strictly.

parse_time(Text, Stamp) :-
    parse_time(Text, _Format, Stamp).

parse_time(Text, Format, Stamp) :-
    to_codes(Text, Codes),
    phrase(date(Format, Y,Mon,D,H,Min,S,UTCOffset), Codes),
    !,
    tz_dst(time(H,Min,S), UTCOffset, TZ, DST),
    date_time_stamp(date(Y,Mon,D,H,Min,S,UTCOffset,TZ,DST), Stamp).

to_codes(In, Codes) :-
    (   is_list(In)
    ->  Codes = In
    ;   atom_codes(In, Codes)
    ).

tz_dst(_Time, UTCOffset, TZ, DST), nonvar(UTCOffset) =>
    TZ = (-), DST = (-).
tz_dst(time(H,M,S), UTCOffset, TZ, DST), var(H), var(M), var(S) =>
    UTCOffset = 0, TZ = (-), DST = (-).
tz_dst(_, _, _, _) =>
    true.

date(iso_8601, Yr, Mon, D, H, Min, S, UTCOffset) --> % BC
    "-", date(iso_8601, Y, Mon, D, H, Min, S, UTCOffset),
    { Yr is -1 * Y }.
date(iso_8601, Y, Mon, D, H, Min, S, UTCOffset) -->
    year(Y),
    iso_8601_rest(Y, Mon, D, H, Min, S, UTCOffset).
date(rfc_1123, Y, Mon, D, H, M, S, UTCOffset) -->
    day_name(_), ", ", ws, % RFC 1123: "Fri, 08 Dec 2006 15:29:44 GMT"
    day_of_the_month(D), ws,
    month_name(Mon), ws,
    year(Y), ws,
    iso_time3(H, M, S), ws,
    timezone(UTCOffset).
date(rfc_1036, Y, Mon, D, H, M, S, UTCOffset) -->
    full_day_name(_), ", ", ws, % RFC 1036: "Friday, 08-Dec-2006 15:29:44 GMT"
    day_of_the_month(D), "-",
    month_name(Mon), "-",
    year2d(Y), ws,
    iso_time3(H, M, S), ws,
    timezone(UTCOffset).
date(asctime, Y, Mon, D, H, M, S, _UTCOffset) -->
    day_name(_), " ",
    month_name(Mon), " ",
    asctime_day_of_the_month(D), " ",
    iso_time3(H, M, S), " ",
    year(Y).

%!  iso_8601_rest(+Year:int, -Mon, -Day, -H, -M, -S, -UTCOffset)
%
%   Process ISO 8601 time-values after parsing the 4-digit year.

iso_8601_rest(_, Mon, D, H, Min, S, UTCOffset) -->
    "-", month(Mon), "-", day(D),
    opt_time(H, Min, S, UTCOffset).
iso_8601_rest(_, Mon, _, _, _, _, _) -->
    "-", month(Mon).
iso_8601_rest(_, Mon, D, H, Min, S, UTCOffset) -->
    month(Mon), day(D),
    opt_time(H, Min, S, UTCOffset).
iso_8601_rest(_, 1, D, H, Min, S, UTCOffset) -->
    "-", ordinal(D),
    opt_time(H, Min, S, UTCOffset).
iso_8601_rest(Yr, 1, D, H, Min, S, UTCOffset) -->
    "-W", week(W), "-", day_of_the_week(DW),
    opt_time(H, Min, S, UTCOffset),
    { week_ordinal(Yr, W, DW, D) }.
iso_8601_rest(Yr, 1, D, H, Min, S, UTCOffset) -->
    "W", week(W), day_of_the_week(DW),
    opt_time(H, Min, S, UTCOffset),
    { week_ordinal(Yr, W, DW, D) }.
iso_8601_rest(Yr, 1, D, _, _, _, _) -->
    "W", week(W),
    { week_ordinal(Yr, W, 1, D) }.

opt_time(Hr, Min, Sec, UTCOffset) -->
    ("T";" "), !, iso_time(Hr, Min, Sec), timezone(UTCOffset).
opt_time(_H, _M, _S, _UTCOffset) --> "".


% TIMEX2 ISO: "2006-12-08T15:29:44 UTC" or "20061208T"
iso_time(H, M, S) -->
    iso_time3(H, M, S).
iso_time(H, M, _) -->
    hour(H), ":", minute(M).
iso_time(H, M, S) -->
    hour(H), minute(M), second(S).
iso_time(H, M, _) -->
    hour(H), minute(M).
iso_time(H, _, _) -->
    hour(H).

iso_time3(H, M, S) -->
    hour(H), ":", minute(M), ":", second(S).

% FIXME: deal with leap seconds
timezone(UTCOffset) -->
    "+", hour(H), ":", minute(M), { UTCOffset is -(H*3600+M*60) }.
timezone(UTCOffset) -->
    "+", hour(H), minute(M), { UTCOffset is -(H*3600+M*60) }.
timezone(UTCOffset) -->
    "+", hour(H), { UTCOffset is -(H*3600) }.
timezone(UTCOffset) -->
    "-", hour(H), ":", minute(M), { UTCOffset is H*3600+M*60 }.
timezone(UTCOffset) -->
    "-", hour(H), minute(M), { UTCOffset is H*3600+M*60 }.
timezone(UTCOffset) -->
    "-", hour(H), { UTCOffset is H*3600 }.
timezone(0) -->
    "Z".
timezone(0) -->
    ws, "UTC".
timezone(0) -->
    ws, "GMT".
timezone(_) -->   % unknown
    [].

day_name(0) --> "Sun".
day_name(1) --> "Mon".
day_name(2) --> "Tue".
day_name(3) --> "Wed".
day_name(4) --> "Thu".
day_name(5) --> "Fri".
day_name(6) --> "Sat".
day_name(7) --> "Sun".

full_day_name(0) --> "Sunday".
full_day_name(1) --> "Monday".
full_day_name(2) --> "Tuesday".
full_day_name(3) --> "Wednesday".
full_day_name(4) --> "Thursday".
full_day_name(5) --> "Friday".
full_day_name(6) --> "Saturday".
full_day_name(7) --> "Sunday".

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

asctime_day_of_the_month(D) -->
    " ", !, digit(D), {D > 0}.
asctime_day_of_the_month(D) -->
    day_of_the_month(D).

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

year2d(Y) -->
    digit(D0),
    digit(D1),
    { Y0 is D0*10+D1,
      (   Y0 >= 70, Y0 =< 99
      ->  Y is Y0+1900
      ;   Y is Y0+2000
      )
    }.

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
