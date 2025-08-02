%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.time.DayOfWeek.
%
%---------------------------------------------------------------------------%

:- module jtime.day_of_week.
:- interface.

:- import_module jtime.jtemporal.
:- import_module jtime.jtemporal.temporal_accessor.

:- import_module calendar.

%---------------------------------------------------------------------------%

:- type jday_of_week.
:- instance temporal_accessor(jday_of_week).

    % Conversion to and from the Mercury standard library's
    % calander.day_of_week/0 type.
    %
:- func from_jday_of_week(jday_of_week) = day_of_week.
:- func to_jday_of_week(day_of_week) = jday_of_week.

%---------------------------------------------------------------------------%

:- func get_value(jday_of_week) = int.

:- func of(int) = jday_of_week.

:- func to_string(jday_of_week) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

:- pred equals(jday_of_week::in, jday_of_week::in) is semidet.

:- pred compare_to(comparison_result::uo, jday_of_week::in,
    jday_of_week::in) is det.

:- implementation.

:- import_module int.
:- import_module require.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", jday_of_week, "java.time.DayOfWeek") where
    equality is day_of_week.equals,
    comparison is day_of_week.compare_to.

:- instance temporal_accessor(jday_of_week) where [].

%---------------------------------------------------------------------------%

from_jday_of_week(JDayOfWeek) = DayOfWeek :-
    I = get_value(JDayOfWeek),
    ( if day_of_week_int(DayOfWeek0, I) then
        DayOfWeek = DayOfWeek0
    else
        unexpected("jtime.day_of_week.from_jday_of_week",
            "value of out-of-range")
    ).

to_jday_of_week(DayOfWeek) = JDayOfWeek :-
    day_of_week_int(DayOfWeek, I),
    do_of(I, JDayOfWeek).

:- pred day_of_week_int(day_of_week, int).
:- mode day_of_week_int(in, out) is det.
:- mode day_of_week_int(out, in) is semidet.

day_of_week_int(monday, 1).
day_of_week_int(tuesday, 2).
day_of_week_int(wednesday, 3).
day_of_week_int(thursday, 4).
day_of_week_int(friday, 5).
day_of_week_int(saturday, 6).
day_of_week_int(sunday, 7).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_value(D::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = D.getValue();
").

%---------------------------------------------------------------------------%

of(I) = Month :-
    ( if I > 0, I < 8 then
        do_of(I, Month)
    else
        unexpected("jtime.day_of_week.of", "argument is not in 1..7")
    ).

:- pred do_of(int::in, jday_of_week::out) is det.
:- pragma foreign_proc("Java",
    do_of(I::in, D::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    D = java.time.DayOfWeek.of(I);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(D::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = D.toString();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    equals(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = A.equals(B);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    compare_to(Result::uo, A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    int res = A.compareTo(B);
    if (res < 0) {
        Result = builtin.COMPARE_LESS;
    } else if (res > 0) {
        Result = builtin.COMPARE_GREATER;
    } else {
        Result = builtin.COMPARE_EQUAL;
    }
").

%---------------------------------------------------------------------------%
:- end_module jtime.day_of_week.
%---------------------------------------------------------------------------%
