%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.time.YearMonth.
%
%---------------------------------------------------------------------------%

:- module jtime.year_month.
:- interface.

:- import_module jtime.format.
:- import_module jtime.format.date_time_formatter.
:- import_module jtime.jtemporal.
:- import_module jtime.jtemporal.temporal.
:- import_module jtime.jtemporal.temporal_accessor.
:- import_module jtime.local_date.
:- import_module jtime.month.

:- import_module calendar.
:- import_module io.

%---------------------------------------------------------------------------%

:- type year_month.
:- instance temporal(year_month).
:- instance temporal_accessor(year_month).

%---------------------------------------------------------------------------%

:- func at_day(year_month, int) = local_date.

:- func at_end_of_month(year_month) = local_date.

:- pred format(year_month::in, date_time_formatter::in,
    string::out) is semidet.

:- func get_year(year_month) = int.

:- func get_month(year_month) = month.    % Mercury calendar.month/0.
:- func get_jmonth(year_month) = jmonth.  % java.time.Month.

:- func get_month_value(year_month) = int.

:- pred is_after(year_month::in, year_month::in) is semidet.

:- pred is_before(year_month::in, year_month::in) is semidet.

:- pred is_leap_year(year_month::in) is semidet.

:- pred now(year_month::out, io::di, io::uo) is det.

:- func of(int, month) = year_month.

:- func to_string(year_month) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

:- pred equals(year_month::in, year_month::in) is semidet.

:- pred compare_to(comparison_result::uo, year_month::in, year_month::in)
    is det.

:- implementation.

:- import_module jlang.
:- import_module jlang.throwable.
:- import_module jtime.month.
:- import_module jtime.year.

:- import_module bool.
:- import_module exception.
:- import_module int.
:- import_module require.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", year_month, "java.time.YearMonth") where
    equality is year_month.equals,
    comparison is year_month.compare_to.

:- instance temporal(year_month) where [].
:- instance temporal_accessor(year_month) where [].

%---------------------------------------------------------------------------%

at_day(YM, D) = LD :-
    do_at_day(YM, D, IsOk, LD, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_at_day(year_month::in, int::in, bool::out,
    local_date::out, throwable::out) is det.
:- pragma foreign_proc("Java",
    do_at_day(YM::in, D::in, IsOk::out, LD::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        LD = YM.atDay(D);
        IsOk = bool.YES;
        Error = null;
    } catch (java.time.DateTimeException e) {
        LD = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    at_end_of_month(YM::in) = (LD::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    LD = YM.atEndOfMonth();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    format(YM::in, Fmt::in, S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        S = YM.format(Fmt);
        SUCCESS_INDICATOR = true;
    } catch (java.time.DateTimeException e) {
        S = null;
        SUCCESS_INDICATOR = false;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_year(YM::in) = (Y::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Y = YM.getYear();
").

%---------------------------------------------------------------------------%

get_month(YM) = Month :-
    JMonth = get_jmonth(YM),
    Month = month.from_jmonth(JMonth).

:- pragma foreign_proc("Java",
    get_jmonth(YM::in) = (JM::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    JM = YM.getMonth();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_month_value(YM::in) = (MV::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MV = YM.getMonthValue();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_after(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = A.isAfter(B);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_before(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = A.isBefore(B);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_leap_year(YM::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = YM.isLeapYear();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    now(YM::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    YM = java.time.YearMonth.now();
").

%---------------------------------------------------------------------------%

of(Year, Month) = YearMonth :-
    ( if Year >= jtime.year.min_value, Year =< jtime.year.max_value then
        JMonth = to_jmonth(Month),
        do_of_year_jmonth(Year, JMonth, YearMonth)
    else
        unexpected("jtime.year_month.of", "year is out-of-range")
    ).

:- pred do_of_year_jmonth(int::in, jmonth::in, year_month::out) is det.
:- pragma foreign_proc("Java",
    do_of_year_jmonth(Y::in, M::in, YM::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    YM = java.time.YearMonth.of(Y, M);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(YM::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = YM.toString();
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
:- end_module year_month.
%---------------------------------------------------------------------------%
