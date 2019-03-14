%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.time.LocalDate.
%
%---------------------------------------------------------------------------%

:- module jtime.local_date.
:- interface.

:- import_module io.

:- type local_date.

:- func get_day_of_month(local_date) = int.

:- func get_day_of_year(local_date) = int.

:- func get_month_value(local_date) = int.

:- func get_year(local_date) = int.

:- pred is_before(local_date::in, local_date::in) is semidet.

:- pred is_after(local_date::in, local_date::in) is semidet.

:- pred is_equal(local_date::in, local_date::in) is semidet.

:- pred is_leap_year(local_date::in) is semidet.

:- pred now(local_date::out, io::di, io::uo) is det.

:- pred parse(string::in, local_date::out) is semidet.

:- func plus_days(local_date, int64) = local_date.

:- func to_string(local_date) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module bool.
:- import_module exception.

:- interface.

:- pred equals(local_date::in, local_date::in) is semidet.

:- pred compare_to(comparison_result::uo, local_date::in, local_date::in)
    is det.

:- implementation.

:- pragma foreign_type("Java", local_date, "java.time.LocalDate") where
    equality is local_date.equals,
    comparison is local_date.compare_to.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_day_of_month(D::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = D.getDayOfMonth();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_day_of_year(D::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = D.getDayOfYear();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_month_value(D::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = D.getMonthValue();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_year(D::in) = (Y::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Y = D.getYear();
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
    is_after(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = A.isAfter(B);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = A.isEqual(B);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_leap_year(A::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = A.isLeapYear();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    now(D::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    D = java.time.LocalDate.now();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    parse(S::in, D::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        D = java.time.LocalDate.parse(S);
        SUCCESS_INDICATOR = true;
    } catch (java.time.format.DateTimeParseException e) {
        D = null;
        SUCCESS_INDICATOR = false;
    }
").

%---------------------------------------------------------------------------%

plus_days(Date, DaysToAdd) = Result :-
    do_plus_days(Date, DaysToAdd, Result, Ok, Error),
    (
        Ok = no,
        throw(java_exception(Error))
    ;
        Ok = yes
    ).

:- pred do_plus_days(local_date::in, int64::in, local_date::out,
    bool::out, throwable::out) is det.

:- pragma foreign_proc("Java",
    do_plus_days(D0::in, Days::in, D::out, Ok::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        D = D0.plusDays(Days);
        Ok = bool.YES;
        Error = null;
    } catch (java.time.DateTimeException e) {
        D = null;
        Ok = bool.NO;
        Error = e;
    }
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
:- end_module local_date.
%---------------------------------------------------------------------------%
