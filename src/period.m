%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.time.Period
%
%---------------------------------------------------------------------------%

:- module jtime.period.
:- interface.

:- import_module jtime.local_date.
:- import_module jtime.jtemporal.
:- import_module jtime.jtemporal.temporal_amount.

%---------------------------------------------------------------------------%

:- type period.

:- instance temporal_amount(period).

%---------------------------------------------------------------------------%

:- func zero = period.

:- func between(local_date, local_date) = period.

:- func from(T) = period <= temporal_amount(T).

:- func get_days(period) = int.

:- func get_months(period) = int.

:- func get_years(period) = int.

:- pred is_negative(period::in) is semidet.

:- pred is_zero(period::in) is semidet.

:- func minus(period, T) = period <= temporal_amount(T).

:- func minus_days(period, int64) = period.

:- func minus_months(period, int64) = period.

:- func minus_years(period, int64) = period.

:- func multiplied_by(period, int) = period.

:- pred parse(string::in, period::out) is semidet.

:- func plus(period, T) = period <= temporal_amount(T).

:- func plus_days(period, int64) = period.

:- func plus_months(period, int64) = period.

:- func plus_years(period, int64) = period.

:- func to_string(period) = string.

:- func to_total_months(period) = int64.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

:- pred equals(period::in, period::in) is semidet.

:- implementation.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module bool.
:- import_module exception.

:- pragma foreign_type("Java", period, "java.time.Period") where
    equality is period.equals.

:- instance temporal_amount(period) where [].

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    zero = (P::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    P = java.time.Period.ZERO;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    between(A::in, B::in) = (P::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    P = java.time.Period.between(A, B);
").

%---------------------------------------------------------------------------%

from(TA) = Period :-
    do_from(TA, IsOk, Period, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_from(T::in, bool::out, period::out, throwable::out)
    is det <= temporal_amount(T).
:- pragma foreign_proc("Java",
    do_from(TA::in, IsOk::out, P::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        P = java.time.Period.from((java.time.temporal.TemporalAmount) TA);
        IsOk = bool.YES;
        Error = null;
    } catch (java.time.DateTimeException | java.lang.ArithmeticException e) {
        P = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_days(P::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = P.getDays();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_months(P::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = P.getMonths();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_years(P::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = P.getYears();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_negative(P::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = P.isNegative();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_zero(P::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = P.isZero();
").

%---------------------------------------------------------------------------%

minus(P0, TA) = P :-
    do_minus(P0, TA, IsOk, P, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_minus(period::in, T::in, bool::out, period::out, throwable::out)
    is det <= temporal_amount(T).
:- pragma foreign_proc("Java",
    do_minus(P0::in, TA::in, IsOk::out, P::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        P = P0.minus((java.time.temporal.TemporalAmount) TA);
        IsOk = bool.YES;
        Error = null;
    } catch (java.time.DateTimeException | java.lang.ArithmeticException e) {
        P = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

minus_days(A, B) = C :-
    do_minus_days(A, B, IsOk, C, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_minus_days(period::in, int64::in, bool::out,
    period::out, throwable::out) is det.
:- pragma foreign_proc("Java",
    do_minus_days(A::in, B::in, IsOk::out, C::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        C = A.minusDays(B);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        C = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

minus_months(A, B) = C :-
    do_minus_months(A, B, IsOk, C, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_minus_months(period::in, int64::in, bool::out,
    period::out, throwable::out) is det.
:- pragma foreign_proc("Java",
    do_minus_months(A::in, B::in, IsOk::out, C::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        C = A.minusMonths(B);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        C = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

minus_years(A, B) = C :-
    do_minus_years(A, B, IsOk, C, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_minus_years(period::in, int64::in, bool::out,
    period::out, throwable::out) is det.
:- pragma foreign_proc("Java",
    do_minus_years(A::in, B::in, IsOk::out, C::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        C = A.minusYears(B);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        C = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

multiplied_by(Period, Multiplicand) = Result :-
    do_multiplied_by(Period, Multiplicand, IsOk, Result, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_multiplied_by(period::in, int::in, bool::out, period::out,
    throwable::out) is det.
:- pragma foreign_proc("Java",
    do_multiplied_by(P::in, M::in, IsOk::out, Result::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        Result = P.multipliedBy(M);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        Result = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    parse(S::in, P::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        P = java.time.Period.parse(S);
        SUCCESS_INDICATOR = true;
    } catch (java.time.format.DateTimeParseException e) {
        P = null;
        SUCCESS_INDICATOR = false;
    }
").

%---------------------------------------------------------------------------%

plus(P0, TA) = P :-
    do_plus(P0, TA, IsOk, P, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_plus(period::in, T::in, bool::out, period::out, throwable::out)
    is det <= temporal_amount(T).
:- pragma foreign_proc("Java",
    do_plus(P0::in, TA::in, IsOk::out, P::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        P = P0.plus((java.time.temporal.TemporalAmount) TA);
        IsOk = bool.YES;
        Error = null;
    } catch (java.time.DateTimeException | java.lang.ArithmeticException e) {
        P = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

plus_days(A, B) = C :-
    do_plus_days(A, B, IsOk, C, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_plus_days(period::in, int64::in, bool::out,
    period::out, throwable::out) is det.
:- pragma foreign_proc("Java",
    do_plus_days(A::in, B::in, IsOk::out, C::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        C = A.plusDays(B);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        C = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

plus_months(A, B) = C :-
    do_plus_months(A, B, IsOk, C, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_plus_months(period::in, int64::in, bool::out,
    period::out, throwable::out) is det.
:- pragma foreign_proc("Java",
    do_plus_months(A::in, B::in, IsOk::out, C::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        C = A.plusMonths(B);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        C = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

plus_years(A, B) = C :-
    do_plus_years(A, B, IsOk, C, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_plus_years(period::in, int64::in, bool::out,
    period::out, throwable::out) is det.
:- pragma foreign_proc("Java",
    do_plus_years(A::in, B::in, IsOk::out, C::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        C = A.plusYears(B);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        C = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(P::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = P.toString();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_total_months(P::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = P.toTotalMonths();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    equals(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = A.equals(B);
").

%---------------------------------------------------------------------------%
:- end_module jtime.period.
%---------------------------------------------------------------------------%
