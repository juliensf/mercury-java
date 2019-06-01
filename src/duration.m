%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.time.Duration.
%
%---------------------------------------------------------------------------%

:- module jtime.duration.
:- interface.

%---------------------------------------------------------------------------%

:- type duration.

:- func zero = duration.

:- func abs(duration) = duration.

:- func divided_by(duration, int64) = duration.

:- func get_nano(duration) = int.

:- func get_seconds(duration) = int64.

:- pred is_negative(duration::in) is semidet.

:- pred is_zero(duration::in) is semidet.

:- func minus(duration, duration) = duration.

:- func minus_days(duration, int64) = duration.

:- func minus_hours(duration, int64) = duration.

:- func minus_millis(duration, int64) = duration.

:- func minus_minutes(duration, int64) = duration.

:- func minus_nanos(duration, int64) = duration.

:- func minus_seconds(duration, int64) = duration.

:- func multiplied_by(duration, int64) = duration.

:- func negated(duration) = duration.

:- func of_days(int64) = duration.

:- func of_hours(int64) = duration.

:- func of_millis(int64) = duration.

:- func of_minutes(int64) = duration.

:- func of_nanos(int64) = duration.

:- func of_seconds(int64) = duration.

:- func of_seconds(int64, int64) = duration.

:- pred parse(string::in, duration::out) is semidet.

:- func plus(duration, duration) = duration.

:- func plus_days(duration, int64) = duration.

:- func plus_hours(duration, int64) = duration.

:- func plus_millis(duration, int64) = duration.

:- func plus_minutes(duration, int64) = duration.

:- func plus_nanos(duration, int64) = duration.

:- func plus_seconds(duration, int64) = duration.

:- func to_days(duration) = int64.

:- func to_hours(duration) = int64.

:- func to_minutes(duration) = int64.

:- func to_nanos(duration) = int64.

:- func to_string(duration) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

:- pred equals(duration::in, duration::in) is semidet.

:- pred compare_to(comparison_result::uo, duration::in, duration::in)
    is det.

:- implementation.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module bool.
:- import_module exception.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", duration, "java.time.Duration") where
    equality is duration.equals,
    comparison is duration.compare_to.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    zero = (D::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    D = java.time.Duration.ZERO;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    abs(D::in) = (AD::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    AD = D.abs();
").

%---------------------------------------------------------------------------%

divided_by(Duration, Divisor) = Result :-
    do_divided_by(Duration, Divisor, IsOk, Result, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_divided_by(duration::in, int64::in, bool::out, duration::out,
    throwable::out) is det.
:- pragma foreign_proc("Java",
    do_divided_by(D::in, Div::in, IsOk::out, Result::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        Result = D.dividedBy(Div);
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
    get_nano(D::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = D.getNano();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_seconds(D::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = D.getSeconds();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_negative(D::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = D.isNegative();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_zero(D::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = D.isZero();
").

%---------------------------------------------------------------------------%

minus(A, B) = C :-
    do_minus(A, B, IsOk, C, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_minus(duration::in, duration::in, bool::out,
    duration::out, throwable::out) is det.
:- pragma foreign_proc("Java",
    do_minus(A::in, B::in, IsOk::out, C::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        C = A.minus(B);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        C = null;
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

:- pred do_minus_days(duration::in, int64::in, bool::out,
    duration::out, throwable::out) is det.
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

minus_hours(A, B) = C :-
    do_minus_hours(A, B, IsOk, C, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_minus_hours(duration::in, int64::in, bool::out,
    duration::out, throwable::out) is det.
:- pragma foreign_proc("Java",
    do_minus_hours(A::in, B::in, IsOk::out, C::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        C = A.minusHours(B);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        C = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

minus_millis(A, B) = C :-
    do_minus_millis(A, B, IsOk, C, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_minus_millis(duration::in, int64::in, bool::out,
    duration::out, throwable::out) is det.
:- pragma foreign_proc("Java",
    do_minus_millis(A::in, B::in, IsOk::out, C::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        C = A.minusMillis(B);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        C = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

minus_minutes(A, B) = C :-
    do_minus_minutes(A, B, IsOk, C, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_minus_minutes(duration::in, int64::in, bool::out,
    duration::out, throwable::out) is det.
:- pragma foreign_proc("Java",
    do_minus_minutes(A::in, B::in, IsOk::out, C::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        C = A.minusMinutes(B);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        C = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

minus_nanos(A, B) = C :-
    do_minus_nanos(A, B, IsOk, C, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_minus_nanos(duration::in, int64::in, bool::out,
    duration::out, throwable::out) is det.
:- pragma foreign_proc("Java",
    do_minus_nanos(A::in, B::in, IsOk::out, C::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        C = A.minusNanos(B);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        C = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

minus_seconds(A, B) = C :-
    do_minus_seconds(A, B, IsOk, C, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_minus_seconds(duration::in, int64::in, bool::out,
    duration::out, throwable::out) is det.
:- pragma foreign_proc("Java",
    do_minus_seconds(A::in, B::in, IsOk::out, C::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        C = A.minusSeconds(B);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        C = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

multiplied_by(Duration, Multiplicand) = Result :-
    do_multiplied_by(Duration, Multiplicand, IsOk, Result, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_multiplied_by(duration::in, int64::in, bool::out, duration::out,
    throwable::out) is det.
:- pragma foreign_proc("Java",
    do_multiplied_by(D::in, M::in, IsOk::out, Result::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        Result = D.multipliedBy(M);
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
    negated(D::in) = (ND::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ND = D.negated();
").

%---------------------------------------------------------------------------%

of_days(N) = D :-
    do_of_days(N, IsOk, D, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_of_days(int64::in, bool::out, duration::out, throwable::out)
    is det.
:- pragma foreign_proc("Java",
    do_of_days(N::in, IsOk::out, D::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        D = java.time.Duration.ofDays(N);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        D = null;
        IsOk = bool.NO;
        Error = e;
    }
").
%---------------------------------------------------------------------------%

of_hours(N) = D :-
    do_of_hours(N, IsOk, D, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_of_hours(int64::in, bool::out, duration::out, throwable::out)
    is det.
:- pragma foreign_proc("Java",
    do_of_hours(N::in, IsOk::out, D::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        D = java.time.Duration.ofHours(N);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        D = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

of_millis(N) = D :-
    do_of_millis(N, IsOk, D, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_of_millis(int64::in, bool::out, duration::out, throwable::out)
    is det.
:- pragma foreign_proc("Java",
    do_of_millis(N::in, IsOk::out, D::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        D = java.time.Duration.ofMillis(N);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        D = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

of_minutes(N) = D :-
    do_of_minutes(N, IsOk, D, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_of_minutes(int64::in, bool::out, duration::out, throwable::out)
    is det.
:- pragma foreign_proc("Java",
    do_of_minutes(N::in, IsOk::out, D::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        D = java.time.Duration.ofMinutes(N);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        D = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

of_nanos(N) = D :-
    do_of_nanos(N, IsOk, D, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_of_nanos(int64::in, bool::out, duration::out, throwable::out)
    is det.
:- pragma foreign_proc("Java",
    do_of_nanos(N::in, IsOk::out, D::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        D = java.time.Duration.ofNanos(N);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        D = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

of_seconds(N) = D :-
    do_of_seconds(N, IsOk, D, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_of_seconds(int64::in, bool::out, duration::out, throwable::out)
    is det.
:- pragma foreign_proc("Java",
    do_of_seconds(N::in, IsOk::out, D::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        D = java.time.Duration.ofSeconds(N);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        D = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

of_seconds(N, Adj) = D :-
    do_of_seconds(N, Adj, IsOk, D, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_of_seconds(int64::in, int64::in, bool::out, duration::out,
    throwable::out) is det.
:- pragma foreign_proc("Java",
    do_of_seconds(N::in, Adj::in, IsOk::out, D::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        D = java.time.Duration.ofSeconds(N, Adj);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        D = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    parse(S::in, D::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        D = java.time.Duration.parse(S);
        SUCCESS_INDICATOR = true;
    } catch (java.time.format.DateTimeParseException e) {
        D = null;
        SUCCESS_INDICATOR = false;
    }
").
%---------------------------------------------------------------------------%

plus(A, B) = C :-
    do_plus(A, B, IsOk, C, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_plus(duration::in, duration::in, bool::out,
    duration::out, throwable::out) is det.
:- pragma foreign_proc("Java",
    do_plus(A::in, B::in, IsOk::out, C::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        C = A.plus(B);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        C = null;
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

:- pred do_plus_days(duration::in, int64::in, bool::out,
    duration::out, throwable::out) is det.
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

plus_hours(A, B) = C :-
    do_plus_hours(A, B, IsOk, C, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_plus_hours(duration::in, int64::in, bool::out,
    duration::out, throwable::out) is det.
:- pragma foreign_proc("Java",
    do_plus_hours(A::in, B::in, IsOk::out, C::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        C = A.plusHours(B);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        C = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

plus_millis(A, B) = C :-
    do_plus_millis(A, B, IsOk, C, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_plus_millis(duration::in, int64::in, bool::out,
    duration::out, throwable::out) is det.
:- pragma foreign_proc("Java",
    do_plus_millis(A::in, B::in, IsOk::out, C::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        C = A.plusMillis(B);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        C = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

plus_minutes(A, B) = C :-
    do_plus_minutes(A, B, IsOk, C, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_plus_minutes(duration::in, int64::in, bool::out,
    duration::out, throwable::out) is det.
:- pragma foreign_proc("Java",
    do_plus_minutes(A::in, B::in, IsOk::out, C::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        C = A.plusMinutes(B);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        C = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

plus_nanos(A, B) = C :-
    do_plus_nanos(A, B, IsOk, C, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_plus_nanos(duration::in, int64::in, bool::out,
    duration::out, throwable::out) is det.
:- pragma foreign_proc("Java",
    do_plus_nanos(A::in, B::in, IsOk::out, C::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        C = A.plusNanos(B);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.ArithmeticException e) {
        C = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

plus_seconds(A, B) = C :-
    do_plus_seconds(A, B, IsOk, C, Error),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_plus_seconds(duration::in, int64::in, bool::out,
    duration::out, throwable::out) is det.
:- pragma foreign_proc("Java",
    do_plus_seconds(A::in, B::in, IsOk::out, C::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        C = A.plusSeconds(B);
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
    to_days(D::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = D.toDays();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_hours(D::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = D.toHours();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_minutes(D::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = D.toMinutes();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_nanos(D::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = D.toNanos();
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
:- end_module duration.
%---------------------------------------------------------------------------%
