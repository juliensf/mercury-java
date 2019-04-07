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

:- func get_nano(duration) = int.

:- func get_seconds(duration) = int64.

:- pred is_negative(duration::in) is semidet.

:- pred is_zero(duration::in) is semidet.

:- func negated(duration) = duration.

:- pred parse(string::in, duration::out) is semidet.

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

:- pragma foreign_proc("Java",
    negated(D::in) = (ND::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ND = D.negated();
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
