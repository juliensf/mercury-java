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

:- pred now(local_date::out, io::di, io::uo) is det.

:- func plus_days(local_date, int64) = local_date.

:- func to_string(local_date) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

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
    now(D::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    D = java.time.LocalDate.now();
").

%---------------------------------------------------------------------------%

plus_days(Date, DaysToAdd) = Result :-
    do_plus_days(Date, DaysToAdd, Result, Ok, ErrMsg),
    (
        Ok = no,
        throw(software_error(ErrMsg))
    ;
        Ok = yes
    ).

:- pred do_plus_days(local_date::in, int64::in, local_date::out,
    bool::out, string::out) is det.

:- pragma foreign_proc("Java",
    do_plus_days(D0::in, Days::in, D::out, Ok::out, ErrMsg::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        D = D0.plusDays(Days);
        Ok = bool.YES;
        ErrMsg = """";
    } catch (java.time.DateTimeException e) {
        D = null;
        Ok = bool.NO;
        ErrMsg = e.getMessage();
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
