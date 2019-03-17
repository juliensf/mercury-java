%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.time.LocalDateTime.
%
%---------------------------------------------------------------------------%

:- module jtime.local_date_time.
:- interface.

:- import_module io.

:- type local_date_time.

:- pred now(local_date_time::out, io::di, io::uo) is det.

:- func to_string(local_date_time) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module bool.
:- import_module exception.

:- interface.

:- pred equals(local_date_time::in, local_date_time::in) is semidet.

:- pred compare_to(comparison_result::uo, local_date_time::in, local_date_time::in)
    is det.

:- implementation.

:- pragma foreign_type("Java", local_date_time, "java.time.LocalDateTime") where
    equality is local_date_time.equals,
    comparison is local_date_time.compare_to.

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

:- pragma foreign_proc("Java",
    now(T::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    T = java.time.LocalDateTime.now();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(T::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = T.toString();
").

%---------------------------------------------------------------------------%
:- end_module local_date_time.
%---------------------------------------------------------------------------%
