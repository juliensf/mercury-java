%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.time.Year.
%
%---------------------------------------------------------------------------%

:- module jtime.year.
:- interface.

:- import_module io.

:- type year.

%---------------------------------------------------------------------------%

:- pred is_after(year::in, year::in) is semidet.

:- pred is_before(year::in, year::in) is semidet.

:- pred is_leap(year::in) is semidet.

:- pred now(year::out, io::di, io::uo) is det.

:- func to_string(year) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

:- pred equals(year::in, year::in) is semidet.

:- pred compare_to(comparison_result::uo, year::in, year::in) is det.

:- implementation.

:- pragma foreign_type("Java", year, "java.time.Year") where
    equality is year.equals,
    comparison is year.compare_to.

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
    is_leap(Y::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = Y.isLeap();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    now(Y::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Y = java.time.Year.now();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(Y::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = Y.toString();
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
:- end_module year.
%---------------------------------------------------------------------------%
