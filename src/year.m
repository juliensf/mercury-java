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

:- import_module jtime.format.
:- import_module jtime.format.date_time_formatter.
:- import_module jtime.jtemporal.
:- import_module jtime.jtemporal.temporal.
:- import_module jtime.jtemporal.temporal_accessor.

:- import_module io.

%---------------------------------------------------------------------------%

:- type year.
:- instance temporal(year).
:- instance temporal_accessor(year).

:- pred is_after(year::in, year::in) is semidet.

:- pred is_before(year::in, year::in) is semidet.

:- pred is_leap(year::in) is semidet.

:- pred format(year::in, date_time_formatter::in, string::out) is semidet.

:- pred now(year::out, io::di, io::uo) is det.

:- pred parse(string::in, year::out) is semidet.

:- pred parse(string::in, date_time_formatter::in, year::out)
    is semidet.

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

:- instance temporal(year) where [].
:- instance temporal_accessor(year) where [].

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    format(Y::in, Fmt::in, S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        S = Y.format(Fmt);
        SUCCESS_INDICATOR = true;
    } catch (java.time.DateTimeException e) {
        S = null;
        SUCCESS_INDICATOR = false;
    }
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
    parse(S::in, Y::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        Y = java.time.Year.parse(S);
        SUCCESS_INDICATOR = true;
    } catch (java.time.format.DateTimeParseException e) {
        Y = null;
        SUCCESS_INDICATOR = false;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    parse(S::in, Fmt::in, Y::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        Y = java.time.Year.parse(S, Fmt);
        SUCCESS_INDICATOR = true;
    } catch (java.time.format.DateTimeParseException e) {
        Y = null;
        SUCCESS_INDICATOR = false;
    }
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
