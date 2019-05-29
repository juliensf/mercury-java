%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.time.LocalTime.
%
%---------------------------------------------------------------------------%

:- module jtime.local_time.
:- interface.

:- import_module jtime.format.
:- import_module jtime.format.date_time_formatter.
:- import_module jtime.jtemporal.
:- import_module jtime.jtemporal.temporal.
:- import_module jtime.jtemporal.temporal_accessor.
:- import_module jtime.local_date.
:- import_module jtime.local_date_time.

:- import_module io.

%---------------------------------------------------------------------------%

:- type local_time.
:- instance temporal(local_time).
:- instance temporal_accessor(local_time).

:- func max = local_time.

:- func midnight = local_time.

:- func min = local_time.

:- func noon = local_time.

:- func at_date(local_time, local_date) = local_date_time.

:- pred format(local_time::in, date_time_formatter::in, string::out)
    is semidet.

:- func get_hour(local_time) = int.

:- func get_minute(local_time) = int.

:- func get_nano(local_time) = int.

:- func get_second(local_time) = int.

:- pred is_before(local_time::in, local_time::in) is semidet.

:- pred is_after(local_time::in, local_time::in) is semidet.

:- pred now(local_time::out, io::di, io::uo) is det.

:- pred parse(string::in, local_time::out) is semidet.

:- pred parse(string::in, date_time_formatter::in, local_time::out)
    is semidet.

:- func to_nano_of_day(local_time) = int64.

:- func to_second_of_day(local_time) = int.

:- func to_string(local_time) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module bool.
:- import_module exception.

:- interface.

:- pred equals(local_time::in, local_time::in) is semidet.

:- pred compare_to(comparison_result::uo, local_time::in, local_time::in)
    is det.

:- implementation.

:- pragma foreign_type("Java", local_time, "java.time.LocalTime") where
    equality is local_time.equals,
    comparison is local_time.compare_to.

:- instance temporal(local_time) where [].
:- instance temporal_accessor(local_time) where [].

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
    max = (D::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    D = java.time.LocalTime.MAX;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    midnight = (D::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    D = java.time.LocalTime.MIDNIGHT;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    min = (D::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    D = java.time.LocalTime.MIN;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    noon = (D::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    D = java.time.LocalTime.NOON;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    at_date(T::in, D::in) = (DT::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DT = T.atDate(D);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    format(T::in, Fmt::in, S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        S = T.format(Fmt);
        SUCCESS_INDICATOR = true;
    } catch (java.time.DateTimeException e) {
        S = null;
        SUCCESS_INDICATOR = false;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_hour(T::in) = (H::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    H = T.getHour();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_minute(T::in) = (M::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    M = T.getMinute();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_nano(T::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = T.getNano();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_second(T::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = T.getSecond();
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
    now(T::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    T = java.time.LocalTime.now();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    parse(S::in, T::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        T = java.time.LocalTime.parse(S);
        SUCCESS_INDICATOR = true;
    } catch (java.time.format.DateTimeParseException e) {
        T = null;
        SUCCESS_INDICATOR = false;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    parse(S::in, Fmt::in, T::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        T = java.time.LocalTime.parse(S, Fmt);
        SUCCESS_INDICATOR = true;
    } catch (java.time.format.DateTimeParseException e) {
        T = null;
        SUCCESS_INDICATOR = false;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_nano_of_day(T::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = T.toNanoOfDay();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_second_of_day(T::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = T.toSecondOfDay();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(T::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = T.toString();
").

%---------------------------------------------------------------------------%
:- end_module local_time.
%---------------------------------------------------------------------------%
