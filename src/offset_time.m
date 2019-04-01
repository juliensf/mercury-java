%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.time.OffsetTime.
%
%---------------------------------------------------------------------------%

:- module jtime.offset_time.
:- interface.

:- import_module jtime.format.
:- import_module jtime.format.date_time_formatter.
:- import_module jtime.zone_offset.

:- import_module io.

:- type offset_time.

%---------------------------------------------------------------------------%

:- func max = offset_time.

:- func min = offset_time.

:- func get_hour(offset_time) = int.

:- func get_minute(offset_time) = int.

:- func get_nano(offset_time) = int.

:- func get_offset(offset_time) = zone_offset.

:- func get_second(offset_time) = int.

:- pred is_before(offset_time::in, offset_time::in) is semidet.

:- pred is_after(offset_time::in, offset_time::in) is semidet.

:- pred now(offset_time::out, io::di, io::uo) is det.

:- pred parse(string::in, offset_time::out) is semidet.

:- pred parse(string::in, date_time_formatter::in, offset_time::out)
    is semidet.

:- func to_string(offset_time) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

:- pred equals(offset_time::in, offset_time::in) is semidet.

:- pred compare_to(comparison_result::uo, offset_time::in, offset_time::in)
    is det.

:- implementation.

:- pragma foreign_type("Java", offset_time, "java.time.OffsetTime") where
    equality is offset_time.equals,
    comparison is offset_time.compare_to.

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
    D = java.time.OffsetTime.MAX;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    min = (D::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    D = java.time.OffsetTime.MIN;
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
    get_offset(T::in) = (Z::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Z = T.getOffset();
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
    now(T::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    T = java.time.OffsetTime.now();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    parse(S::in, T::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        T = java.time.OffsetTime.parse(S);
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
        T = java.time.OffsetTime.parse(S, Fmt);
        SUCCESS_INDICATOR = true;
    } catch (java.time.format.DateTimeParseException e) {
        T = null;
        SUCCESS_INDICATOR = false;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(T::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = T.toString();
").

%---------------------------------------------------------------------------%
:- end_module offset_time.
%---------------------------------------------------------------------------%
