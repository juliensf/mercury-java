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

:- import_module jtime.format.
:- import_module jtime.format.date_time_formatter.
:- import_module jtime.local_date.
:- import_module jtime.local_time.

:- import_module io.

%---------------------------------------------------------------------------%

:- type local_date_time.

:- func min = local_date_time.

:- func max = local_date_time.

:- func get_day_of_month(local_date_time) = int.

:- func get_hour(local_date_time) = int.

:- func get_minute(local_date_time) = int.

:- func get_month_value(local_date_time) = int.

:- func get_nano(local_date_time) = int.

:- func get_second(local_date_time) = int.

:- func get_year(local_date_time) = int.

:- pred is_after(local_date_time::in, local_date_time::in) is semidet.

:- pred is_before(local_date_time::in, local_date_time::in) is semidet.

:- pred is_equal(local_date_time::in, local_date_time::in) is semidet.

:- pred parse(string::in, local_date_time::out) is semidet.

:- pred parse(string::in, date_time_formatter::in, local_date_time::out)
    is semidet.

:- pred now(local_date_time::out, io::di, io::uo) is det.

:- func of(local_date, local_time) = local_date_time.

:- func to_string(local_date_time) = string.

:- func to_local_date(local_date_time) = local_date.

:- func to_local_time(local_date_time) = local_time.

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
    min = (DT::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DT = java.time.LocalDateTime.MIN;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    max = (DT::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DT = java.time.LocalDateTime.MAX;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_day_of_month(DT::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = DT.getDayOfMonth();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_hour(DT::in) = (H::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    H = DT.getHour();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_minute(DT::in) = (M::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    M = DT.getMinute();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_month_value(DT::in) = (M::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    M = DT.getMonthValue();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_nano(DT::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = DT.getNano();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_second(DT::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = DT.getSecond();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_year(DT::in) = (Y::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Y = DT.getYear();
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
    is_equal(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = A.isEqual(B);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    parse(S::in, DT::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        DT = java.time.LocalDateTime.parse(S);
        SUCCESS_INDICATOR = true;
    } catch (java.time.format.DateTimeParseException e) {
        DT = null;
        SUCCESS_INDICATOR = false;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    parse(S::in, Fmt::in, DT::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        DT = java.time.LocalDateTime.parse(S, Fmt);
        SUCCESS_INDICATOR = true;
    } catch (java.time.format.DateTimeParseException e) {
        DT = null;
        SUCCESS_INDICATOR = false;
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
    of(D::in, T::in) = (DT::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DT = java.time.LocalDateTime.of(D, T);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(DT::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = DT.toString();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_local_date(DT::in) = (D::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    D = DT.toLocalDate();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_local_time(DT::in) = (T::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    T = DT.toLocalTime();
").

%---------------------------------------------------------------------------%
:- end_module local_date_time.
%---------------------------------------------------------------------------%
