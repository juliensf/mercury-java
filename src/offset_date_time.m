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

:- module jtime.offset_date_time.
:- interface.

:- import_module jtime.format.
:- import_module jtime.format.date_time_formatter.
:- import_module jtime.instant.
:- import_module jtime.jtemporal.
:- import_module jtime.jtemporal.temporal.
:- import_module jtime.jtemporal.temporal_accessor.
:- import_module jtime.local_date.
:- import_module jtime.local_date_time.
:- import_module jtime.local_time.
:- import_module jtime.month.
:- import_module jtime.offset_time.
:- import_module jtime.zone_offset.

:- import_module calendar.
:- import_module io.

%---------------------------------------------------------------------------%

:- type offset_date_time.
:- instance temporal(offset_date_time).
:- instance temporal_accessor(offset_date_time).

:- func max = offset_date_time.

:- func min = offset_date_time.

:- pred format(offset_date_time::in, date_time_formatter::in, string::out)
    is semidet.

:- func get_day_of_month(offset_date_time) = int.

:- func get_day_of_year(offset_date_time) = int.

:- func get_hour(offset_date_time) = int.

:- func get_minute(offset_date_time) = int.

:- func get_month(offset_date_time) = month.    % Mercury calendar.month/0.
:- func get_jmonth(offset_date_time) = jmonth.  % java.time.Month.

:- func get_month_value(offset_date_time) = int.

:- func get_nano(offset_date_time) = int.

:- func get_offset(offset_date_time) = zone_offset.

:- func get_second(offset_date_time) = int.

:- func get_year(offset_date_time) = int.

:- pred is_before(offset_date_time::in, offset_date_time::in) is semidet.

:- pred is_after(offset_date_time::in, offset_date_time::in) is semidet.

:- pred now(offset_date_time::out, io::di, io::uo) is det.

:- pred parse(string::in, offset_date_time::out) is semidet.

:- pred parse(string::in, date_time_formatter::in, offset_date_time::out)
    is semidet.

:- func to_epoch_second(offset_date_time) = int64.

:- func to_instant(offset_date_time) = instant.

:- func to_local_date(offset_date_time) = local_date.

:- func to_local_date_time(offset_date_time) = local_date_time.

:- func to_local_time(offset_date_time) = local_time.

:- func to_offset_time(offset_date_time) = offset_time.

:- func to_string(offset_date_time) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

:- pred equals(offset_date_time::in, offset_date_time::in) is semidet.

:- pred compare_to(comparison_result::uo, offset_date_time::in,
    offset_date_time::in) is det.

:- implementation.

:- pragma foreign_type("Java", offset_date_time, "java.time.OffsetDateTime")
    where equality is offset_date_time.equals,
    comparison is offset_date_time.compare_to.

:- instance temporal(offset_date_time) where [].
:- instance temporal_accessor(offset_date_time) where [].

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
    max = (DT::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DT = java.time.OffsetDateTime.MAX;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    min = (DT::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DT = java.time.OffsetDateTime.MIN;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    format(ODT::in, Fmt::in, S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        S = ODT.format(Fmt);
        SUCCESS_INDICATOR = true;
    } catch (java.time.DateTimeException e) {
        S = null;
        SUCCESS_INDICATOR = false;
    }
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
    get_day_of_year(DT::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = DT.getDayOfYear();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_hour(DT::in) = (H::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    H = DT.getHour();
").

%---------------------------------------------------------------------------%

get_month(DT) = Month :-
    JMonth = get_jmonth(DT),
    Month = month.from_jmonth(JMonth).

:- pragma foreign_proc("Java",
    get_jmonth(DT::in) = (JM::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    JM = DT.getMonth();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_month_value(DT::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = DT.getMonthValue();
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
    get_offset(DT::in) = (Z::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Z = DT.getOffset();
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
    now(DT::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DT = java.time.OffsetDateTime.now();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    parse(S::in, DT::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        DT = java.time.OffsetDateTime.parse(S);
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
        DT = java.time.OffsetDateTime.parse(S, Fmt);
        SUCCESS_INDICATOR = true;
    } catch (java.time.format.DateTimeParseException e) {
        DT = null;
        SUCCESS_INDICATOR = false;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_epoch_second(DT::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = DT.toEpochSecond();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_instant(DT::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = DT.toInstant();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_local_date(DT::in) = (LD::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    LD = DT.toLocalDate();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_local_date_time(DT::in) = (LDT::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    LDT = DT.toLocalDateTime();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_local_time(DT::in) = (LT::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    LT = DT.toLocalTime();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_offset_time(ODT::in) = (OT::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    OT = ODT.toOffsetTime();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(DT::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = DT.toString();
").

%---------------------------------------------------------------------------%
:- end_module offset_date_time.
%---------------------------------------------------------------------------%
