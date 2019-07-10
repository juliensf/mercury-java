%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.time.ZonedDateTime.
%
%---------------------------------------------------------------------------%

:- module jtime.zoned_date_time.
:- interface.

:- import_module jtime.format.
:- import_module jtime.format.date_time_formatter.
:- import_module jtime.jtemporal.
:- import_module jtime.jtemporal.temporal.
:- import_module jtime.jtemporal.temporal_accessor.
:- import_module jtime.local_date.
:- import_module jtime.local_date_time.
:- import_module jtime.local_time.
:- import_module jtime.month.
:- import_module jtime.zone_offset.

:- import_module calendar.
:- import_module io.

%---------------------------------------------------------------------------%

:- type zoned_date_time.
:- instance temporal(zoned_date_time).
:- instance temporal_accessor(zoned_date_time).

:- pred format(zoned_date_time::in, date_time_formatter::in, string::out)
    is semidet.

:- func get_day_of_month(zoned_date_time) = int.

:- func get_day_of_year(zoned_date_time) = int.

:- func get_hour(zoned_date_time) = int.

:- func get_minute(zoned_date_time) = int.

:- func get_month(zoned_date_time) = month.    % Mercury calendar.month/0.
:- func get_jmonth(zoned_date_time) = jmonth.  % java.time.Month.

:- func get_month_value(zoned_date_time) = int.

:- func get_nano(zoned_date_time) = int.

:- func get_offset(zoned_date_time) = zone_offset.

:- func get_second(zoned_date_time) = int.

:- func get_year(zoned_date_time) = int.

:- pred now(zoned_date_time::out, io::di, io::uo) is det.

:- pred parse(string::in, zoned_date_time::out) is semidet.

:- pred parse(string::in, date_time_formatter::in, zoned_date_time::out)
    is semidet.

:- func to_local_date(zoned_date_time) = local_date.

:- func to_local_date_time(zoned_date_time) = local_date_time.

:- func to_local_time(zoned_date_time) = local_time.

:- func to_string(zoned_date_time) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

:- pred equals(zoned_date_time::in, zoned_date_time::in) is semidet.

:- pred compare_to(comparison_result::uo, zoned_date_time::in,
    zoned_date_time::in) is det.

:- implementation.

:- pragma foreign_type("Java", zoned_date_time, "java.time.ZonedDateTime")
    where equality is zoned_date_time.equals,
    comparison is zoned_date_time.compare_to.

:- instance temporal(zoned_date_time) where [].
:- instance temporal_accessor(zoned_date_time) where [].

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
    format(ZDT::in, Fmt::in, S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        S = ZDT.format(Fmt);
        SUCCESS_INDICATOR = true;
    } catch (java.time.DateTimeException e) {
        S = null;
        SUCCESS_INDICATOR = false;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_day_of_month(ZDT::in) = (D::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    D = ZDT.getDayOfMonth();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_day_of_year(ZDT::in) = (D::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    D = ZDT.getDayOfYear();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_hour(ZDT::in) = (H::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    H = ZDT.getHour();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_minute(ZDT::in) = (H::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    H = ZDT.getMinute();
").

%---------------------------------------------------------------------------%

get_month(ZDT) = Month :-
    JMonth = get_jmonth(ZDT),
    Month = month.from_jmonth(JMonth).

:- pragma foreign_proc("Java",
    get_jmonth(ZDT::in) = (JM::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    JM = ZDT.getMonth();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_month_value(ZDT::in) = (MV::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MV = ZDT.getMonthValue();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_nano(ZDT::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = ZDT.getNano();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_offset(ZDT::in) = (ZO::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ZO = ZDT.getOffset();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_second(ZDT::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = ZDT.getSecond();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_year(ZDT::in) = (Y::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Y = ZDT.getYear();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    now(ZDT::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ZDT = java.time.ZonedDateTime.now();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    parse(S::in, ZDT::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ZDT = java.time.ZonedDateTime.parse(S);
        SUCCESS_INDICATOR = true;
    } catch (java.time.format.DateTimeParseException e) {
        ZDT = null;
        SUCCESS_INDICATOR = false;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    parse(S::in, Fmt::in, ZDT::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ZDT = java.time.ZonedDateTime.parse(S, Fmt);
        SUCCESS_INDICATOR = true;
    } catch (java.time.format.DateTimeParseException e) {
        ZDT = null;
        SUCCESS_INDICATOR = false;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_local_date(ZDT::in) = (LD::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    LD = ZDT.toLocalDate();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_local_date_time(ZDT::in) = (LDT::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    LDT = ZDT.toLocalDateTime();
").
%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_local_time(ZDT::in) = (LT::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    LT = ZDT.toLocalTime();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(ZDT::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = ZDT.toString();
").

%---------------------------------------------------------------------------%
:- end_module zoned_date_time.
%---------------------------------------------------------------------------%
