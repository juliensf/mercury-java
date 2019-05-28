%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.time.YearMonth.
%
%---------------------------------------------------------------------------%

:- module jtime.year_month.
:- interface.

:- import_module jtime.jtemporal.
:- import_module jtime.jtemporal.temporal_accessor.
:- import_module jtime.local_date.

:- import_module io.

:- type year_month.
:- instance temporal_accessor(year_month).

%---------------------------------------------------------------------------%

:- func at_end_of_month(year_month) = local_date.

:- func get_year(year_month) = int.

:- func get_month_value(year_month) = int.

:- pred is_after(year_month::in, year_month::in) is semidet.

:- pred is_before(year_month::in, year_month::in) is semidet.

:- pred is_leap_year(year_month::in) is semidet.

:- pred now(year_month::out, io::di, io::uo) is det.

:- func to_string(year_month) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

:- pred equals(year_month::in, year_month::in) is semidet.

:- pred compare_to(comparison_result::uo, year_month::in, year_month::in)
    is det.

:- implementation.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", year_month, "java.time.YearMonth") where
    equality is year_month.equals,
    comparison is year_month.compare_to.

:- instance temporal_accessor(year_month) where [].

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    at_end_of_month(YM::in) = (LD::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    LD = YM.atEndOfMonth();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_year(YM::in) = (Y::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Y = YM.getYear();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_month_value(YM::in) = (MV::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MV = YM.getMonthValue();
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
    is_leap_year(YM::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = YM.isLeapYear();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    now(YM::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    YM = java.time.YearMonth.now();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(YM::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = YM.toString();
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
:- end_module year_month.
%---------------------------------------------------------------------------%
