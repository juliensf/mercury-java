%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.time.ZoneOffset.
%
%---------------------------------------------------------------------------%

:- module jtime.zone_offset.
:- interface.

:- import_module jtime.jtemporal.
:- import_module jtime.jtemporal.temporal_accessor.

:- type zone_offset.
:- instance temporal_accessor(zone_offset).

%---------------------------------------------------------------------------%

:- func max = zone_offset.

:- func min = zone_offset.

:- func utc = zone_offset.

:- func get_id(zone_offset) = string.

:- func get_total_seconds(zone_offset) = int.

:- func to_string(zone_offset) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

:- pred equals(zone_offset::in, zone_offset::in) is semidet.

:- pred compare_to(comparison_result::uo, zone_offset::in, zone_offset::in)
    is det.

:- implementation.

:- pragma foreign_type("Java", zone_offset, "java.time.ZoneOffset") where
    equality is zone_offset.equals,
    comparison is zone_offset.compare_to.

:- instance temporal_accessor(zone_offset) where [].

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    min = (ZO::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ZO = java.time.ZoneOffset.MIN;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    max = (ZO::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ZO = java.time.ZoneOffset.MAX;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    utc = (ZO::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ZO = java.time.ZoneOffset.UTC;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_id(ZO::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = ZO.getId();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_total_seconds(ZO::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = ZO.getTotalSeconds();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(ZO::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = ZO.toString();
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
:- end_module zone_offset.
%---------------------------------------------------------------------------%
