%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.time.Month.
%
%---------------------------------------------------------------------------%

:- module jtime.month.
:- interface.

:- import_module calendar.

%---------------------------------------------------------------------------%

:- type jmonth.

    % Conversion to and from the Mercury standard library's calander.month/0
    % type.
    %
:- func from_jmonth(jmonth) = month.
:- func to_jmonth(month) = jmonth.

%---------------------------------------------------------------------------%

:- func get_value(jmonth) = int.

:- func of(int) = jmonth.

:- func to_string(jmonth) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

:- pred equals(jmonth::in, jmonth::in) is semidet.

:- pred compare_to(comparison_result::uo, jmonth::in, jmonth::in) is det.

:- implementation.

:- import_module int.
:- import_module require.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", jmonth, "java.time.Month") where
    equality is month.equals,
    comparison is month.compare_to.

%---------------------------------------------------------------------------%

from_jmonth(JMonth) = Month :-
    I = get_value(JMonth),
    Month = calendar.det_int_to_month(I).

to_jmonth(Month) = JMonth :-
    I = calendar.month_to_int(Month),
    do_of(I, JMonth).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_value(M::in) = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = M.getValue();
").

%---------------------------------------------------------------------------%

of(I) = Month :-
    ( if I > 0, I < 13 then
        do_of(I, Month)
    else
        unexpected("jtime.month.of", "int is not in 1..12")
    ).

:- pred do_of(int::in, jmonth::out) is det.
:- pragma foreign_proc("Java",
    do_of(I::in, M::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    M = java.time.Month.of(I);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(M::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = M.toString();
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
:- end_module jtime.month.
%---------------------------------------------------------------------------%
