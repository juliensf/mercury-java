%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.time.Instant.
%
%---------------------------------------------------------------------------%

:- module jtime.instant.
:- interface.

:- import_module io.

:- type instant.

%---------------------------------------------------------------------------%

:- func epoch = instant.

:- func max = instant.

:- func min = instant.

:- func get_epoch_second(instant) = int64.

:- func get_nano(instant) = int.

:- pred is_after(instant::in, instant::in) is semidet.

:- pred is_before(instant::in, instant::in) is semidet.

:- pred now(instant::out, io::di, io::uo) is det.

:- func to_epoch_milli(instant) = int64.

:- func to_string(instant) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

:- pred equals(instant::in, instant::in) is semidet.

:- pred compare_to(comparison_result::uo, instant::in, instant::in) is det.

:- implementation.

:- pragma foreign_type("Java", instant, "java.time.Instant") where
    equality is instant.equals,
    comparison is instant.compare_to.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    epoch = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = java.time.Instant.EPOCH;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    min = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = java.time.Instant.MIN;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    max = (I::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = java.time.Instant.MAX;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_epoch_second(I::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = I.getEpochSecond();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_nano(I::in) = (NS::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    NS = I.getNano();
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
    now(I::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    I = java.time.Instant.now();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_epoch_milli(I::in) = (MS::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    MS = I.toEpochMilli();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(I::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = I.toString();
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
:- end_module instant.
%---------------------------------------------------------------------------%
