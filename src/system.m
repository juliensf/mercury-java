%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.lang.System.
%
%---------------------------------------------------------------------------%

:- module jlang.system.
:- interface.

:- import_module io.

:- pred current_time_millis(int64::out, io::di, io::uo) is det.

:- pred gc(io::di, io::uo) is det.

:- pred nano_time(int64::out, io::di, io::uo) is det.

:- pred run_finalization(io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    current_time_millis(T::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    T = java.lang.System.currentTimeMillis();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    gc(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    java.lang.System.gc();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    nano_time(T::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    T = java.lang.System.nanoTime();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    run_finalization(_IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    java.lang.System.runFinalization();
").

%---------------------------------------------------------------------------%
:- end_module system.
%---------------------------------------------------------------------------%
