%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, 2025 Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.lang.System.
%
%---------------------------------------------------------------------------%

:- module jlang.system.
:- interface.

:- import_module jio.
:- import_module jio.console.

:- import_module io.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- pred console(maybe(console)::out, io::di, io::uo) is det.

:- pred current_time_millis(int64::out, io::di, io::uo) is det.

:- pred gc(io::di, io::uo) is det.

:- pred nano_time(int64::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.

%---------------------------------------------------------------------------%

console(MaybeConsole, !IO) :-
    do_get_console(Console, HaveConsole, !IO),
    (
        HaveConsole = yes,
        MaybeConsole = yes(Console)
    ;
        HaveConsole = no,
        MaybeConsole = no
    ).

:- pred do_get_console(console::out, bool::out, io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_get_console(C::out, HaveC::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    C = java.lang.System.console();
    HaveC = (C != null) ? bool.YES : bool.NO;
").

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
:- end_module system.
%---------------------------------------------------------------------------%
