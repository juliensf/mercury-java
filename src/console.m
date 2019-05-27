%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.io.Console.
%
%---------------------------------------------------------------------------%

:- module jio.console.
:- interface.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module io.
:- import_module string.
:- import_module stream.

%---------------------------------------------------------------------------%

:- type console.

:- pred flush(console::in, io::di, io::uo) is det.

:- pred read_line(console::in, stream.result(string, throwable)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", console, "java.io.Console").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    flush(C::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    C.flush();
").

%---------------------------------------------------------------------------%

read_line(C, Result, !IO) :-
    do_read_line(C, Line, AtEof, IsOk, Error, !IO),
    (
        IsOk = yes,
        (
            AtEof = yes,
            Result = eof
        ;
            AtEof = no,
            Result = ok(Line)
        )
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_read_line(console::in, string::out, bool::out, bool::out,
    throwable::out, io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_read_line(C::in, Line::out, AtEof::out, IsOk::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        Line = C.readLine();
        AtEof = (Line == null) ? bool.YES : bool.NO;
        IsOk =  bool.YES;
        Error = null;
    } catch (java.io.IOError e) {
        AtEof = bool.NO;
        Line = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%
:- end_module jio.console.
%---------------------------------------------------------------------------%
