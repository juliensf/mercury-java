%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.io.Reader.
%
%---------------------------------------------------------------------------%

:- module jio.reader.
:- interface.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module char.
:- import_module io.
:- import_module stream.

%---------------------------------------------------------------------------%

:- typeclass reader(T) where [].

:- type reader.
:- instance reader(reader).

:- pred close(R::in, io::di, io::uo) is det <= reader(R).

%:- pred mark(R::in, int::in, io::di, io::uo) is det <= reader(R)

%:- pred mark_supported(R::in, io::ui) is semidet <= reader(R).

:- pred read(R::in, stream.result(char, throwable)::out,
    io::di, io::uo) is det <= reader(R).

:- pred ready(R::in, io::ui) is semidet <= reader(R).

%:- pred reset(R::in, io::di, io::uo) is det <= reader(R).

%:- pred skip(R::in, int64::int, io::di, io::uo) is det <= reader(R).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module bool.
:- import_module exception.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", reader, "java.io.Reader").

:- instance reader(reader) where [].

%---------------------------------------------------------------------------%

close(R, !IO) :-
    do_close(R, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_close(R::in, bool::out, throwable::out, io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_close(R::in, IsOk::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.io.Reader) R).close();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

read(R, Result, !IO) :-
    do_read(R, CharCode, IsOk, Error, !IO),
    (
        IsOk = yes,
        ( if CharCode = -1 then
            Result = eof
        else
            Char = char.det_from_int(CharCode),
            Result = ok(Char)
        )
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_read(R::in, int::out, bool::out, throwable::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_read(R::in, CharCode::out, IsOk::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        int c1 = ((java.io.Reader) R).read();
        if (c1 == -1 || !java.lang.Character.isHighSurrogate((char) c1)) {
            IsOk = bool.YES;
            Error = null;
            CharCode = c1;
        } else {
            int c2 = ((java.io.Reader) R).read();
            if (c2 != -1 && !java.lang.Character.isLowSurrogate((char) c2)) {
                CharCode = 0xfffd;
            } else {
                CharCode = Character.toCodePoint((char) c1, (char) c2);
            }
            IsOk = bool.YES;
            Error = null;
        }
    } catch (java.io.IOException e) {
        CharCode = 0;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

ready(R, IO) :-
    do_ready(R, IsReady, IsOk, Error, IO),
    (
        IsOk = yes,
        (
            IsReady = no,
            false
        ;
            IsReady = yes
        )
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_ready(R::in, bool::out, bool::out, throwable::out, io::ui) is det.
:- pragma foreign_proc("Java",
    do_ready(R::in, IsReady::out, IsOk::out, Error::out, _IO::ui),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        IsReady = ((java.io.Reader) R).ready() ? bool.YES : bool.NO;
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IsOk = bool.NO;
        IsReady = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%
:- end_module reader.
%---------------------------------------------------------------------------%
