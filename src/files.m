%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.nio.file.Files.
%
%---------------------------------------------------------------------------%

:- module jnio.jfile.files.
:- interface.

:- import_module jio.
:- import_module jio.buffered_reader.
:- import_module jlang.
:- import_module jlang.throwable.
:- import_module jnio.jcharset.
:- import_module jnio.jcharset.charset.
:- import_module jnio.jfile.path.

:- import_module io.
:- import_module stream.

%---------------------------------------------------------------------------%

:- pred new_buffered_reader(P::in,
    stream.result(buffered_reader, throwable)::out,
    io::di, io::uo) is det <= path(P).

:- pred new_buffered_reader(P::in, C::in,
    stream.result(buffered_reader, throwable)::out,
    io::di, io::uo) is det <= (path(P), charset(C)).

:- pred size(P::in, stream.result(int64, throwable)::out,
    io::di, io::uo) is det <= path(P).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.

%---------------------------------------------------------------------------%

new_buffered_reader(Path, Result, !IO) :-
    do_new_buffered_reader(Path, IsOk, Reader, Error, !IO),
    (
        IsOk = yes,
        Result = ok(Reader)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_new_buffered_reader(P::in, bool::out, buffered_reader::out,
    throwable::out, io::di, io::uo) is det <= path(P).
:- pragma foreign_proc("Java",
    do_new_buffered_reader(P::in, IsOk::out, BR::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        BR = java.nio.file.Files.newBufferedReader((java.nio.file.Path) P);
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException | java.lang.SecurityException e) {
        IsOk = bool.NO;
        BR = null;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

new_buffered_reader(Path, Charset, Result, !IO) :-
    do_new_buffered_reader(Path, Charset, IsOk, Reader, Error, !IO),
    (
        IsOk = yes,
        Result = ok(Reader)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_new_buffered_reader(P::in, C::in, bool::out, buffered_reader::out,
    throwable::out, io::di, io::uo) is det <= (path(P), charset(C)).
:- pragma foreign_proc("Java",
    do_new_buffered_reader(P::in, C::in, IsOk::out, BR::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        BR = java.nio.file.Files.newBufferedReader(
            (java.nio.file.Path) P,
            (java.nio.charset.Charset) C);
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException | java.lang.SecurityException e) {
        IsOk = bool.NO;
        BR = null;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

size(Path, Result, !IO) :-
    do_size(Path, IsOk, Size, Error, !IO),
    (
        IsOk = yes,
        Result = ok(Size)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_size(P::in, bool::out, int64::out, throwable::out,
    io::di, io::uo) is det <= path(P).
:- pragma foreign_proc("Java",
    do_size(P::in, IsOk::out, Size::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        Size = java.nio.file.Files.size((java.nio.file.Path) P);
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException | java.lang.SecurityException e) {
        Size = 0L;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%
:- end_module jnio.jfile.files.
%---------------------------------------------------------------------------%
