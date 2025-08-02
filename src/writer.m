%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.io.Writer.
%
%---------------------------------------------------------------------------%

:- module jio.writer.
:- interface.

:- import_module char.
:- import_module io.
:- import_module stream.

%---------------------------------------------------------------------------%

:- typeclass writer(T) where [].

:- type writer.
:- instance writer(writer).

:- instance stream(writer, io).
:- instance output(writer, io).
:- instance writer(writer, char, io).
:- instance writer(writer, string, io).

%---------------------------------------------------------------------------%

:- pred close(W::in, io::di, io::uo) is det <= writer(W).

:- pred flush(W::in, io::di, io::uo) is det <= writer(W).

:- pred write_char(W::in, char::in, io::di, io::uo) is det <= writer(W).

:- pred write_string(W::in, string::in, io::di, io::uo) is det <= writer(W).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module bool.
:- import_module exception.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", writer, "java.io.Writer").

:- instance writer(writer) where [].

:- instance stream(writer, io) where [
    (name(_, "<<java.io.Writer>>", !IO))
].

:- instance output(writer, io) where [
    pred(flush/3) is writer.flush
].

:- instance writer(writer, char, io) where [
    pred(put/4) is write_char
].

:- instance writer(writer, string, io) where [
    pred(put/4) is write_string
].

%---------------------------------------------------------------------------%

close(W, !IO) :-
    do_close(W, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_close(W::in, bool::out, throwable::out, io::di, io::uo)
    is det <= writer(W).
:- pragma foreign_proc("Java",
    do_close(W::in, IsOk::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.io.Writer) W).close();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

flush(W, !IO) :-
    do_flush(W, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_flush(W::in, bool::out, throwable::out, io::di, io::uo)
    is det <= writer(W).
:- pragma foreign_proc("Java",
    do_flush(W::in, IsOk::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.io.Writer) W).flush();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

write_char(W, C, !IO) :-
    do_write_char(W, C, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_write_char(W::in, char::in, bool::out, throwable::out,
    io::di, io::uo) is det <= writer(W).
:- pragma foreign_proc("Java",
    do_write_char(W::in, C::in, IsOk::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.io.Writer) W).write(java.lang.Character.toChars(C));
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.IllegalArgumentException | java.io.IOException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

write_string(W, S, !IO) :-
    do_write_string(W, S, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_write_string(W::in, string::in, bool::out, throwable::out,
    io::di, io::uo) is det <= writer(W).
:- pragma foreign_proc("Java",
    do_write_string(W::in, S::in, IsOk::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.io.Writer) W).write(S);
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%
:- end_module jio.writer.
%---------------------------------------------------------------------------%
