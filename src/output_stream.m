%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.io.OutputStream.
%
%---------------------------------------------------------------------------%

:- module jio.output_stream.
:- interface.

:- import_module io.
:- import_module stream.

%---------------------------------------------------------------------------%

:- typeclass output_stream(T) where [].

:- type joutput_stream.

    % For manipulating values of type java.io.OutputStream directly.
    %
    % NOTE: we prefix this type with 'j' to avoid the name clash with Mercury's
    % standard library.
    %
:- instance output_stream(joutput_stream).

:- instance stream(joutput_stream, io).
:- instance output(joutput_stream, io).
:- instance writer(joutput_stream, uint, io).

%---------------------------------------------------------------------------%

:- pred close(T::in, io::di, io::uo) is det <= output_stream(T).

:- pred flush(T::in, io::di, io::uo) is det <= output_stream(T).

:- pred write(T::in, uint::in, io::di, io::uo) is det <= output_stream(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module bool.
:- import_module exception.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", joutput_stream, "java.io.OutputStream").

:- instance output_stream(joutput_stream) where [].

%---------------------------------------------------------------------------%

close(Stream, !IO) :-
    do_close(Stream, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_close(T::in, bool::out, throwable::out, io::di, io::uo)
    is det <= output_stream(T).
:- pragma foreign_proc("Java",
    do_close(Stream::in, IsOk::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.io.OutputStream) Stream).close();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

flush(Stream, !IO) :-
    do_flush(Stream, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_flush(T::in, bool::out, throwable::out, io::di, io::uo)
    is det <= output_stream(T).
:- pragma foreign_proc("Java",
    do_flush(Stream::in, IsOk::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.io.OutputStream) Stream).flush();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

write(Stream, Byte, !IO) :-
    do_write_byte(Stream, Byte, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_write_byte(T::in, uint::in, bool::out, throwable::out,
    io::di, io::uo) is det <= output_stream(T).
:- pragma foreign_proc("Java",
    do_write_byte(Stream::in, Byte::in, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.io.OutputStream) Stream).write(Byte);
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- instance stream(joutput_stream, io) where [
    pred(name/4) is output_stream.name
].

:- instance output(joutput_stream, io) where [
    ( flush(Stream, !IO) :-
        output_stream.flush(Stream, !IO)
    )
].

:- instance writer(joutput_stream, uint, io) where [
    ( put(Stream, Byte, !IO) :-
        output_stream.write(Stream, Byte, !IO)
    )
].

%---------------------------------------------------------------------------%

:- pred name(joutput_stream::in, string::out, io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    name(Stream::in, Name::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = Stream.toString();
").

%---------------------------------------------------------------------------%
:- end_module output_stream.
%---------------------------------------------------------------------------%
