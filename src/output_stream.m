%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.io.OutputStream.
%
%---------------------------------------------------------------------------%

:- module jio.output_stream.
:- interface.

:- import_module bitmap.
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
:- instance writer(joutput_stream, uint8, io).

%---------------------------------------------------------------------------%

:- pred close(T::in, io::di, io::uo) is det <= output_stream(T).

:- pred flush(T::in, io::di, io::uo) is det <= output_stream(T).

    % Write a single byte to the output stream.
    %
:- pred write_byte(T::in, uint8::in, io::di, io::uo) is det
    <= output_stream(T).

    % Write the bytes in the bitmap to the output stream.
    % Throws an exception if the bitmap contains a partial final byte.
    %
:- pred write_bytes(T::in, bitmap::in, io::di, io::uo) is det
    <= output_stream(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module bool.
:- import_module exception.
:- import_module require.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", joutput_stream, "java.io.OutputStream").

:- pragma foreign_decl("Java", "
    import jmercury.runtime.MercuryBitmap;
").

%---------------------------------------------------------------------------%

:- instance output_stream(joutput_stream) where [].

:- instance stream(joutput_stream, io) where [
    pred(name/4) is output_stream.name
].

:- instance output(joutput_stream, io) where [
    pred(flush/3) is output_stream.flush
].

:- instance writer(joutput_stream, uint8, io) where [
    pred(put/4) is output_stream.write_byte
].

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

write_byte(Stream, Byte, !IO) :-
    do_write_byte(Stream, Byte, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_write_byte(T::in, uint8::in, bool::out, throwable::out,
    io::di, io::uo) is det <= output_stream(T).
:- pragma foreign_proc("Java",
    do_write_byte(Stream::in, Byte::in, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.io.OutputStream) Stream).write(Byte & 0xff);
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

write_bytes(Stream, Bitmap, !IO) :-
    ( if _NumBytes : int = num_bytes(Bitmap) then
        do_write_bytes(Stream, Bitmap, IsOk, Error, !IO),
        (
            IsOk = yes
        ;
            IsOk = no,
            throw(java_exception(Error))
        )
    else
        unexpected("jio.output_stream.write_bytes", "parital final byte")
    ).

:- pred do_write_bytes(T::in, bitmap::in, bool::out, throwable::out,
    io::di, io::uo) is det <= output_stream(T).
:- pragma foreign_proc("Java",
    do_write_bytes(Stream::in, Bitmap::in, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.io.OutputStream) Stream).write(Bitmap.elements);
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

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
