%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.io.FileOutputStream.
%
%---------------------------------------------------------------------------%

:- module jio.file_output_stream.
:- interface.

:- import_module jio.file_descriptor.
:- import_module jio.output_stream.

:- import_module io.
:- import_module stream.

%---------------------------------------------------------------------------%

:- typeclass file_output_stream(T) <= output_stream(T) where [].

:- type file_output_stream.
:- instance output_stream(file_output_stream).
:- instance file_output_stream(file_output_stream).

:- instance stream(file_output_stream, io).
:- instance output(file_output_stream, io).
:- instance writer(file_output_stream, uint8, io).

%---------------------------------------------------------------------------%

:- pred close(T::in, io::di, io::uo) is det <= file_output_stream(T).

:- pred flush(T::in, io::di, io::uo) is det <= file_output_stream(T).

:- pred get_fd(T::in, file_descriptor::out,
    io::di, io::uo) is det <= file_output_stream(T).

:- pred write_byte(T::in, uint8::in, io::di, io::uo)
    is det <= file_output_stream(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module bool.
:- import_module exception.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", file_output_stream,
    "java.io.FileOutputStream").

:- instance output_stream(file_output_stream) where [].
:- instance file_output_stream(file_output_stream) where [].

:- instance stream(file_output_stream, io) where [
    pred(name/4) is file_output_stream.name
].

:- instance output(file_output_stream, io) where [
    pred(flush/3) is file_output_stream.flush
].

:- instance writer(file_output_stream, uint8, io) where [
    pred(put/4) is file_output_stream.write_byte
].

%---------------------------------------------------------------------------%

close(Stream, !IO) :-
    output_stream.close(Stream, !IO).

flush(Stream, !IO) :-
    output_stream.flush(Stream, !IO).

write_byte(Stream, Byte, !IO) :-
    output_stream.write_byte(Stream, Byte, !IO).

%---------------------------------------------------------------------------%

get_fd(Stream, FD, !IO) :-
    do_get_fd(Stream, IsOk, FD, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_get_fd(T::in, bool::out, file_descriptor::out,
    throwable::out, io::di, io::uo) is det <= file_output_stream(T).
:- pragma foreign_proc("Java",
    do_get_fd(Stream::in, IsOk::out, FD::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        FD = ((java.io.FileOutputStream) Stream).getFD();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        FD = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pred name(file_output_stream::in, string::out, io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    name(Stream::in, Name::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = Stream.toString();
").

%---------------------------------------------------------------------------%
:- end_module file_output_stream.
%---------------------------------------------------------------------------%
