%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.io.BufferedWriter.
%
%---------------------------------------------------------------------------%

:- module jio.buffered_writer.
:- interface.

:- import_module jio.writer.

:- import_module char.
:- import_module io.
:- import_module stream.

%---------------------------------------------------------------------------%

:- type buffered_writer.
:- typeclass buffered_writer(T) <= writer(T) where [].
:- instance buffered_writer(buffered_writer).
:- instance writer(buffered_writer).

:- instance stream(buffered_writer, io).
:- instance output(buffered_writer, io).

%---------------------------------------------------------------------------%

:- pred new_buffered_writer(W::in, buffered_writer::out, io::di, io::uo)
    is det <= writer(W).

:- pred new_buffered_writer(W::in, int::in, buffered_writer::out,
    io::di, io::uo) is det <= writer(W).

%---------------------------------------------------------------------------%

:- pred close(W::in, io::di, io::uo) is det <= buffered_writer(W).

:- pred flush(W::in, io::di, io::uo) is det <= buffered_writer(W).

:- pred write_char(W::in, char::in, io::di, io::uo) is det
    <= buffered_writer(W).

:- pred write_string(W::in, string::in, io::di, io::uo) is det
    <= buffered_writer(W).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", buffered_writer, "java.io.BufferedWriter").

:- instance buffered_writer(buffered_writer) where [].
:- instance writer(buffered_writer) where [].

%---------------------------------------------------------------------------%

:- instance stream(buffered_writer, io) where [
    (name(_, "<<java.io.BufferedWriter>>", !IO))
].

:- instance output(buffered_writer, io) where [
    pred(flush/3) is buffered_writer.flush
].

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    new_buffered_writer(W::in, BW::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    BW = new java.io.BufferedWriter((java.io.Writer) W);
").

%---------------------------------------------------------------------------%

new_buffered_writer(W, Size, BW, !IO) :-
    ( if Size > 0 then
        do_new_buffered_writer(W, Size, BW, !IO)
    else
        unexpected("jio.buffered_writer.new_buffered_writer", "Size =< 0")
    ).

:- pred do_new_buffered_writer(W::in, int::in, buffered_writer::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_new_buffered_writer(W::in, Size::in, BW::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    BW = new java.io.BufferedWriter((java.io.Writer) W, Size);
").

%---------------------------------------------------------------------------%

close(PW, !IO) :-
    writer.close(PW, !IO).

flush(PW, !IO) :-
    writer.flush(PW, !IO).

write_char(PW, C, !IO) :-
    writer.write_char(PW, C, !IO).

write_string(PW, S, !IO) :-
    writer.write_string(PW, S, !IO).

%---------------------------------------------------------------------------%
:- end_module jio.buffered_writer.
%---------------------------------------------------------------------------%
