%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.io.BufferedReader.
%
%---------------------------------------------------------------------------%

:- module jio.buffered_reader.
:- interface.

:- import_module jio.reader.
:- import_module jlang.
:- import_module jlang.throwable.

:- import_module char.
:- import_module io.
:- import_module stream.

%---------------------------------------------------------------------------%

:- typeclass buffered_reader(T) <= reader(T) where [].

:- type buffered_reader.
:- instance buffered_reader(buffered_reader).
:- instance reader(buffered_reader).

:- instance stream(buffered_reader, io).
:- instance input(buffered_reader, io).
:- instance reader(buffered_reader, char, io, throwable).

%---------------------------------------------------------------------------%

:- pred new_buffered_reader(R::in, buffered_reader::out, io::di, io::uo)
    is det <= reader(R).

%---------------------------------------------------------------------------%

:- pred close(R::in, io::di, io::uo) is det <= buffered_reader(R).

:- pred read(R::in, stream.result(char, throwable)::out,
    io::di, io::uo) is det <= buffered_reader(R).

:- pred read_line(R::in, stream.result(string, throwable)::out,
    io::di, io::uo) is det <= buffered_reader(R).

:- pred ready(R::in, io::ui) is semidet <= buffered_reader(R).

:- pred reset(R::in, io::di, io::uo) is det <= buffered_reader(R).

:- pred skip(R::in, int64::in, int64::out, io::di, io::uo)
    is det <= buffered_reader(R).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", buffered_reader, "java.io.BufferedReader").

:- instance buffered_reader(buffered_reader) where [].
:- instance reader(buffered_reader) where [].

:- instance stream(buffered_reader, io) where [
    ( name(_Stream, Name, !IO) :-
        Name = "<<java.io.BufferedReader>>"
    )
].

:- instance input(buffered_reader, io) where [].

:- instance reader(buffered_reader, char, io, throwable) where [
    pred(get/4) is buffered_reader.read
].

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    new_buffered_reader(R::in, BR::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    BR = new java.io.BufferedReader((java.io.Reader) R);
").

%---------------------------------------------------------------------------%

close(R, !IO) :-
    reader.close(R, !IO).

read(R, Result, !IO) :-
    reader.read(R, Result, !IO).

ready(R, IO) :-
    reader.ready(R, IO).

reset(R, !IO) :-
    reader.reset(R, !IO).

skip(R, ToSkip, Skipped, !IO) :-
    reader.skip(R, ToSkip, Skipped, !IO).

%---------------------------------------------------------------------------%

read_line(R, Result, !IO) :-
    do_read_line(R, Line, AtEof, IsOk, Error, !IO),
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

:- pred do_read_line(R::in, string::out, bool::out, bool::out,
    throwable::out, io::di, io::uo) is det <= buffered_reader(R).
:- pragma foreign_proc("Java",
    do_read_line(R::in, Line::out, AtEof::out, IsOk::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        Line = ((java.io.BufferedReader) R).readLine();
        AtEof = (Line == null) ? bool.YES : bool.NO;
        IsOk =  bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        AtEof = bool.NO;
        Line = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%
:- end_module jio.buffered_reader.
%---------------------------------------------------------------------------%
