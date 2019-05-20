%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.io.LineNumberReader.
%
%---------------------------------------------------------------------------%

:- module jio.line_number_reader.
:- interface.

:- import_module jio.buffered_reader.
:- import_module jio.reader.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module char.
:- import_module io.
:- import_module stream.

%---------------------------------------------------------------------------%

:- typeclass line_number_reader(T) <= buffered_reader(T) where [].

:- type line_number_reader.
:- instance line_number_reader(line_number_reader).
:- instance buffered_reader(line_number_reader).
:- instance reader(line_number_reader).

:- instance stream(line_number_reader, io).
:- instance input(line_number_reader, io).
:- instance reader(line_number_reader, char, io, throwable).
:- instance line_oriented(line_number_reader, io).

%---------------------------------------------------------------------------%

:- pred new_line_number_reader(R::in, line_number_reader::out,
    io::di, io::uo) is det <= reader(R).

%---------------------------------------------------------------------------%

:- pred close(R::in, io::di, io::uo) is det <= line_number_reader(R).

:- pred mark(R::in, int::in, io::di, io::uo) is det <= line_number_reader(R).

:- pred mark_supported(R::in, io::ui) is semidet <= line_number_reader(R).

:- func get_line_number(R::in, io::ui) = (int::out) is det
    <= line_number_reader(R).

:- pred read(R::in, stream.result(char, throwable)::out,
    io::di, io::uo) is det <= line_number_reader(R).

:- pred read_line(R::in, stream.result(string, throwable)::out,
    io::di, io::uo) is det <= line_number_reader(R).

:- pred ready(R::in, io::ui) is semidet <= line_number_reader(R).

:- pred reset(R::in, io::di, io::uo) is det <= line_number_reader(R).

:- pred set_line_number(R::in, int::in, io::di, io::uo) is det
    <= line_number_reader(R).

:- pred skip(R::in, int64::in, int64::out, io::di, io::uo)
    is det <= line_number_reader(R).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", line_number_reader,
    "java.io.LineNumberReader").

:- instance line_number_reader(line_number_reader) where [].
:- instance buffered_reader(line_number_reader) where [].
:- instance reader(line_number_reader) where [].

%---------------------------------------------------------------------------%

:- instance stream(line_number_reader, io) where [
    ( name(_Stream, Name, !IO) :-
        Name = "<<java.io.LineNumberReader>>"
    )
].

:- instance input(line_number_reader, io) where [].

:- instance reader(line_number_reader, char, io, throwable) where [
    pred(get/4) is line_number_reader.read
].

:- instance line_oriented(line_number_reader, io) where [
    ( get_line(R, N, !IO) :-
        N = get_line_number(R, !.IO)
    ),
    ( set_line(R, N, !IO) :-
        set_line_number(R, N, !IO)
    )
].

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    new_line_number_reader(R::in, LNR::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    LNR = new java.io.LineNumberReader((java.io.Reader) R);
").

%---------------------------------------------------------------------------%

close(R, !IO) :-
    reader.close(R, !IO).

mark(R, L, !IO) :-
    reader.mark(R, L, !IO).

mark_supported(R, IO) :-
    reader.mark_supported(R, IO).

read(R, Result, !IO) :-
    reader.read(R, Result, !IO).

ready(R, IO) :-
    reader.ready(R, IO).

read_line(R, Result, !IO) :-
    buffered_reader.read_line(R, Result, !IO).

reset(R, !IO) :-
    reader.reset(R, !IO).

skip(R, ToSkip, Skipped, !IO) :-
    reader.skip(R, ToSkip, Skipped, !IO).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_line_number(R::in, _IO::ui) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = ((java.io.LineNumberReader) R).getLineNumber();
").

:- pragma foreign_proc("Java",
    set_line_number(R::in, N::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ((java.io.LineNumberReader) R).setLineNumber(N);
").

%---------------------------------------------------------------------------%
:- end_module line_number_reader.
%---------------------------------------------------------------------------%
