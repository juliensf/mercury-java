%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.io.InputStreamReader.
%
%---------------------------------------------------------------------------%

:- module jio.input_stream_reader.
:- interface.

:- import_module jio.input_stream.
:- import_module jio.reader.
:- import_module jlang.
:- import_module jlang.throwable.
:- import_module jnio.
:- import_module jnio.jcharset.
:- import_module jnio.jcharset.charset.

:- import_module char.
:- import_module io.
:- import_module stream.

%---------------------------------------------------------------------------%

:- typeclass input_stream_reader(R) <= reader(R) where [].

:- type input_stream_reader.
:- instance input_stream_reader(input_stream_reader).
:- instance reader(input_stream_reader).

:- instance stream(input_stream_reader, io).
:- instance input(input_stream_reader, io).
:- instance reader(input_stream_reader, char, io, throwable).

%---------------------------------------------------------------------------%

:- pred new_input_stream_reader(IS::in, input_stream_reader::out,
    io::di, io::uo) is det <= input_stream(IS).

:- pred new_input_stream_reader(IS::in, CS::in, input_stream_reader::out,
    io::di, io::uo) is det <= (input_stream(IS), charset(CS)).

%---------------------------------------------------------------------------%

:- pred close(R::in, io::di, io::uo) is det <= input_stream_reader(R).

%:- pred mark(R::in, int::in, io::di, io::uo) is det <= input_stream_reader(R)

%:- pred mark_supported(R::in, io::ui) is semidet <= input_stream_reader(R).

:- pred read(R::in, stream.result(char, throwable)::out,
    io::di, io::uo) is det <= input_stream_reader(R).

:- pred ready(R::in, io::ui) is semidet <= input_stream_reader(R).

:- pred reset(R::in, io::di, io::uo) is det <= input_stream_reader(R).

:- pred skip(R::in, int64::in, int64::out, io::di, io::uo)
    is det <= input_stream_reader(R).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", input_stream_reader,
    "java.io.InputStreamReader").

:- instance input_stream_reader(input_stream_reader) where [].
:- instance reader(input_stream_reader) where [].

:- instance stream(input_stream_reader, io) where [
    ( name(_Stream, Name, !IO) :-
        Name = "<<java.io.InputStreamReader>>"
    )
].

:- instance input(input_stream_reader, io) where [].

:- instance reader(input_stream_reader, char, io, throwable) where [
    pred(get/4) is input_stream_reader.read
].

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    new_input_stream_reader(IS::in, ISR::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ISR = new java.io.InputStreamReader((java.io.InputStream) IS);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    new_input_stream_reader(IS::in, CS::in, ISR::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ISR = new java.io.InputStreamReader((java.io.InputStream) IS,
        (java.nio.charset.Charset) CS);
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
:- end_module input_stream_reader.
%---------------------------------------------------------------------------%
