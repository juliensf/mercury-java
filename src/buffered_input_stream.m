%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.io.BufferedInputStream.
%
%---------------------------------------------------------------------------%

:- module jio.buffered_input_stream.
:- interface.

:- import_module jio.filter_input_stream.
:- import_module jio.input_stream.
:- import_module jlang.
:- import_module jlang.throwable.

:- import_module io.
:- import_module stream.

%---------------------------------------------------------------------------%

:- typeclass buffered_input_stream(T) <= filter_input_stream(T) where [].

:- type buffered_input_stream.

:- instance input_stream(buffered_input_stream).
:- instance filter_input_stream(buffered_input_stream).
:- instance buffered_input_stream(buffered_input_stream).

:- instance stream(buffered_input_stream, io).
:- instance input(buffered_input_stream, io).
:- instance reader(buffered_input_stream, uint8, io, throwable).

%---------------------------------------------------------------------------%

:- pred new_buffered_input_stream(S::in, buffered_input_stream::out,
    io::di, io::uo) is det <= input_stream(S).

:- pred new_buffered_input_stream(S::in, int::in, buffered_input_stream::out,
    io::di, io::uo) is det <= input_stream(S).

%---------------------------------------------------------------------------%

:- pred available(T::in, io.result(int)::out, io::di, io::uo)
    is det <= buffered_input_stream(T).

:- pred close(T::in, io::di, io::uo) is det <= buffered_input_stream(T).

:- pred mark(T::in, int::in, io::di, io::uo)
    is det <= buffered_input_stream(T).

:- pred mark_supported(T::in, io::ui) is semidet <= buffered_input_stream(T).

:- pred read_byte(T::in, stream.result(uint8, throwable)::out, io::di, io::uo)
    is det <= buffered_input_stream(T).

% XXX TODO read multiple bytes.
:- pred reset(T::in, io::di, io::uo) is det <= buffered_input_stream(T).

:- pred skip(T::in, int64::in, io::di, io::uo) is det
    <= buffered_input_stream(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module require.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", buffered_input_stream,
    "java.io.BufferedInputStream").

:- instance input_stream(buffered_input_stream) where [].
:- instance filter_input_stream(buffered_input_stream) where [].
:- instance buffered_input_stream(buffered_input_stream) where [].

:- instance stream(buffered_input_stream, io) where [
    ( name(_, "<<java.io.BufferedInputStream>>", !IO) )
].

:- instance input(buffered_input_stream, io) where [].

:- instance reader(buffered_input_stream, uint8, io, throwable) where [
    pred(get/4) is input_stream.read_byte
].
%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    new_buffered_input_stream(S::in, BIS::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    BIS = new java.io.BufferedInputStream((java.io.InputStream) S);
").

new_buffered_input_stream(S, N, BIS, !IO) :-
    ( if N > 0 then
        do_new_buffered_input_stream(S, N, BIS, !IO)
    else
        unexpected("jio.buffered_input_stream.new_bufferd_input_stream",
            "N =< 0")
    ).

:- pred do_new_buffered_input_stream(S::in, int::in,
    buffered_input_stream::out, io::di, io::uo) is det <= input_stream(S).

:- pragma foreign_proc("Java",
    do_new_buffered_input_stream(S::in, N::in, BIS::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    BIS = new java.io.BufferedInputStream((java.io.InputStream) S, N);
").

%---------------------------------------------------------------------------%

available(BIS, Result, !IO) :-
    input_stream.available(BIS, Result, !IO).

close(BIS, !IO) :-
    input_stream.close(BIS, !IO).

mark(BIS, RL, !IO) :-
    input_stream.mark(BIS, RL, !IO).

mark_supported(BIS, IO) :-
    input_stream.mark_supported(BIS, IO).

read_byte(BIS, Result, !IO) :-
    input_stream.read_byte(BIS, Result, !IO).

reset(BIS, !IO) :-
    input_stream.reset(BIS, !IO).

skip(BIS, N, !IO) :-
    input_stream.skip(BIS, N, !IO).

%---------------------------------------------------------------------------%
:- end_module jio.buffered_input_stream.
%---------------------------------------------------------------------------%
