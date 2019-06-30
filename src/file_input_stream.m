%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.io.FileInputStream.
%
%---------------------------------------------------------------------------%

:- module jio.file_input_stream.
:- interface.

%:- import_module jio.file.
:- import_module jio.file_descriptor.
:- import_module jio.input_stream.
:- import_module jlang.
:- import_module jlang.throwable.

:- import_module io.
:- import_module stream.

%---------------------------------------------------------------------------%

:- type file_input_stream.

:- instance input_stream(file_input_stream).

%---------------------------------------------------------------------------%

:- pred available(file_input_stream::in, io.result(int)::out,
    io::di, io::uo) is det.

:- pred close(file_input_stream::in, io::di, io::uo) is det.

:- pred mark(file_input_stream::in, int::in, io::di, io::uo) is det.

:- pred mark_supported(file_input_stream::in, io::ui) is semidet.

:- pred read_byte(file_input_stream::in, stream.result(uint8, throwable)::out,
    io::di, io::uo) is det.

% XXX TODO read multiple bytes.

:- pred reset(file_input_stream::in, io::di, io::uo) is det.

:- pred skip(file_input_stream::in, int64::in, io::di, io::uo) is det.

% :- pred get_channel(file_input_stream::in, file_channel::out,
%    io::di, io::uo) is det.

:- pred get_fd(file_input_stream::in, file_descriptor::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module array.
:- import_module bool.
:- import_module exception.
:- import_module require.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", file_input_stream, "java.io.FileInputStream").

%---------------------------------------------------------------------------%

:- instance input_stream(file_input_stream) where [].

%---------------------------------------------------------------------------%

available(Stream, Result, !IO) :-
    input_stream.available(Stream, Result, !IO).

mark(Stream, N, !IO) :-
    input_stream.mark(Stream, N, !IO).

close(Stream, !IO) :-
    input_stream.close(Stream, !IO).

mark_supported(Stream, IO) :-
    input_stream.mark_supported(Stream, IO).

read_byte(Stream, Result, !IO) :-
    input_stream.read_byte(Stream, Result, !IO).

reset(Stream, !IO) :-
    input_stream.reset(Stream, !IO).

skip(Stream, N, !IO) :-
    input_stream.skip(Stream, N, !IO).

%---------------------------------------------------------------------------%

get_fd(Stream, FD, !IO) :-
    do_get_fd(Stream, IsOk, FD, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_get_fd(file_input_stream::in, bool::out, file_descriptor::out,
    throwable::out, io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_get_fd(Stream::in, IsOk::out, FD::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        FD = Stream.getFD();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        FD = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%
:- end_module file_input_stream.
%---------------------------------------------------------------------------%
