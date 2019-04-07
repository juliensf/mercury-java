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

:- import_module io.

%---------------------------------------------------------------------------%

:- type file_input_stream.

:- instance input_stream(file_input_stream).

% :- pred get_channel(file_input_stream::in, file_channel::out,
%    io::di, io::uo) is det.

:- pred get_fd(file_input_stream::in, file_descriptor::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module bool.
:- import_module exception.
:- import_module require.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", file_input_stream, "java.io.FileInputStream").

%---------------------------------------------------------------------------%

:- instance input_stream(file_input_stream) where [
    pred(available/4) is file_input_stream_available,
    pred(close/3) is file_input_stream_close,
    pred(mark/4) is file_input_stream_mark,
    pred(mark_supported/2) is file_input_stream_mark_supported,
    pred(read/4) is file_input_stream_read_byte,
    pred(reset/3) is file_input_stream_reset,
    pred(skip/4) is file_input_stream_skip
].

%---------------------------------------------------------------------------%

:- pragma no_determinism_warning(file_input_stream_available/4).
:- pred file_input_stream_available(file_input_stream::in,
    io.result(int)::out, io::di, io::uo) is det.

file_input_stream_available(_, _, _, _) :-
    error("NYI available for FileInputStream").

:- pragma no_determinism_warning(file_input_stream_mark/4).
:- pred file_input_stream_mark(file_input_stream::in, int::in,
    io::di, io::uo) is det.

file_input_stream_mark(_, _, _, _) :-
    error("NYI mark for FileInputStream").

%---------------------------------------------------------------------------%

:- pred file_input_stream_close(file_input_stream::in, io::di, io::uo) is det.
file_input_stream_close(Stream, !IO) :-
    do_file_input_stream_close(Stream, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_file_input_stream_close(file_input_stream::in, bool::out,
    throwable::out, io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_file_input_stream_close(Stream::in, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        Stream.close();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pred file_input_stream_mark_supported(file_input_stream::in,
    io::ui) is semidet.
:- pragma foreign_proc("Java",
    file_input_stream_mark_supported(Stream::in, _IO::ui),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = Stream.markSupported();
").

%---------------------------------------------------------------------------%

:- pragma no_determinism_warning(file_input_stream_read_byte/4).
:- pred file_input_stream_read_byte(file_input_stream::in,
    io.result(uint)::out, io::di, io::uo) is det.

file_input_stream_read_byte(_, _, _, _) :-
    error("NYI read for FileInputStream").

:- pragma no_determinism_warning(file_input_stream_reset/3).
:- pred file_input_stream_reset(file_input_stream::in,
    io::di, io::uo) is det.

file_input_stream_reset(_, _, _) :-
    error("NYI reset for FileInputStream").

:- pragma no_determinism_warning(file_input_stream_skip/4).
:- pred file_input_stream_skip(file_input_stream::in, int64::in,
    io::di, io::uo) is det.

file_input_stream_skip(_, _, _, _) :-
    error("NYI skip for FileInputStream").

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
