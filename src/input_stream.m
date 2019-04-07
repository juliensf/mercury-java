%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.io.InputStream.
%
%---------------------------------------------------------------------------%

:- module jio.input_stream.
:- interface.

:- import_module io.

%---------------------------------------------------------------------------%

    % Subclasses of InputStream are instances of this type class.
    %
:- typeclass input_stream(T) where [
    pred available(T::in, io.result(int)::out, io::di, io::uo) is det,
    pred close(T::in, io::di, io::uo) is det,
    pred mark(T::in, int::in, io::di, io::uo) is det,
    pred mark_supported(T::in, io::ui) is semidet,
    pred read(T::in, io.result(uint)::out, io::di, io::uo) is det,
    % TODO: read(byte[] b)
    % TODO: read(byte[] b, int off, int len
    pred reset(T::in, io::di, io::uo) is det,
    pred skip(T::in, int64::in, io::di, io::uo) is det
].

%---------------------------------------------------------------------------%

    % For manipulating values of type java.io.InputStream directly.
    %
    % NOTE: we prefix this type with 'j' to avoid the name clash with Mercury's
    % standard library.
    %
:- type jinput_stream.

:- instance input_stream(jinput_stream).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module bool.
:- import_module exception.
:- import_module require.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", jinput_stream, "java.io.InputStream").

%---------------------------------------------------------------------------%

:- instance input_stream(jinput_stream) where [
    pred(available/4) is input_stream_available,
    pred(close/3) is input_stream_close,
    pred(mark/4) is input_stream_mark,
    pred(mark_supported/2) is input_stream_mark_supported,
    pred(read/4) is input_stream_read_byte,
    pred(reset/3) is input_stream_reset,
    pred(skip/4) is input_stream_skip
].

%---------------------------------------------------------------------------%

:- pragma no_determinism_warning(input_stream_available/4).
:- pred input_stream_available(jinput_stream::in,
    io.result(int)::out, io::di, io::uo) is det.

input_stream_available(_, _, _, _) :-
    error("NYI available for InputStream").

:- pragma no_determinism_warning(input_stream_mark/4).
:- pred input_stream_mark(jinput_stream::in, int::in,
    io::di, io::uo) is det.

input_stream_mark(_, _, _, _) :-
    error("NYI mark for InputStream").

%---------------------------------------------------------------------------%

:- pred input_stream_close(jinput_stream::in, io::di, io::uo) is det.
input_stream_close(Stream, !IO) :-
    do_input_stream_close(Stream, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_input_stream_close(jinput_stream::in, bool::out,
    throwable::out, io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_input_stream_close(Stream::in, IsOk::out, Error::out,
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

:- pred input_stream_mark_supported(jinput_stream::in,
    io::ui) is semidet.
:- pragma foreign_proc("Java",
    input_stream_mark_supported(Stream::in, _IO::ui),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = Stream.markSupported();
").

%---------------------------------------------------------------------------%

:- pragma no_determinism_warning(input_stream_read_byte/4).
:- pred input_stream_read_byte(jinput_stream::in,
    io.result(uint)::out, io::di, io::uo) is det.

input_stream_read_byte(_, _, _, _) :-
    error("NYI read for InputStream").

:- pragma no_determinism_warning(input_stream_reset/3).
:- pred input_stream_reset(jinput_stream::in,
    io::di, io::uo) is det.

input_stream_reset(_, _, _) :-
    error("NYI reset for InputStream").

:- pragma no_determinism_warning(input_stream_skip/4).
:- pred input_stream_skip(jinput_stream::in, int64::in,
    io::di, io::uo) is det.

input_stream_skip(_, _, _, _) :-
    error("NYI skip for InputStream").

%---------------------------------------------------------------------------%
:- end_module input_stream.
%---------------------------------------------------------------------------%

