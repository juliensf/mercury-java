%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 2025 Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.io.InputStream.
%
%---------------------------------------------------------------------------%

:- module jio.input_stream.
:- interface.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module io.
:- import_module stream.

%---------------------------------------------------------------------------%

:- typeclass input_stream(T) where [].

    % For manipulating values of type java.io.InputStream directly.
    %
    % NOTE: we prefix this type with 'j' to avoid the name clash with Mercury's
    % standard library.
    %
:- type jinput_stream.

:- instance input_stream(jinput_stream).

:- instance stream(jinput_stream, io).
:- instance input(jinput_stream, io).
:- instance reader(jinput_stream, uint8, io, throwable).

%---------------------------------------------------------------------------%

:- pred available(T::in, io.result(int)::out, io::di, io::uo)
    is det <= input_stream(T).

:- pred close(T::in, io::di, io::uo) is det <= input_stream(T).

:- pred mark(T::in, int::in, io::di, io::uo) is det <= input_stream(T).

:- pred mark_supported(T::in, io::ui) is semidet <= input_stream(T).

:- pred read_byte(T::in, stream.result(uint8, throwable)::out,
    io::di, io::uo) is det <= input_stream(T).

% XXX TODO read multiple bytes.
:- pred reset(T::in, io::di, io::uo) is det <= input_stream(T).

:- pred skip(T::in, int64::in, io::di, io::uo) is det <= input_stream(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module require.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", jinput_stream, "java.io.InputStream").

%---------------------------------------------------------------------------%

:- instance input_stream(jinput_stream) where [].

:- instance stream(jinput_stream, io) where [
    ( name(_, "<<java.io.InputStream>>", !IO) )
].

:- instance input(jinput_stream, io) where [].

:- instance reader(jinput_stream, uint8, io, throwable) where [
    pred(get/4) is read_byte
].

%---------------------------------------------------------------------------%

:- pragma no_determinism_warning(available/4).

available(_, _, _, _) :-
    error("NYI available for InputStream").

:- pragma no_determinism_warning(mark/4).

mark(_, _, _, _) :-
    error("NYI mark for InputStream").

%---------------------------------------------------------------------------%

close(Stream, !IO) :-
    do_input_stream_close(Stream, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_input_stream_close(T::in, bool::out,
    throwable::out, io::di, io::uo) is det <= input_stream(T).
:- pragma foreign_proc("Java",
    do_input_stream_close(Stream::in, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.io.InputStream) Stream).close();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    mark_supported(Stream::in, _IO::ui),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((java.io.InputStream) Stream).markSupported();
").

%---------------------------------------------------------------------------%

:- pragma no_determinism_warning(read_byte/4).

read_byte(_, _, _, _) :-
    error("NYI read (byte) for InputStream").

:- pragma no_determinism_warning(reset/3).

reset(_, _, _) :-
    error("NYI reset for InputStream").

:- pragma no_determinism_warning(skip/4).

skip(_, _, _, _) :-
    error("NYI skip for InputStream").

%---------------------------------------------------------------------------%
:- end_module input_stream.
%---------------------------------------------------------------------------%

