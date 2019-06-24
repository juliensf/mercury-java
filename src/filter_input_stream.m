%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.io.FilterInputStream.
%
%---------------------------------------------------------------------------%

:- module jio.filter_input_stream.
:- interface.

:- import_module jio.input_stream.

:- import_module io.

%---------------------------------------------------------------------------%

:- typeclass filter_input_stream(T) <= input_stream(T) where [].

:- type filter_input_stream.

:- instance input_stream(filter_input_stream).
:- instance filter_input_stream(filter_input_stream).

%---------------------------------------------------------------------------%

:- pred available(T::in, io.result(int)::out, io::di, io::uo)
    is det <= filter_input_stream(T).

:- pred close(T::in, io::di, io::uo) is det <= filter_input_stream(T).

:- pred mark(T::in, int::in, io::di, io::uo)
    is det <= filter_input_stream(T).

:- pred mark_supported(T::in, io::ui) is semidet <= filter_input_stream(T).

:- pred read_byte(T::in, io.result(uint8)::out, io::di, io::uo)
    is det <= filter_input_stream(T).

% XXX TODO read multiple bytes.
:- pred reset(T::in, io::di, io::uo) is det <= filter_input_stream(T).

:- pred skip(T::in, int64::in, io::di, io::uo) is det
    <= filter_input_stream(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", filter_input_stream,
    "java.io.FilterInputStream").

:- instance input_stream(filter_input_stream) where [].
:- instance filter_input_stream(filter_input_stream) where [].

%---------------------------------------------------------------------------%

available(FS, Result, !IO) :-
    input_stream.available(FS, Result, !IO).

close(FS, !IO) :-
    input_stream.close(FS, !IO).

mark(FS, RL, !IO) :-
    input_stream.mark(FS, RL, !IO).

mark_supported(FS, IO) :-
    input_stream.mark_supported(FS, IO).

read_byte(FS, Result, !IO) :-
    input_stream.read_byte(FS, Result, !IO).

reset(FS, !IO) :-
    input_stream.reset(FS, !IO).

skip(FS, N, !IO) :-
    input_stream.skip(FS, N, !IO).

%---------------------------------------------------------------------------%
:- end_module jio.filter_input_stream.
%---------------------------------------------------------------------------%
