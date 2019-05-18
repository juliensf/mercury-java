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

:- pred close(R::in, io::di, io::uo) is det <= buffered_reader(R).

:- pred read(R::in, stream.result(char, throwable)::out,
    io::di, io::uo) is det <= buffered_reader(R).

:- pred ready(R::in, io::ui) is semidet <= buffered_reader(R).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", buffered_reader, "java.io.BufferedReader").

:- instance buffered_reader(buffered_reader) where [].
:- instance reader(buffered_reader) where [].

%---------------------------------------------------------------------------%

close(R, !IO) :-
    reader.close(R, !IO).

read(R, Result, !IO) :-
    reader.read(R, Result, !IO).

ready(R, IO) :-
    reader.ready(R, IO).

%---------------------------------------------------------------------------%
:- end_module jio.buffered_reader.
%---------------------------------------------------------------------------%
