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

:- import_module io.

:- typeclass buffered_reader(T) <= reader(T) where [].

:- type buffered_reader.
:- instance buffered_reader(buffered_reader).
:- instance reader(buffered_reader).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", buffered_reader, "java.io.BufferedReader").

:- instance buffered_reader(buffered_reader) where [].
:- instance reader(buffered_reader) where [].

%---------------------------------------------------------------------------%
:- end_module jio.buffered_reader.
%---------------------------------------------------------------------------%
