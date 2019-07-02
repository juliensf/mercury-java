%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% A Mercury wrapper for the java.nio.file.Path interface.
%
%-----------------------------------------------------------------------------%

:- module jnio.jfile.path.
:- interface.

:- typeclass path(T) where [].
:- type path.
:- instance path(path).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", path, "java.nio.file.Path").

:- instance path(path) where [].

%-----------------------------------------------------------------------------%
:- end_module jnio.jfile.path.
%-----------------------------------------------------------------------------%
