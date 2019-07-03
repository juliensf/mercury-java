%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.nio.file.OpenOption interface.
%
%---------------------------------------------------------------------------%

:- module jnio.jfile.open_option.
:- interface.

:- typeclass open_option(T) where [
    func to_jopen_option(T) = jopen_option
].

:- type open_option
    --->    some [O] open_option(O) => open_option(O).

:- type jopen_option.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", jopen_option, "java.nio.file.OpenOption").

%---------------------------------------------------------------------------%
:- end_module jnio.jfile.open_option.
%---------------------------------------------------------------------------%
