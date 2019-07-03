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
    func to_open_option(T) = open_option
].

:- type open_option.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", open_option, "java.nio.file.OpenOption").

%---------------------------------------------------------------------------%
:- end_module jnio.jfile.open_option.
%---------------------------------------------------------------------------%
