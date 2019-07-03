%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.nio.file.CopyOption interface.
%
%---------------------------------------------------------------------------%

:- module jnio.jfile.copy_option.
:- interface.

:- typeclass copy_option(T) where [
    func to_copy_option(T) = copy_option
].

:- type copy_option.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", copy_option, "java.nio.file.CopyOption").

%---------------------------------------------------------------------------%
:- end_module jnio.jfile.copy_option.
%---------------------------------------------------------------------------%
