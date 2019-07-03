%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% A Mercury wrapper for the java.nio.file package.
%
% NOTE: we prefix the name of this module with 'j' in order to avoid the
% name clash with the wrapper for the File class.
%
%-----------------------------------------------------------------------------%

:- module jnio.jfile.
:- interface.

%-----------------------------------------------------------------------------%

:- include_module copy_option.
:- include_module files.
:- include_module link_option.
:- include_module open_option.
:- include_module path.
:- include_module standard_copy_option.
:- include_module standard_open_option.

%-----------------------------------------------------------------------------%
:- end_module jnio.jfile.
%-----------------------------------------------------------------------------%
