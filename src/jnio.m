%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2019 Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% A Mercury wrapper for the java.nio package.
%
%-----------------------------------------------------------------------------%

:- module jnio.
:- interface.

:- include_module channels.
:- include_module jcharset.  % Prefix with 'j' to avoid clash with Charset.
:- include_module jfile.     % Prefix with 'j' to avoid clash with File.

%-----------------------------------------------------------------------------%
:- end_module jnio.
%-----------------------------------------------------------------------------%
