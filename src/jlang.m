%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% A Mercury wrapper for the java.lang package.
%
%-----------------------------------------------------------------------------%

:- module jlang.
:- interface.

:- include_module character.
:- include_module object.
:- include_module stack_trace_element.
:- include_module string_builder.
:- include_module system.
:- include_module throwable.

%-----------------------------------------------------------------------------%
:- end_module jlang.
%-----------------------------------------------------------------------------%
