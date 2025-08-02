%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2019 Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% A Mercury wrapper for the java.nio.charset.Charset class.
%
%-----------------------------------------------------------------------------%

:- module jnio.jcharset.charset.
:- interface.

:- typeclass charset(T) where [].

:- type charset.
:- instance charset(charset).

%-----------------------------------------------------------------------------%

:- func name(CS) = string <= charset(CS).

:- func to_string(CS) = string <= charset(CS).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", charset, "java.nio.charset.Charset").

:- instance charset(charset) where [].

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    name(CS::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = ((java.nio.charset.Charset) CS).name();
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(CS::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = ((java.nio.charset.Charset) CS).toString();
").

%-----------------------------------------------------------------------------%
:- end_module jnio.jcharset.charset.
%-----------------------------------------------------------------------------%
