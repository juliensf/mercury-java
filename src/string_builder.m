%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.lang.StringBuilder.
%
%---------------------------------------------------------------------------%

:- module jlang.string_builder.
:- interface.

:- import_module char.
:- import_module io.
:- import_module stream.

%---------------------------------------------------------------------------%

:- type string_builder.

:- pred new_string_builder(string_builder::out,
    io::di, io::uo) is det.

:- pred new_string_builder(string::in, string_builder::out,
    io::di, io::uo) is det.

:- instance stream(string_builder, io).
:- instance output(string_builder, io).
:- instance writer(string_builder, char, io).
:- instance writer(string_builder, string, io).

:- func to_string(string_builder::in, io::ui) = (string::out)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", string_builder, "java.lang.StringBuilder").

%---------------------------------------------------------------------------%

:- instance stream(string_builder, io) where [
    name(_, "<<string_builder>>", !IO)
].

:- instance output(string_builder, io) where [
    flush(_, !IO)
].

:- instance writer(string_builder, char, io) where [
    pred(put/4) is append_char
].

:- instance writer(string_builder, string, io) where [
    pred(put/4) is append_string
].

%---------------------------------------------------------------------------%

:- pred append_char(string_builder::in, char::in, io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    append_char(SB::in, C::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SB.appendCodePoint(C);
").

%---------------------------------------------------------------------------%

:- pred append_string(string_builder::in, string::in, io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    append_string(SB::in, S::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SB.append(S);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    new_string_builder(SB::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SB = new java.lang.StringBuilder();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    new_string_builder(S::in, SB::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SB = new java.lang.StringBuilder(S);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(SB::in, _IO::ui) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = SB.toString();
").

%---------------------------------------------------------------------------%
:- end_module string_builder.
%---------------------------------------------------------------------------%
