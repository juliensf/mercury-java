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
:- instance writer(string_builder, float, io).
:- instance writer(string_builder, int, io).
:- instance writer(string_builder, int8, io).
:- instance writer(string_builder, int16, io).
:- instance writer(string_builder, int32, io).
:- instance writer(string_builder, int64, io).
:- instance writer(string_builder, string, io).
:- instance writer(string_builder, uint, io).
:- instance writer(string_builder, uint8, io).
:- instance writer(string_builder, uint16, io).
:- instance writer(string_builder, uint32, io).
:- instance writer(string_builder, uint64, io).

:- func to_string(string_builder::in, io::ui) = (string::out)
    is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int8.
:- import_module int16.
:- import_module int32.
:- import_module int64.
:- import_module string.
:- import_module uint.
:- import_module uint8.
:- import_module uint16.
:- import_module uint32.
:- import_module uint64.

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

:- instance writer(string_builder, float, io) where [
    pred(put/4) is append_float
].

:- instance writer(string_builder, int, io) where [
    pred(put/4) is append_int
].

:- instance writer(string_builder, int8, io) where [
    ( put(SB, I8, !IO) :-
        I = to_int(I8),
        append_int(SB, I, !IO)
    )
].

:- instance writer(string_builder, int16, io) where [
    ( put(SB, I16, !IO) :-
        I = to_int(I16),
        append_int(SB, I, !IO)
    )
].

:- instance writer(string_builder, int32, io) where [
    pred(put/4) is append_int32
].

:- instance writer(string_builder, int64, io) where [
    pred(put/4) is append_int64
].

:- instance writer(string_builder, string, io) where [
    pred(put/4) is append_string
].

:- instance writer(string_builder, uint, io) where [
    ( put(SB, U, !IO) :-
        S = uint_to_string(U),
        append_string(SB, S, !IO)
    )
].

:- instance writer(string_builder, uint8, io) where [
    ( put(SB, U8, !IO) :-
        I = to_int(U8),
        append_int(SB, I, !IO)
    )
].

:- instance writer(string_builder, uint16, io) where [
    ( put(SB, U16, !IO) :-
        I = to_int(U16),
        append_int(SB, I, !IO)
    )
].

:- instance writer(string_builder, uint32, io) where [
    ( put(SB, U32, !IO) :-
        S = uint32_to_string(U32),
        append_string(SB, S, !IO)
    )
].

:- instance writer(string_builder, uint64, io) where [
    ( put(SB, U32, !IO) :-
        S = uint64_to_string(U32),
        append_string(SB, S, !IO)
    )
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

:- pred append_float(string_builder::in, float::in, io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    append_float(SB::in, F::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SB.append(F);
").

%---------------------------------------------------------------------------%

:- pred append_int(string_builder::in, int::in, io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    append_int(SB::in, I::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SB.append(I);
").

%---------------------------------------------------------------------------%

:- pred append_int32(string_builder::in, int32::in, io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    append_int32(SB::in, I32::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SB.append(I32);
").

%---------------------------------------------------------------------------%

:- pred append_int64(string_builder::in, int64::in, io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    append_int64(SB::in, I64::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SB.append(I64);
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
