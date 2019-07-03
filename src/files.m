%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.nio.file.Files.
%
%---------------------------------------------------------------------------%

:- module jnio.jfile.files.
:- interface.

:- import_module jio.
:- import_module jio.buffered_reader.
:- import_module jio.buffered_writer.
:- import_module jlang.
:- import_module jlang.throwable.
:- import_module jnio.jcharset.
:- import_module jnio.jcharset.charset.
:- import_module jnio.jfile.open_option.
:- import_module jnio.jfile.path.

:- import_module io.
:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- pred new_buffered_reader(P::in,
    maybe_error(buffered_reader, throwable)::out,
    io::di, io::uo) is det <= path(P).

:- pred new_buffered_reader(P::in, C::in,
    maybe_error(buffered_reader, throwable)::out,
    io::di, io::uo) is det <= (path(P), charset(C)).

:- pred new_buffered_writer(P::in, C::in, list(open_option)::in,
    maybe_error(buffered_writer, throwable)::out,
    io::di, io::uo) is det <= (path(P), charset(C)).

:- pred new_buffered_writer(P::in, list(open_option)::in,
    maybe_error(buffered_writer, throwable)::out,
    io::di, io::uo) is det <= path(P).

:- pred size(P::in, maybe_error(int64, throwable)::out,
    io::di, io::uo) is det <= path(P).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.

%---------------------------------------------------------------------------%

new_buffered_reader(Path, Result, !IO) :-
    do_new_buffered_reader(Path, IsOk, Reader, Error, !IO),
    (
        IsOk = yes,
        Result = ok(Reader)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_new_buffered_reader(P::in, bool::out, buffered_reader::out,
    throwable::out, io::di, io::uo) is det <= path(P).
:- pragma foreign_proc("Java",
    do_new_buffered_reader(P::in, IsOk::out, BR::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        BR = java.nio.file.Files.newBufferedReader((java.nio.file.Path) P);
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException | java.lang.SecurityException e) {
        IsOk = bool.NO;
        BR = null;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

new_buffered_reader(Path, Charset, Result, !IO) :-
    do_new_buffered_reader(Path, Charset, IsOk, Reader, Error, !IO),
    (
        IsOk = yes,
        Result = ok(Reader)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_new_buffered_reader(P::in, C::in, bool::out, buffered_reader::out,
    throwable::out, io::di, io::uo) is det <= (path(P), charset(C)).
:- pragma foreign_proc("Java",
    do_new_buffered_reader(P::in, C::in, IsOk::out, BR::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        BR = java.nio.file.Files.newBufferedReader(
            (java.nio.file.Path) P,
            (java.nio.charset.Charset) C);
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException | java.lang.SecurityException e) {
        IsOk = bool.NO;
        BR = null;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

size(Path, Result, !IO) :-
    do_size(Path, IsOk, Size, Error, !IO),
    (
        IsOk = yes,
        Result = ok(Size)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_size(P::in, bool::out, int64::out, throwable::out,
    io::di, io::uo) is det <= path(P).
:- pragma foreign_proc("Java",
    do_size(P::in, IsOk::out, Size::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        Size = java.nio.file.Files.size((java.nio.file.Path) P);
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException | java.lang.SecurityException e) {
        Size = 0L;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

new_buffered_writer(Path, Charset, Options, Result, !IO) :-
    JOptions = list.map(open_option_to_jopen_option, Options),
    list.length(Options, NumOptions),
    do_new_buffered_writer(Path, Charset ,NumOptions, JOptions, IsOk, Writer,
        Error, !IO),
    (
        IsOk = yes,
        Result = ok(Writer)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_new_buffered_writer(P::in, C::in, int::in, list(jopen_option)::in,
    bool::out, buffered_writer::out, throwable::out, io::di, io::uo)
    is det <= (path(P), charset(C)).
:- pragma foreign_proc("Java",
    do_new_buffered_writer(P::in, C::in, NumOptions::in, Options::in,
        IsOk::out, BW::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    java.nio.file.OpenOption opts[] = new java.nio.file.OpenOption[NumOptions];
    for (int i = 0; !list.is_empty(Options); i++) {
        opts[i] = list.det_head(Options);
        Options = list.det_tail(Options);
    }

    try {
        BW = java.nio.file.Files.newBufferedWriter(
            (java.nio.file.Path) P,
            (java.nio.charset.Charset) C, opts);
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException |
            java.lang.UnsupportedOperationException |
            java.lang.SecurityException e) {
        BW = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

new_buffered_writer(Path, Options, Result, !IO) :-
    JOptions = list.map(open_option_to_jopen_option, Options),
    list.length(Options, NumOptions),
    do_new_buffered_writer(Path, NumOptions, JOptions, IsOk, Writer, Error,
        !IO),
    (
        IsOk = yes,
        Result = ok(Writer)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_new_buffered_writer(P::in, int::in, list(jopen_option)::in,
    bool::out, buffered_writer::out, throwable::out, io::di, io::uo)
    is det <= path(P).
:- pragma foreign_proc("Java",
    do_new_buffered_writer(P::in, NumOptions::in, Options::in,
        IsOk::out, BW::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    java.nio.file.OpenOption opts[] = new java.nio.file.OpenOption[NumOptions];
    for (int i = 0; !list.is_empty(Options); i++) {
        opts[i] = list.det_head(Options);
        Options = list.det_tail(Options);
    }

    try {
        BW = java.nio.file.Files.newBufferedWriter(
            (java.nio.file.Path) P, opts);
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException |
            java.lang.UnsupportedOperationException |
            java.lang.SecurityException e) {
        BW = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- func open_option_to_jopen_option(open_option) = jopen_option.

open_option_to_jopen_option(open_option(Option)) = to_jopen_option(Option).

%---------------------------------------------------------------------------%
:- end_module jnio.jfile.files.
%---------------------------------------------------------------------------%
