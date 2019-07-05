%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% A Mercury wrapper for the java.nio.file.Path interface.
%
%-----------------------------------------------------------------------------%

:- module jnio.jfile.path.
:- interface.

:- import_module jio.
:- import_module jio.file.
:- import_module jlang.
:- import_module jlang.throwable.
:- import_module jnet.
:- import_module jnet.uri.

:- import_module io.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- typeclass path(T) where [].
:- type path.
:- instance path(path).

%-----------------------------------------------------------------------------%

:- pred is_absolute(P::in) is semidet <= path(P).

:- pred to_file(P::in, maybe_error(file, throwable)::out, io::di, io::uo)
    is det <= path(P).

:- pred to_uri(P::in, maybe_error(uri, throwable)::out, io::di, io::uo)
    is det <= path(P).

:- func to_string(P::in, io::ui) = (string::out) is det <= path(P).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.

%-----------------------------------------------------------------------------%

:- pragma foreign_type("Java", path, "java.nio.file.Path").

:- instance path(path) where [].

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_absolute(P::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((java.nio.file.Path) P).isAbsolute();
").

%-----------------------------------------------------------------------------%

to_file(Path, Result, !IO) :-
    do_to_file(Path, IsOk, File, Error, !IO),
    (
        IsOk = yes,
        Result = ok(File)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_to_file(P::in, bool::out, file::out, throwable::out,
    io::di, io::uo) is det <= path(P).
:- pragma foreign_proc("Java",
    do_to_file(P::in, IsOk::out, F::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        F = ((java.nio.file.Path) P).toFile();
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.UnsupportedOperationException e) {
        F = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%-----------------------------------------------------------------------------%

to_uri(Path, Result, !IO) :-
    do_to_uri(Path, IsOk, URI, Error, !IO),
    (
        IsOk = yes,
        Result = ok(URI)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_to_uri(P::in, bool::out, uri::out, throwable::out,
    io::di, io::uo) is det <= path(P).
:- pragma foreign_proc("Java",
    do_to_uri(P::in, IsOk::out, U::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        U = ((java.nio.file.Path) P).toUri();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOError | java.lang.SecurityException e) {
        U = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(P::in, _IO::ui) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = P.toString();
").

%-----------------------------------------------------------------------------%
:- end_module jnio.jfile.path.
%-----------------------------------------------------------------------------%
