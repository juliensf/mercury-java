%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2019 Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% A Mercury wrapper for the java.nio.file.Paths class.
%
%-----------------------------------------------------------------------------%

:- module jnio.jfile.paths.
:- interface.

:- import_module jlang.
:- import_module jlang.throwable.
:- import_module jnet.
:- import_module jnet.uri.
:- import_module jnio.jfile.path.

:- import_module io.
:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- pred get(string::in, list(string)::in, maybe_error(path, throwable)::out,
    io::di, io::uo) is det.

:- pred get(uri::in, maybe_error(path, throwable)::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool.

%-----------------------------------------------------------------------------%

get(First, More, Result, !IO) :-
    list.length(More, NumMore),
    do_get(First, More, NumMore, IsOk, Path, Error, !IO),
    (
        IsOk = yes,
        Result = ok(Path)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_get(string::in, list(string)::in, int::in, bool::out, path::out,
    throwable::out, io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_get(First::in, More::in, NumMore::in, IsOk::out, P::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    String[] more = new String[NumMore];
    for (int i = 0; !list.is_empty(More); i++) {
        more[i] = list.det_head(More);
        More = list.det_tail(More);
    }

    try {
        P = java.nio.file.Paths.get(First, more);
        IsOk = bool.YES;
        Error = null;
    } catch (java.nio.file.InvalidPathException e) {
        IsOk = bool.NO;
        P = null;
        Error = e;
    }
").

%-----------------------------------------------------------------------------%

get(URI, Result, !IO) :-
    do_get(URI, IsOk, Path, Error, !IO),
    (
        IsOk = yes,
        Result = ok(Path)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_get(uri::in, bool::out, path::out, throwable::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_get(U::in, IsOk::out, P::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        P = java.nio.file.Paths.get(U);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.IllegalArgumentException |
            java.nio.file.FileSystemNotFoundException |
            java.lang.SecurityException e) {
        P = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%-----------------------------------------------------------------------------%
:- end_module jnio.jfile.paths.
%-----------------------------------------------------------------------------%
