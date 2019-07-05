%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.io.File.
%
%---------------------------------------------------------------------------%

:- module jio.file.
:- interface.

:- import_module jlang.
:- import_module jlang.throwable.
:- import_module jnio.
:- import_module jnio.jfile.
:- import_module jnio.jfile.path.

:- import_module io.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type file.

:- func file(string) = file.

:- func get_name(file) = string.

:- func get_parent(file) = maybe(string).

:- pred to_path(file::in, maybe_error(path, throwable)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", file, "java.io.File").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    file(S::in) = (F::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    F = new java.io.File(S);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_name(F::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = F.getName();
").

%---------------------------------------------------------------------------%

get_parent(File) = Result :-
    do_get_raw_query(File, Ok, Parent),
    (
        Ok = no,
        Result = no
    ;
        Ok = yes,
        Result = yes(Parent)
    ).

:- pred do_get_raw_query(file::in, bool::out, string::out) is det.

:- pragma foreign_proc("Java",
    do_get_raw_query(F::in, Ok::out, P::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    P = F.getParent();
    Ok = (P == null) ? bool.NO : bool.YES;
").

%---------------------------------------------------------------------------%

to_path(File, Result, !IO) :-
    do_to_path(File, IsOk, Path, Error, !IO),
    (
        IsOk = yes,
        Result = ok(Path)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_to_path(file::in, bool::out, path::out, throwable::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_to_path(F::in, IsOk::out, P::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        P = F.toPath();
        IsOk = bool.YES;
        Error = null;
    } catch (java.nio.file.InvalidPathException e) {
        P = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%
:- end_module file.
%---------------------------------------------------------------------------%
