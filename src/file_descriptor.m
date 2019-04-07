%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.io.FileDescriptor.
%
%---------------------------------------------------------------------------%

:- module jio.file_descriptor.
:- interface.

:- import_module io.

%---------------------------------------------------------------------------%

:- type file_descriptor.

:- pred sync(file_descriptor::in, io::di, io::uo) is det.

:- pred valid(file_descriptor::in, io::ui) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module bool.
:- import_module exception.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", file_descriptor, "java.io.FileDescriptor").

%---------------------------------------------------------------------------%

sync(FD, !IO) :-
    do_sync(FD, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_sync(file_descriptor::in, bool::out, throwable::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_sync(FD::in, IsOk::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        FD.sync();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.SyncFailedException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    valid(FD::in, _IO::ui),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = FD.valid();
").

%---------------------------------------------------------------------------%
:- end_module file_descriptor.
%---------------------------------------------------------------------------%
