%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.io.Reader.
%
%---------------------------------------------------------------------------%

:- module jio.reader.
:- interface.

:- import_module io.

:- typeclass reader(T) where [].

:- type reader.
:- instance reader(reader).

:- pred close(R::in, io::di, io::uo) is det <= reader(R).

%:- pred mark(R::in, int::in, io::di, io::uo) is det <= reader(R)

%:- pred mark_supported(R::in, io::ui) is semidet <= reader(R).

%:- pred read(R::in, io.result(char)::out, io::di, io::uo) is det <= reader(R).

%:- pred is_ready(R::in, io::ui) is semidet.

%:- pred reset(R::in, io::di, io::uo) is det <= reader(R).

%:- pred skip(R::in, int64::int, io::di, io::uo) is det <= reader(R).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module bool.
:- import_module exception.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", reader, "java.io.Reader").

:- instance reader(reader) where [].

%---------------------------------------------------------------------------%

close(R, !IO) :-
    do_close(R, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_close(R::in, bool::out, throwable::out, io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_close(R::in, IsOk::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.io.Reader) R).close();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%
:- end_module reader.
%---------------------------------------------------------------------------%
