%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.nio.channels.Channel.
%
%---------------------------------------------------------------------------%

:- module jnio.channels.channel.
:- interface.

:- import_module io.

%---------------------------------------------------------------------------%

:- typeclass channel(T) where [].

:- type channel.

:- instance channel(channel).

%---------------------------------------------------------------------------%

:- pred is_open(T::in, io::ui) is det <= channel(T).

:- pred close(T::in, io::di, io::uo) is det <= channel(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module bool.
:- import_module exception.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", channel, "java.nio.channels.Channel").

:- instance channel(channel) where [].

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_open(C::in, _IO::ui),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((java.nio.channels.Channel) C).isOpen();
").

%---------------------------------------------------------------------------%

close(C, !IO) :-
    do_close(C, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_close(C::in, bool::out, throwable::out, io::di, io::uo) is det
    <= channel(C).
:- pragma foreign_proc("Java",
    do_close(C::in, IsOk::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.nio.channels.Channel) C).close();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%
:- end_module channel.
%---------------------------------------------------------------------------%
