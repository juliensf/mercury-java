%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.net.Socket.
%
%---------------------------------------------------------------------------%

:- module jnet.socket.
:- interface.

:- import_module jio.
:- import_module jio.input_stream.
:- import_module jio.output_stream.
:- import_module jlang.
:- import_module jlang.throwable.
:- import_module jnet.inet_address.
:- import_module jnet.socket_address.

:- import_module io.
:- import_module maybe.
:- import_module uint16.

%---------------------------------------------------------------------------%

:- typeclass socket(T) where [].

:- type socket.
:- instance socket(socket).

:- pred new_socket(I::in, uint16::in, maybe_error(socket, throwable)::out,
    io::di, io::uo) is det <= inet_address(I).

:- pred bind(S::in, A::in, io::di, io::uo) is det
    <= (socket(S), socket_address(A)).

%:- pred connect(S::in, A::in, io::di, io::uo) is det
%    <= (socket(S), socket_address(A)).

%:- pred connect(S::in, A::in, int::in, io::di, io::uo) is det
%    <= (socket(S), socket_address(A)).

:- pred close(S::in, io::di, io::uo) is det <= socket(S).

:- pred get_input_stream(S::in, maybe_error(jinput_stream, throwable)::out,
    io::di, io::uo) is det <= socket(S).

:- pred get_output_stream(S::in, maybe_error(joutput_stream, throwable)::out,
    io::di, io::uo) is det <= socket(S).

:- pred shutdown_input(S::in, io::di, io::uo) is det <= socket(S).

:- pred shutdown_output(S::in, io::di, io::uo) is det <= socket(S).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", socket, "java.net.Socket").
:- instance socket(socket) where [].

%---------------------------------------------------------------------------%

new_socket(IP, Port, Result, !IO) :-
    do_new_socket(IP, cast_to_int(Port), IsOk, Socket, Error, !IO),
    (
        IsOk = yes,
        Result = ok(Socket)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_new_socket(I::in, int::in, bool::out, socket::out,
    throwable::out, io::di, io::uo) is det <= inet_address(I).
:- pragma foreign_proc("Java",
    do_new_socket(IP::in, Port::in, IsOk::out, S::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        S = new java.net.Socket((java.net.InetAddress) IP, Port);
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException | java.lang.SecurityException e) {
        S = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%


bind(S, A, !IO) :-
    do_bind(S, A, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_bind(S::in, A::in, bool::out, throwable::out, io::di, io::uo)
    is det <= (socket(S), socket_address(A)).
:- pragma foreign_proc("Java",
    do_bind(S::in, A::in, IsOk::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.net.Socket) S).bind((java.net.SocketAddress) A);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.Exception e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

close(S, !IO) :-
    do_close(S, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_close(S::in, bool::out, throwable::out, io::di, io::uo)
    is det <= socket(S).
:- pragma foreign_proc("Java",
    do_close(S::in, IsOk::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.net.Socket) S).close();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

get_input_stream(S, Result, !IO) :-
    do_get_input_stream(S, IsOk, InputStream, Error, !IO),
    (
        IsOk = yes,
        Result = ok(InputStream)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_get_input_stream(S::in, bool::out, jinput_stream::out,
    throwable::out, io::di, io::uo) is det <= socket(S).
:- pragma foreign_proc("Java",
    do_get_input_stream(S::in, IsOk::out, IS::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        IS = ((java.net.Socket) S).getInputStream();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IS = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

get_output_stream(S, Result, !IO) :-
    do_get_output_stream(S, IsOk, OutputStream, Error, !IO),
    (
        IsOk = yes,
        Result = ok(OutputStream)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_get_output_stream(S::in, bool::out, joutput_stream::out,
    throwable::out, io::di, io::uo) is det <= socket(S).
:- pragma foreign_proc("Java",
    do_get_output_stream(S::in, IsOk::out, IS::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        IS = ((java.net.Socket) S).getOutputStream();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IS = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

shutdown_input(S, !IO) :-
    do_shutdown_input(S, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_shutdown_input(S::in, bool::out, throwable::out, io::di, io::uo)
    is det <= socket(S).
:- pragma foreign_proc("Java",
    do_shutdown_input(S::in, IsOk::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.net.Socket) S).shutdownInput();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

shutdown_output(S, !IO) :-
    do_shutdown_output(S, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_shutdown_output(S::in, bool::out, throwable::out, io::di, io::uo)
    is det <= socket(S).
:- pragma foreign_proc("Java",
    do_shutdown_output(S::in, IsOk::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.net.Socket) S).shutdownOutput();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%
:- end_module socket.
%---------------------------------------------------------------------------%
