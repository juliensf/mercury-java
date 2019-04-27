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
:- import_module jnet.proxy.
:- import_module jnet.socket_address.

:- import_module bool.
:- import_module io.
:- import_module maybe.
:- import_module uint16.

%---------------------------------------------------------------------------%

:- typeclass socket(T) where [].

:- type socket.
:- instance socket(socket).

%---------------------------------------------------------------------------%
%
% Wrappers for Socket constructors.
%

    % Socket().
:- pred new_unconnected_socket(socket::out, io::di, io::uo) is det.

    % Socket(Proxy proxy)
:- pred new_unconnected_socket(proxy::in, maybe_error(socket, throwable)::out,
    io::di, io::uo) is det.

:- pred new_socket(I::in, uint16::in, maybe_error(socket, throwable)::out,
    io::di, io::uo) is det <= inet_address(I).

%---------------------------------------------------------------------------%

:- pred bind(S::in, A::in, io::di, io::uo) is det
    <= (socket(S), socket_address(A)).

:- pred close(S::in, io::di, io::uo) is det <= socket(S).

:- pred connect(S::in, A::in, io::di, io::uo) is det
    <= (socket(S), socket_address(A)).

:- pred connect(S::in, A::in, int::in, io::di, io::uo) is det
    <= (socket(S), socket_address(A)).

% get_channel

:- func get_inet_address(S::in, io::ui) = (maybe(inet_address)::out)
    is det <= socket(S).

:- pred get_input_stream(S::in, maybe_error(jinput_stream, throwable)::out,
    io::di, io::uo) is det <= socket(S).

:- pred get_keep_alive(S::in, maybe_error(bool, throwable)::out,
    io::di, io::uo) is det <= socket(S).

:- func get_local_address(S::in, io::ui) = (inet_address::out)
    is det <= socket(S).

:- pred get_local_port(S::in, maybe(uint16)::out, io::di, io::uo)
    is det <= socket(S).

:- pred get_local_socket_address(S::in, maybe(socket_address)::out,
    io::di, io::uo) is det <= socket(S).

:- pred get_oob_inline(S::in, maybe_error(bool, throwable)::out,
    io::di, io::uo) is det <= socket(S).

:- pred get_output_stream(S::in, maybe_error(joutput_stream, throwable)::out,
    io::di, io::uo) is det <= socket(S).

:- pred is_bound(S::in, io::ui) is semidet <= socket(S).

:- pred is_closed(S::in, io::ui) is semidet <= socket(S).

:- pred is_connected(S::in, io::ui) is semidet <= socket(S).

:- pred is_input_shutdown(S::in, io::ui) is semidet <= socket(S).

:- pred is_output_shutdown(S::in, io::ui) is semidet <= socket(S).

:- pred shutdown_input(S::in, io::di, io::uo) is det <= socket(S).

:- pred shutdown_output(S::in, io::di, io::uo) is det <= socket(S).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", socket, "java.net.Socket").
:- instance socket(socket) where [].

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    new_unconnected_socket(S::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = new java.net.Socket();
").

%---------------------------------------------------------------------------%

new_unconnected_socket(Proxy, Result, !IO) :-
    do_new_unconnected_socket(Proxy, IsOk, Socket, Error, !IO),
    (
        IsOk = yes,
        Result = ok(Socket)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_new_unconnected_socket(proxy::in, bool::out, socket::out,
    throwable::out, io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_new_unconnected_socket(P::in, IsOk::out, S::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        S = new java.net.Socket(P);
        IsOk = bool.YES;
        Error = null;
    } catch (
        java.lang.IllegalArgumentException | java.lang.SecurityException e
    ) {
        S = null;
        IsOk = bool.NO;
        Error = e;
    }
").

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

connect(S, A, !IO) :-
    do_connect(S, A, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_connect(S::in, A::in, bool::out, throwable::out,
    io::di, io::uo) is det <= (socket(S), socket_address(A)).
:- pragma foreign_proc("Java",
    do_connect(S::in, A::in, IsOk::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.net.Socket) S).connect((java.net.SocketAddress) A);
        IsOk = bool.YES;
        Error = null;
    } catch (
        java.io.IOException |
        java.nio.channels.IllegalBlockingModeException |
        java.lang.IllegalArgumentException e
    ) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

connect(S, A, Timeout, !IO) :-
    do_connect(S, A, Timeout, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_connect(S::in, A::in, int::in, bool::out, throwable::out,
    io::di, io::uo) is det <= (socket(S), socket_address(A)).
:- pragma foreign_proc("Java",
    do_connect(S::in, A::in, T::in, IsOk::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.net.Socket) S).connect((java.net.SocketAddress) A, T);
        IsOk = bool.YES;
        Error = null;
    } catch (
        java.io.IOException |
        java.nio.channels.IllegalBlockingModeException |
        java.lang.IllegalArgumentException e
    ) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

get_inet_address(S, IO) = Result :-
    do_get_inet_address(S, HaveResult, InetAddr, IO),
    (
        HaveResult = yes,
        Result = yes(InetAddr)
    ;
        HaveResult = no,
        Result = no
    ).

:- pred do_get_inet_address(S::in, bool::out, inet_address::out, io::ui) is det
    <= socket(S).
:- pragma foreign_proc("Java",
    do_get_inet_address(S::in, HaveResult::out, IA::out, _IO::ui),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    IA = ((java.net.Socket) S).getInetAddress();
    HaveResult = (IA != null ? bool.YES : bool.NO);
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

get_keep_alive(S, Result, !IO) :-
    do_get_keep_alive(S, IsOk, KeepAlive, Error, !IO),
    (
        IsOk = yes,
        Result = ok(KeepAlive)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_get_keep_alive(S::in, bool::out, bool::out, throwable::out,
    io::di, io::uo) is det <= socket(S).
:- pragma foreign_proc("Java",
    do_get_keep_alive(S::in, IsOk::out, KeepAlive::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        KeepAlive = ((java.net.Socket) S).getKeepAlive() ? bool.YES : bool.NO;
        IsOk = bool.YES;
        Error = null;
    } catch (java.net.SocketException e) {
        KeepAlive = bool.NO;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_local_address(S::in, _IO::ui) = (IA::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    IA = ((java.net.Socket) S).getLocalAddress();
").

%---------------------------------------------------------------------------%

get_local_port(S, Result, !IO) :-
    do_get_local_port(S, PortAsInt, !IO),
    ( if PortAsInt = -1 then
        Result = no
    else
        Port = det_from_int(PortAsInt),
        Result = yes(Port)
    ).

:- pred do_get_local_port(S::in, int::out, io::di, io::uo)
    is det <= socket(S).
:- pragma foreign_proc("Java",
    do_get_local_port(S::in, P::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    P = ((java.net.Socket) S).getLocalPort();
").

%---------------------------------------------------------------------------%

get_local_socket_address(S, Result, !IO) :-
    do_get_local_socket_address(S, HaveSA, SA, !IO),
    (
        HaveSA = no,
        Result = no
    ;
        HaveSA = yes,
        Result = yes(SA)
    ).

:- pred do_get_local_socket_address(S::in, bool::out, socket_address::out,
    io::di, io::uo) is det <= socket(S).
:- pragma foreign_proc("Java",
    do_get_local_socket_address(S::in, HaveSA::out, SA::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SA = ((java.net.Socket) S).getLocalSocketAddress();
    HaveSA = (SA != null ? bool.YES : bool.NO);
").

%---------------------------------------------------------------------------%

get_oob_inline(S, Result, !IO) :-
    do_get_oob_inline(S, IsOk, OOBInline, Error, !IO),
    (
        IsOk = yes,
        Result = ok(OOBInline)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_get_oob_inline(S::in, bool::out, bool::out, throwable::out,
    io::di, io::uo) is det <= socket(S).
:- pragma foreign_proc("Java",
    do_get_oob_inline(S::in, IsOk::out, OOBInline::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        OOBInline = ((java.net.Socket) S).getOOBInline() ? bool.YES : bool.NO;
        IsOk = bool.YES;
        Error = null;
    } catch (java.net.SocketException e) {
        OOBInline = bool.NO;
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

:- pragma foreign_proc("Java",
    is_bound(S::in, _IO::ui),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((java.net.Socket) S).isBound();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_closed(S::in, _IO::ui),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((java.net.Socket) S).isClosed();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_connected(S::in, _IO::ui),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((java.net.Socket) S).isConnected();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_input_shutdown(S::in, _IO::ui),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((java.net.Socket) S).isInputShutdown();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_output_shutdown(S::in, _IO::ui),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((java.net.Socket) S).isOutputShutdown();
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
