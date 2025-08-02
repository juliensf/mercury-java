%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 Julien Fischer.
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

%---------------------------------------------------------------------------%

:- typeclass socket(T) where [].

:- type socket.
:- instance socket(socket).

%---------------------------------------------------------------------------%
%
% Wrappers for Socket constructors.
%

    % Socket().
:- pred new_socket(socket::out, io::di, io::uo) is det.

    % Socket(Proxy proxy)
:- pred new_socket(proxy::in, maybe_error(socket, throwable)::out,
    io::di, io::uo) is det.

:- pred new_socket(I::in, uint16::in, maybe_error(socket, throwable)::out,
    io::di, io::uo) is det <= inet_address(I).

:- pred new_socket(RA::in, uint16::in, LA::in, uint16::in,
    maybe_error(socket, throwable)::out, io::di, io::uo) is det
    <= (inet_address(RA), inet_address(LA)).

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

:- pred get_port(S::in, maybe(uint16)::out, io::di, io::uo)
    is det <= socket(S).

:- pred get_receive_buffer_size(S::in, maybe_error(int, throwable)::out,
    io::di, io::uo) is det <= socket(S).

:- pred get_remote_socket_address(S::in, maybe(socket_address)::out,
    io::di, io::uo) is det <= socket(S).

:- pred get_reuse_address(S::in, maybe_error(bool, throwable)::out,
    io::di, io::uo) is det <= socket(S).

:- pred get_send_buffer_size(S::in, maybe_error(int, throwable)::out,
    io::di, io::uo) is det <= socket(S).

:- pred get_so_linger(S::in, maybe_error(int, throwable)::out,
    io::di, io::uo) is det <= socket(S).

:- pred get_so_timeout(S::in, maybe_error(int, throwable)::out,
    io::di, io::uo) is det <= socket(S).

:- pred get_tcp_no_delay(S::in, maybe_error(bool, throwable)::out,
    io::di, io::uo) is det <= socket(S).

:- pred get_traffic_class(S::in, maybe_error(uint8, throwable)::out,
    io::di, io::uo) is det <= socket(S).

:- pred is_bound(S::in, io::ui) is semidet <= socket(S).

:- pred is_closed(S::in, io::ui) is semidet <= socket(S).

:- pred is_connected(S::in, io::ui) is semidet <= socket(S).

:- pred is_input_shutdown(S::in, io::ui) is semidet <= socket(S).

:- pred is_output_shutdown(S::in, io::ui) is semidet <= socket(S).

:- pred send_urgent_data(S::in, uint8::in, io::di, io::uo)
    is det <= socket(S).

:- pred set_keep_alive(S::in, bool::in, io::di, io::uo)
    is det <= socket(S).

:- pred set_oob_inline(S::in, bool::in, io::di, io::uo)
    is det <= socket(S).

:- pred set_reuse_address(S::in, bool::in, io::di, io::uo)
    is det <= socket(S).

:- pred set_performance_preferences(S::in, int::in, int::in, int::in,
    io::di, io::uo) is det <= socket(S).

:- pred set_receive_buffer_size(S::in, int::in, io::di, io::uo)
    is det <= socket(S).

:- pred set_send_buffer_size(S::in, int::in, io::di, io::uo)
    is det <= socket(S).

:- pred set_so_linger(S::in, bool::in, int::in, io::di, io::uo)
    is det <= socket(S).

:- pred set_so_timeout(S::in, int::in, io::di, io::uo)
    is det <= socket(S).

:- pred set_tcp_no_delay(S::in, bool::in, io::di, io::uo)
    is det <= socket(S).

:- pred set_traffic_class(S::in, uint8::in, io::di, io::uo)
    is det <= socket(S).

:- pred shutdown_input(S::in, io::di, io::uo) is det <= socket(S).

:- pred shutdown_output(S::in, io::di, io::uo) is det <= socket(S).

:- func to_string(S::in, io::ui) = (string::out) is det <= socket(S).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module exception.
:- import_module uint8.
:- import_module uint16.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", socket, "java.net.Socket").
:- instance socket(socket) where [].

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    new_socket(S::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = new java.net.Socket();
").

%---------------------------------------------------------------------------%

new_socket(Proxy, Result, !IO) :-
    do_new_socket(Proxy, IsOk, Socket, Error, !IO),
    (
        IsOk = yes,
        Result = ok(Socket)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_new_socket(proxy::in, bool::out, socket::out, throwable::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_new_socket(P::in, IsOk::out, S::out, Error::out, _IO0::di, _IO::uo),
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

new_socket(RA, RPort, LA, LPort, Result, !IO) :-
    do_new_socket(RA, to_int(RPort), LA, to_int(LPort), IsOk, Socket, Error,
        !IO),
    (
        IsOk = no,
        Result = error(Error)
    ;
        IsOk = yes,
        Result = ok(Socket)
    ).

:- pred do_new_socket(RA::in, int::in, LA::in, int::in,
    bool::out, socket::out, throwable::out, io::di, io::uo) is det
    <= (inet_address(RA), inet_address(LA)).
:- pragma foreign_proc("Java",
    do_new_socket(RA::in, RPort::in, LA::in, LPort::in, IsOk::out,
        S::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        S = new java.net.Socket((java.net.InetAddress) RA, RPort,
            (java.net.InetAddress) LA, LPort);
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

get_port(S, Result, !IO) :-
    do_get_port(S, PortAsInt, !IO),
    ( if PortAsInt = 0 then
        Result = no
    else
        Result = yes(det_from_int(PortAsInt))
    ).

:- pred do_get_port(S::in, int::out, io::di, io::uo) is det <= socket(S).
:- pragma foreign_proc("Java",
    do_get_port(S::in, Port::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Port = ((java.net.Socket) S).getPort();
").

%---------------------------------------------------------------------------%

get_receive_buffer_size(S, Result, !IO) :-
    do_get_receive_buffer_size(S, Size, IsOk, Error, !IO),
    (
        IsOk = yes,
        Result = ok(Size)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_get_receive_buffer_size(S::in, int::out, bool::out, throwable::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_get_receive_buffer_size(S::in, Size::out, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        Size = ((java.net.Socket) S).getReceiveBufferSize();
        IsOk = bool.YES;
        Error = null;
    } catch (java.net.SocketException e) {
        Size = 0;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

get_remote_socket_address(S, Result, !IO) :-
    do_get_remote_socket_address(S, HaveSA, SA, !IO),
    (
        HaveSA = no,
        Result = no
    ;
        HaveSA = yes,
        Result = yes(SA)
    ).

:- pred do_get_remote_socket_address(S::in, bool::out, socket_address::out,
    io::di, io::uo) is det <= socket(S).
:- pragma foreign_proc("Java",
    do_get_remote_socket_address(S::in, HaveSA::out, SA::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SA = ((java.net.Socket) S).getRemoteSocketAddress();
    HaveSA = (SA != null ? bool.YES : bool.NO);
").

%---------------------------------------------------------------------------%

get_reuse_address(S, Result, !IO) :-
    do_get_reuse_address(S, ReuseAddr, IsOk, Error, !IO),
    (
        IsOk = yes,
        Result = ok(ReuseAddr)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_get_reuse_address(S::in, bool::out, bool::out, throwable::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_get_reuse_address(S::in, RA::out, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        RA = ((java.net.Socket) S).getReuseAddress() ? bool.YES : bool.NO;
        IsOk = bool.YES;
        Error = null;
    } catch (java.net.SocketException e) {
        RA = bool.NO;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

get_send_buffer_size(S, Result, !IO) :-
    do_get_send_buffer_size(S, Size, IsOk, Error, !IO),
    (
        IsOk = yes,
        Result = ok(Size)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_get_send_buffer_size(S::in, int::out, bool::out, throwable::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_get_send_buffer_size(S::in, Size::out, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        Size = ((java.net.Socket) S).getSendBufferSize();
        IsOk = bool.YES;
        Error = null;
    } catch (java.net.SocketException e) {
        Size = 0;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

get_so_linger(S, Result, !IO) :-
    do_get_so_linger(S, TO, IsOk, Error, !IO),
    (
        IsOk = yes,
        Result = ok(TO)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_get_so_linger(S::in, int::out, bool::out, throwable::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_get_so_linger(S::in, TO::out, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        TO = ((java.net.Socket) S).getSoLinger();
        IsOk = bool.YES;
        Error = null;
    } catch (java.net.SocketException e) {
        TO = 0;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

get_so_timeout(S, Result, !IO) :-
    do_get_so_timeout(S, TO, IsOk, Error, !IO),
    (
        IsOk = yes,
        Result = ok(TO)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_get_so_timeout(S::in, int::out, bool::out, throwable::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_get_so_timeout(S::in, TO::out, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        TO = ((java.net.Socket) S).getSoTimeout();
        IsOk = bool.YES;
        Error = null;
    } catch (java.net.SocketException e) {
        TO = 0;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

get_tcp_no_delay(S, Result, !IO) :-
    do_get_tcp_no_delay(S, NoDelay, IsOk, Error, !IO),
    (
        IsOk = yes,
        Result = ok(NoDelay)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_get_tcp_no_delay(S::in, bool::out, bool::out, throwable::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_get_tcp_no_delay(S::in, NoDelay::out, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        NoDelay = ((java.net.Socket) S).getTcpNoDelay() ? bool.YES : bool.NO;
        IsOk = bool.YES;
        Error = null;
    } catch (java.net.SocketException e) {
        NoDelay = bool.NO;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

get_traffic_class(S, Result, !IO) :-
    do_get_traffic_class(S, TCInt, IsOk, Error, !IO),
    (
        IsOk = yes,
        Result = ok(det_from_int(TCInt))
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_get_traffic_class(S::in, int::out, bool::out, throwable::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_get_traffic_class(S::in, TC::out, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        TC = ((java.net.Socket) S).getTrafficClass();
        IsOk = bool.YES;
        Error = null;
    } catch (java.net.SocketException e) {
        TC = 0;
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

send_urgent_data(S, Byte, !IO) :-
    do_send_urgent_data(S, to_int(Byte), IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_send_urgent_data(S::in, int::in, bool::out, throwable::out,
    io::di, io::uo) is det <= socket(S).
:- pragma foreign_proc("Java",
    do_send_urgent_data(S::in, Byte::in, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.net.Socket) S).sendUrgentData(Byte);
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

set_keep_alive(S, On, !IO) :-
    do_set_keep_alive(S, On, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_set_keep_alive(S::in, bool::in, bool::out, throwable::out,
    io::di, io::uo) is det <= socket(S).
:- pragma foreign_proc("Java",
    do_set_keep_alive(S::in, On::in, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.net.Socket) S).setKeepAlive(On.equals(bool.YES));
        IsOk = bool.YES;
        Error = null;
    } catch (java.net.SocketException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

set_oob_inline(S, On, !IO) :-
    do_set_oob_inline(S, On, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_set_oob_inline(S::in, bool::in, bool::out, throwable::out,
    io::di, io::uo) is det <= socket(S).
:- pragma foreign_proc("Java",
    do_set_oob_inline(S::in, On::in, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.net.Socket) S).setOOBInline(On.equals(bool.YES));
        IsOk = bool.YES;
        Error = null;
    } catch (java.net.SocketException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

set_reuse_address(S, On, !IO) :-
    do_set_reuse_address(S, On, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_set_reuse_address(S::in, bool::in, bool::out, throwable::out,
    io::di, io::uo) is det <= socket(S).
:- pragma foreign_proc("Java",
    do_set_reuse_address(S::in, On::in, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.net.Socket) S).setReuseAddress(On.equals(bool.YES));
        IsOk = bool.YES;
        Error = null;
    } catch (java.net.SocketException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    set_performance_preferences(S::in, CT::in, L::in, BW::in,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ((java.net.Socket) S).setPerformancePreferences(CT, L, BW);
").

%---------------------------------------------------------------------------%

set_receive_buffer_size(S, Size, !IO) :-
    do_set_receive_buffer_size(S, Size, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_set_receive_buffer_size(S::in, int::in, bool::out, throwable::out,
    io::di, io::uo) is det <= socket(S).
:- pragma foreign_proc("Java",
    do_set_receive_buffer_size(S::in, Size::in, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.net.Socket) S).setReceiveBufferSize(Size);
        IsOk = bool.YES;
        Error = null;
    } catch (java.net.SocketException | java.lang.IllegalArgumentException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

set_send_buffer_size(S, Size, !IO) :-
    do_set_send_buffer_size(S, Size, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_set_send_buffer_size(S::in, int::in, bool::out, throwable::out,
    io::di, io::uo) is det <= socket(S).
:- pragma foreign_proc("Java",
    do_set_send_buffer_size(S::in, Size::in, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.net.Socket) S).setSendBufferSize(Size);
        IsOk = bool.YES;
        Error = null;
    } catch (java.net.SocketException | java.lang.IllegalArgumentException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

set_so_linger(S, On, Linger, !IO) :-
    do_set_so_linger(S, On, Linger, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_set_so_linger(S::in, bool::in, int::in, bool::out,
    throwable::out, io::di, io::uo) is det <= socket(S).
:- pragma foreign_proc("Java",
    do_set_so_linger(S::in, On::in, Linger::in, IsOk::out,
        Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.net.Socket) S).setSoLinger(On.equals(bool.YES), Linger);
        IsOk = bool.YES;
        Error = null;
    } catch (java.net.SocketException | java.lang.IllegalArgumentException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

set_so_timeout(S, TO, !IO) :-
    do_set_so_timeout(S, TO, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_set_so_timeout(S::in, int::in, bool::out, throwable::out,
    io::di, io::uo) is det <= socket(S).
:- pragma foreign_proc("Java",
    do_set_so_timeout(S::in, TO::in, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.net.Socket) S).setSoTimeout(TO);
        IsOk = bool.YES;
        Error = null;
    } catch (java.net.SocketException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

set_tcp_no_delay(S, On, !IO) :-
    do_set_tcp_no_delay(S, On, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_set_tcp_no_delay(S::in, bool::in, bool::out, throwable::out,
    io::di, io::uo) is det <= socket(S).
:- pragma foreign_proc("Java",
    do_set_tcp_no_delay(S::in, On::in, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.net.Socket) S).setTcpNoDelay(On.equals(bool.YES));
        IsOk = bool.YES;
        Error = null;
    } catch (java.net.SocketException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

set_traffic_class(S, TC, !IO) :-
    do_set_traffic_class(S, to_int(TC), IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_set_traffic_class(S::in, int::in, bool::out, throwable::out,
    io::di, io::uo) is det <= socket(S).
:- pragma foreign_proc("Java",
    do_set_traffic_class(S::in, TC::in, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.net.Socket) S).setTrafficClass(TC);
        IsOk = bool.YES;
        Error = null;
    } catch (java.net.SocketException e) {
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

:- pragma foreign_proc("Java",
    to_string(Sock::in, _IO::ui) = (Str::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Str = ((java.net.Socket) Sock).toString();
").

%---------------------------------------------------------------------------%
:- end_module socket.
%---------------------------------------------------------------------------%
