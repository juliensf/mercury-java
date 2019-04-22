%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.net.HttpURLConnection.
%
%---------------------------------------------------------------------------%

:- module jnet.http_url_connection.
:- interface.

:- import_module jnet.url_connection.

:- import_module io.

%---------------------------------------------------------------------------%

:- typeclass http_url_connection(T) <= url_connection(T) where [
    pred get_request_method(T::in, string::out, io::di, io::uo) is det
].

:- type http_url_connection.

:- instance url_connection(http_url_connection).
:- instance http_url_connection(http_url_connection).

    % Cast URLConnection -> HttpURLConnection.
    % False if the cast is not allowed.
    %
:- pred from_url_connection(url_connection::in, http_url_connection::out)
    is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module jio.
:- import_module jio.input_stream.
:- import_module jio.output_stream.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module bool.
:- import_module exception.
:- import_module int.
:- import_module maybe.
:- import_module require.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", http_url_connection,
    "java.net.HttpURLConnection").

:- instance url_connection(http_url_connection) where [
    pred(connect/3) is http_url_connection.connect,
    pred(set_connect_timeout/4) is http_url_connection.set_connect_timeout,
    pred(get_input_stream/4) is http_url_connection.get_input_stream,
    pred(get_output_stream/4) is http_url_connection.get_output_stream
].

:- instance http_url_connection(http_url_connection) where [
    pred(get_request_method/4) is http_url_connection_get_request_method
].

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    from_url_connection(U::in, H::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        H = (java.net.HttpURLConnection) U;
        SUCCESS_INDICATOR = true;
    } catch (java.lang.ClassCastException e) {
        H = null;
        SUCCESS_INDICATOR = false;
    }
").

%---------------------------------------------------------------------------%

:- pred connect(http_url_connection::in, io::di, io::uo) is det.

connect(UC, !IO) :-
    do_connect(UC, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_connect(http_url_connection::in, bool::out, throwable::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_connect(UC::in, IsOk::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        UC.connect();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pred set_connect_timeout(http_url_connection::in, int::in,
    io::di, io::uo) is det.

set_connect_timeout(UC, Timeout, !IO) :-
    ( if Timeout < 0 then
        unexpected("jnet.http_url_connection.set_connect_timeout",
            "timeout value is negative")
    else
        do_set_connect_timeout(UC, Timeout, !IO)
    ).

:- pred do_set_connect_timeout(http_url_connection::in, int::in,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_set_connect_timeout(UC::in, T::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    UC.setConnectTimeout(T);
").

%---------------------------------------------------------------------------%

:- pred get_input_stream(http_url_connection::in,
    maybe_error(jinput_stream, throwable)::out, io::di, io::uo) is det.

get_input_stream(UC, Result, !IO) :-
    do_get_input_stream(UC, IsOk, Stream, Error, !IO),
    (
        IsOk = no,
        Result = error(Error)
    ;
        IsOk = yes,
        Result = ok(Stream)
    ).

:- pred do_get_input_stream(http_url_connection::in, bool::out,
    jinput_stream::out, throwable::out, io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_get_input_stream(UC::in, IsOk::out, Stream::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        Stream = UC.getInputStream();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        Stream = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pred get_output_stream(http_url_connection::in,
    maybe_error(joutput_stream, throwable)::out, io::di, io::uo) is det.

get_output_stream(UC, Result, !IO) :-
    do_get_output_stream(UC, IsOk, Stream, Error, !IO),
    (
        IsOk = no,
        Result = error(Error)
    ;
        IsOk = yes,
        Result = ok(Stream)
    ).

:- pred do_get_output_stream(http_url_connection::in, bool::out,
    joutput_stream::out, throwable::out, io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_get_output_stream(UC::in, IsOk::out, Stream::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        Stream = UC.getOutputStream();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        Stream = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pred http_url_connection_get_request_method(http_url_connection::in,
    string::out, io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    http_url_connection_get_request_method(U::in, M::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    M = U.getRequestMethod();
").

%---------------------------------------------------------------------------%
:- end_module http_url_connection.
%---------------------------------------------------------------------------%
