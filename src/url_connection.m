%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.net.URLConnection.
%
%---------------------------------------------------------------------------%

:- module jnet.url_connection.
:- interface.

:- import_module jio.
:- import_module jio.input_stream.
:- import_module jio.output_stream.
:- import_module jlang.
:- import_module jlang.throwable.
%:- import_module jnet.url.

:- import_module io.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- typeclass url_connection(T) where [
    pred connect(T::in, io::di, io::uo) is det,
    pred set_connect_timeout(T::in, int::in, io::di, io::uo) is det,
    %pred get_connect_timeout(T::in, int::out, io::di, io::uo) is det,
    %pred set_read_timeout(T::in, int::in, io::di, io::uo) is det,
    %pred get_read_timeout(T::in, int::out, io::di, io::uo) is det,
    %pred get_url(T::in, url::out, io::di, io::uo) is det,
    %pred get_content_length(T::in, maybe(int64)::out, io::di, io::uo) is det,
    %pred get_content_type(T::in, maybe(string)::out, io::di, io::uo) is det,
    %pred get_content_encoding(T::in, maybe(string)::out,
    %    io::di, io::uo) is det,
    %pred get_expiration(T::in, maybe(int64)::out, io::di, io::uo) is det,
    %pred get_date(T::in, maybe(int64)::out, io::di, io::uo) is det,
    %pred get_last_modified(T::in, maybe(int64)::out, io::di, io::uo) is det,
    %pred get_header_field(T::in, string::in, maybe(string)::out,
    %    io::di, io::uo) is det,
    pred get_input_stream(T::in, maybe_error(jinput_stream, throwable)::out,
        io::di, io::uo) is det,
    pred get_output_stream(T::in, maybe_error(joutput_stream, throwable)::out,
        io::di, io::uo) is det
].

%---------------------------------------------------------------------------%

:- type url_connection.

:- instance url_connection(url_connection).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module exception.
:- import_module require.

:- pragma foreign_type("Java", url_connection, "java.net.URLConnection").

%---------------------------------------------------------------------------%

:- instance url_connection(url_connection) where [
    pred(connect/3) is url_connection_connect,
    pred(set_connect_timeout/4) is url_connection_set_connect_timeout,
    %pred(get_connect_timeout/4) is url_connection_get_connect_timeout,
    %pred(set_read_timeout/4) is url_connection_set_read_timeout,
    %pred(get_read_timeout/4) is url_connection_get_read_timeout,
    %pred(get_url/4) is url_connection_get_url,
    %pred(get_content_length/4) is url_connection_get_content_length,
    %pred(get_content_type/4) is url_connection_get_content_type,
    %pred(get_content_encoding/4) is url_connection_get_content_encoding,
    %pred(get_expiration/4) is url_connection_get_expiration,
    %pred(get_date/4) is url_connection_get_date,
    %pred(get_last_modified/4) is url_connection_get_last_modified,
    %pred(get_header_field/5) is url_connection_get_header_field,
    pred(get_input_stream/4) is url_connection_get_input_stream,
    pred(get_output_stream/4) is url_connection_get_output_stream
].

%---------------------------------------------------------------------------%

:- pred url_connection_connect(url_connection::in, io::di, io::uo) is det.

url_connection_connect(UC, !IO) :-
    do_connect(UC, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_connect(url_connection::in, bool::out, throwable::out,
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

:- pred url_connection_set_connect_timeout(url_connection::in, int::in,
    io::di, io::uo) is det.

url_connection_set_connect_timeout(UC, Timeout, !IO) :-
    ( if Timeout < 0 then
        unexpected("jnet.url_connection.set_connect_timeout",
            "timeout value is negative")
    else
        do_set_connect_timeout(UC, Timeout, !IO)
    ).

:- pred do_set_connect_timeout(url_connection::in, int::in,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    do_set_connect_timeout(UC::in, T::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    UC.setConnectTimeout(T);
").

%---------------------------------------------------------------------------%

:- pred url_connection_get_input_stream(url_connection::in,
    maybe_error(jinput_stream, throwable)::out, io::di, io::uo) is det.

url_connection_get_input_stream(UC, Result, !IO) :-
    do_get_input_stream(UC, IsOk, Stream, Error, !IO),
    (
        IsOk = no,
        Result = error(Error)
    ;
        IsOk = yes,
        Result = ok(Stream)
    ).

:- pred do_get_input_stream(url_connection::in, bool::out,
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

:- pred url_connection_get_output_stream(url_connection::in,
    maybe_error(joutput_stream, throwable)::out, io::di, io::uo) is det.

url_connection_get_output_stream(UC, Result, !IO) :-
    do_get_output_stream(UC, IsOk, Stream, Error, !IO),
    (
        IsOk = no,
        Result = error(Error)
    ;
        IsOk = yes,
        Result = ok(Stream)
    ).

:- pred do_get_output_stream(url_connection::in, bool::out,
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
:- end_module url_connection.
%---------------------------------------------------------------------------%
