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

:- typeclass url_connection(T) where [].

:- pred connect(T::in, io::di, io::uo) is det <= url_connection(T).

:- pred set_connect_timeout(T::in, int::in, io::di, io::uo) is det
    <= url_connection(T).

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

:- pred get_input_stream(T::in, maybe_error(jinput_stream, throwable)::out,
    io::di, io::uo) is det <= url_connection(T).

:- pred get_output_stream(T::in, maybe_error(joutput_stream, throwable)::out,
    io::di, io::uo) is det <= url_connection(T).

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

:- instance url_connection(url_connection) where [].

%---------------------------------------------------------------------------%

connect(UC, !IO) :-
    do_connect(UC, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_connect(T::in, bool::out, throwable::out,
    io::di, io::uo) is det <= url_connection(T).
:- pragma foreign_proc("Java",
    do_connect(UC::in, IsOk::out, Error::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.net.URLConnection) UC).connect();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

set_connect_timeout(UC, Timeout, !IO) :-
    ( if Timeout < 0 then
        unexpected("jnet.url_connection.set_connect_timeout",
            "timeout value is negative")
    else
        do_set_connect_timeout(UC, Timeout, !IO)
    ).

:- pred do_set_connect_timeout(T::in, int::in,
    io::di, io::uo) is det <= url_connection(T).
:- pragma foreign_proc("Java",
    do_set_connect_timeout(UC::in, T::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ((java.net.URLConnection) UC).setConnectTimeout(T);
").

%---------------------------------------------------------------------------%

get_input_stream(UC, Result, !IO) :-
    do_get_input_stream(UC, IsOk, Stream, Error, !IO),
    (
        IsOk = no,
        Result = error(Error)
    ;
        IsOk = yes,
        Result = ok(Stream)
    ).

:- pred do_get_input_stream(T::in, bool::out, jinput_stream::out,
    throwable::out, io::di, io::uo) is det <= url_connection(T).
:- pragma foreign_proc("Java",
    do_get_input_stream(UC::in, IsOk::out, Stream::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        Stream = ((java.net.URLConnection) UC).getInputStream();
        IsOk = bool.YES;
        Error = null;
    } catch (java.io.IOException e) {
        Stream = null;
        IsOk = bool.NO;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

get_output_stream(UC, Result, !IO) :-
    do_get_output_stream(UC, IsOk, Stream, Error, !IO),
    (
        IsOk = no,
        Result = error(Error)
    ;
        IsOk = yes,
        Result = ok(Stream)
    ).

:- pred do_get_output_stream(T::in, bool::out, joutput_stream::out,
    throwable::out, io::di, io::uo) is det <= url_connection(T).
:- pragma foreign_proc("Java",
    do_get_output_stream(UC::in, IsOk::out, Stream::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        Stream = ((java.net.URLConnection) UC).getOutputStream();
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
