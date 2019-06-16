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
:- import_module jnet.url.

:- import_module bool.
:- import_module io.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- type url_connection.
:- typeclass url_connection(T) where [].
:- instance url_connection(url_connection).

%---------------------------------------------------------------------------%

:- pred add_request_property(T::in, string::in, string::in,
    io::di, io::uo) is det <= url_connection(T).

:- pred connect(T::in, io::di, io::uo) is det <= url_connection(T).

:- pred get_allow_user_interaction(T::in, bool::out, io::di, io::uo)
    is det <= url_connection(T).

:- pred get_connect_timeout(T::in, int::out, io::di, io::uo)
    is det <= url_connection(T).

:- pred get_content_encoding(T::in, maybe(string)::out, io::di, io::uo)
    is det <= url_connection(T).

:- pred get_content_length(T::in, maybe(int64)::out, io::di, io::uo)
    is det <= url_connection(T).

:- pred get_content_type(T::in, maybe(string)::out, io::di, io::uo)
    is det <= url_connection(T).

:- pred get_date(T::in, maybe(int64)::out, io::di, io::uo)
    is det <= url_connection(T).

:- pred get_expiration(T::in, maybe(int64)::out, io::di, io::uo)
    is det <= url_connection(T).

:- pred get_header_field(T::in, string::in, maybe(string)::out,
    io::di, io::uo) is det <= url_connection(T).

:- pred get_if_modified_since(T::in, int64::out, io::di, io::uo)
    is det <= url_connection(T).

:- pred get_input_stream(T::in, maybe_error(jinput_stream, throwable)::out,
    io::di, io::uo) is det <= url_connection(T).

:- pred get_last_modified(T::in, maybe(int64)::out, io::di, io::uo)
    is det <= url_connection(T).

:- pred get_output_stream(T::in, maybe_error(joutput_stream, throwable)::out,
    io::di, io::uo) is det <= url_connection(T).

:- pred get_url(T::in, url::out, io::di, io::uo)
    is det <= url_connection(T).

:- pred set_connect_timeout(T::in, int::in, io::di, io::uo) is det
    <= url_connection(T).

    %pred set_read_timeout(T::in, int::in, io::di, io::uo) is det,
    %pred get_read_timeout(T::in, int::out, io::di, io::uo) is det,

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module int64.
:- import_module exception.
:- import_module require.

:- pragma foreign_type("Java", url_connection, "java.net.URLConnection").

%---------------------------------------------------------------------------%

:- instance url_connection(url_connection) where [].

%---------------------------------------------------------------------------%

add_request_property(UC, K, V, !IO) :-
    do_add_request_property(UC, K, V, IsOk, Error, !IO),
    (
        IsOk = yes
    ;
        IsOk = no,
        throw(java_exception(Error))
    ).

:- pred do_add_request_property(T::in, string::in, string::in, bool::out,
    throwable::out, io::di, io::uo) is det <= url_connection(T).
:- pragma foreign_proc("Java",
    do_add_request_property(UC::in, K::in, V::in, IsOk::out, Error::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        ((java.net.URLConnection) UC).addRequestProperty(K, V);
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.IllegalStateException e) {
        IsOk = bool.NO;
        Error = e;
    }
").

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

:- pragma foreign_proc("Java",
    get_allow_user_interaction(UC::in, AUI::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    if (((java.net.URLConnection) UC).getAllowUserInteraction()) {
        AUI = bool.YES;
    } else {
        AUI = bool.NO;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_connect_timeout(UC::in, Timeout::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Timeout = ((java.net.URLConnection) UC).getConnectTimeout();
").

%---------------------------------------------------------------------------%

get_content_encoding(UC, MaybeEncoding, !IO) :-
    do_get_content_encoding(UC, HaveEncoding, Encoding, !IO),
    (
        HaveEncoding = yes,
        MaybeEncoding = yes(Encoding)
    ;
        HaveEncoding = no,
        MaybeEncoding = no
    ).

:- pred do_get_content_encoding(T::in, bool::out, string::out, io::di ,io::uo)
    is det <= url_connection(T).
:- pragma foreign_proc("Java",
    do_get_content_encoding(UC::in, HaveEncoding::out, Encoding::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Encoding = ((java.net.URLConnection) UC).getContentEncoding();
    HaveEncoding = (Encoding != null ? bool.YES : bool.NO);
").

%---------------------------------------------------------------------------%

get_content_length(UC, MaybeLength, !IO) :-
    do_get_content_length(UC, Length, !IO),
    ( if Length > -1i64  then
        MaybeLength = yes(Length)
    else
        MaybeLength = no
    ).

:- pred do_get_content_length(T::in, int64::out, io::di, io::uo) is det
    <= url_connection(T).
:- pragma foreign_proc("Java",
    do_get_content_length(UC::in, L::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    L = ((java.net.URLConnection) UC).getContentLengthLong();
").

%---------------------------------------------------------------------------%

get_content_type(UC, MaybeType, !IO) :-
    do_get_content_type(UC, HaveType, Type, !IO),
    (
        HaveType = yes,
        MaybeType = yes(Type)
    ;
        HaveType = no,
        MaybeType = no
    ).

:- pred do_get_content_type(T::in, bool::out, string::out, io::di ,io::uo)
    is det <= url_connection(T).
:- pragma foreign_proc("Java",
    do_get_content_type(UC::in, HaveType::out, Type::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Type = ((java.net.URLConnection) UC).getContentType();
    HaveType = (Type != null ? bool.YES : bool.NO);
").

%---------------------------------------------------------------------------%

get_date(UC, MaybeDate, !IO) :-
    do_get_date(UC, Date, !IO),
    ( if Date > 0i64 then
        MaybeDate = yes(Date)
    else
        MaybeDate = no
    ).

:- pred do_get_date(T::in, int64::out, io::di, io::uo) is det
    <= url_connection(T).
:- pragma foreign_proc("Java",
    do_get_date(UC::in, D::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    D = ((java.net.URLConnection) UC).getDate();
").

%---------------------------------------------------------------------------%

get_expiration(UC, MaybeExpiration, !IO) :-
    do_get_expiration(UC, Expiration, !IO),
    ( if Expiration > 0i64 then
        MaybeExpiration = yes(Expiration)
    else
        MaybeExpiration = no
    ).

:- pred do_get_expiration(T::in, int64::out, io::di, io::uo) is det
    <= url_connection(T).
:- pragma foreign_proc("Java",
    do_get_expiration(UC::in, E::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    E = ((java.net.URLConnection) UC).getExpiration();
").

%---------------------------------------------------------------------------%

get_header_field(UC, Name, MaybeValue, !IO) :-
    do_get_header_field(UC, Name, HaveValue, Value, !IO),
    (
        HaveValue = yes,
        MaybeValue = yes(Value)
    ;
        HaveValue = no,
        MaybeValue = no
    ).

:- pred do_get_header_field(T::in, string::in, bool::out, string::out,
    io::di, io::uo) is det <= url_connection(T).
:- pragma foreign_proc("Java",
    do_get_header_field(UC::in, Name::in, HaveValue::out, Value::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Value = ((java.net.URLConnection) UC).getHeaderField(Name);
    HaveValue = (Value != null ? bool.YES : bool.NO);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_if_modified_since(UC::in, IMS::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    IMS = ((java.net.URLConnection) UC).getIfModifiedSince();
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

get_last_modified(UC, MaybeLM, !IO) :-
    do_get_last_modified(UC, LM, !IO),
    ( if LM > 0i64 then
        MaybeLM = yes(LM)
    else
        MaybeLM = no
    ).

:- pred do_get_last_modified(T::in, int64::out, io::di, io::uo) is det
    <= url_connection(T).
:- pragma foreign_proc("Java",
    do_get_last_modified(UC::in, LM::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    LM = ((java.net.URLConnection) UC).getLastModified();
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

:- pragma foreign_proc("Java",
    get_url(UC::in, U::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    U = ((java.net.URLConnection) UC).getURL();
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
:- end_module url_connection.
%---------------------------------------------------------------------------%
