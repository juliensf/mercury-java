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

:- import_module jio.
:- import_module jio.input_stream.
:- import_module jio.output_stream.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module jnet.url_connection.

:- import_module bool.
:- import_module io.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- typeclass http_url_connection(T) <= url_connection(T) where [].

:- type http_url_connection.

:- instance url_connection(http_url_connection).
:- instance http_url_connection(http_url_connection).

    % Cast URLConnection -> HttpURLConnection.
    % False if the cast is not allowed.
    %
:- pred from_url_connection(url_connection::in, http_url_connection::out)
    is semidet.

%---------------------------------------------------------------------------%

:- pred add_request_property(T::in, string::in, string::in,
    io::di, io::uo) is det <= http_url_connection(T).

:- pred connect(T::in, io::di, io::uo) is det <= http_url_connection(T).

:- pred get_allow_user_interaction(T::in, bool::out, io::di, io::uo)
    is det <= http_url_connection(T).

:- pred get_connect_timeout(T::in, int::out, io::di, io::uo)
    is det <= http_url_connection(T).

:- pred get_content_encoding(T::in, maybe(string)::out, io::di, io::uo)
    is det <= http_url_connection(T).

:- pred get_content_length(T::in, maybe(int64)::out, io::di, io::uo)
    is det <= http_url_connection(T).

:- pred get_content_type(T::in, maybe(string)::out, io::di, io::uo)
    is det <= url_connection(T).

:- pred get_date(T::in, maybe(int64)::out, io::di, io::uo)
    is det <= url_connection(T).

:- pred get_expiration(T::in, maybe(int64)::out, io::di, io::uo)
    is det <= url_connection(T).

:- pred get_request_method(T::in, string::out, io::di, io::uo)
    is det <= http_url_connection(T).

:- pred get_if_modified_since(T::in, int64::out, io::di, io::uo)
    is det <= http_url_connection(T).

:- pred get_input_stream(T::in, maybe_error(jinput_stream, throwable)::out,
    io::di, io::uo) is det <= http_url_connection(T).

:- pred get_output_stream(T::in, maybe_error(joutput_stream, throwable)::out,
    io::di, io::uo) is det <= http_url_connection(T).

:- pred get_read_timeout(T::in, int::out, io::di, io::uo)
    is det <= http_url_connection(T).

:- pred set_connect_timeout(T::in, int::in, io::di, io::uo)
    is det <= http_url_connection(T).

:- pred set_read_timeout(T::in, int::in, io::di, io::uo)
    is det <= http_url_connection(T).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.


:- import_module bool.
:- import_module exception.
:- import_module int.
:- import_module require.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", http_url_connection,
    "java.net.HttpURLConnection").

:- instance url_connection(http_url_connection) where [].
:- instance http_url_connection(http_url_connection) where [].

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

add_request_property(UC, K, V, !IO) :-
    url_connection.add_request_property(UC, K, V, !IO).

connect(UC, !IO) :-
    url_connection.connect(UC, !IO).

get_allow_user_interaction(UC, AUI, !IO) :-
    url_connection.get_allow_user_interaction(UC, AUI, !IO).

get_connect_timeout(UC, Timeout, !IO) :-
    url_connection.get_connect_timeout(UC, Timeout, !IO).

get_content_encoding(UC, CE, !IO) :-
    url_connection.get_content_encoding(UC, CE, !IO).

get_content_length(UC, CL, !IO) :-
    url_connection.get_content_length(UC, CL, !IO).

get_content_type(UC, CT, !IO) :-
    url_connection.get_content_type(UC, CT, !IO).

get_date(UC, D, !IO) :-
    url_connection.get_date(UC, D, !IO).

get_expiration(UC, E, !IO) :-
    url_connection.get_expiration(UC, E, !IO).

get_if_modified_since(UC, IMS, !IO) :-
    url_connection.get_if_modified_since(UC, IMS, !IO).

get_input_stream(UC, Result, !IO) :-
    url_connection.get_input_stream(UC, Result, !IO).

get_output_stream(UC, Result, !IO) :-
    url_connection.get_output_stream(UC, Result, !IO).

get_read_timeout(UC, Timeout, !IO) :-
    url_connection.get_read_timeout(UC, Timeout, !IO).

set_connect_timeout(UC, Timeout, !IO) :-
    url_connection.set_connect_timeout(UC, Timeout, !IO).

set_read_timeout(UC, Timeout, !IO) :-
    url_connection.set_read_timeout(UC, Timeout, !IO).

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_request_method(U::in, M::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    M = ((java.net.HttpURLConnection) U).getRequestMethod();
").

%---------------------------------------------------------------------------%
:- end_module http_url_connection.
%---------------------------------------------------------------------------%
