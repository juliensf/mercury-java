%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.net.URL.
%
%---------------------------------------------------------------------------%

:- module jnet.url.
:- interface.

:- import_module jnet.uri.

:- import_module maybe.

%---------------------------------------------------------------------------%

:- type url.

:- func url(string) = url.

:- func get_authority(url) = string.

:- func get_default_port(url) = maybe(uint16).

:- func get_file(url) = string.

:- func get_host(url) = string.

:- func get_path(url) = string.

:- func get_port(url) = maybe(uint16).

:- func get_protocol(url) = string.

:- func get_query(url) = maybe(string).

:- func get_ref(url) = maybe(string).

:- func get_user_info(url) = maybe(string).

:- func to_uri(url) = uri.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module jlang.
:- import_module jlang.throwable.

:- import_module bool.
:- import_module exception.
:- import_module uint16.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", url, "java.net.URL").

%---------------------------------------------------------------------------%

url(String) = URL :-
    do_new_url(String, URL, Ok, Err),
    (
        Ok = no,
        throw(java_exception(Err))
    ;
        Ok = yes
    ).

:- pred do_new_url(string::in, url::out, bool::out, throwable::out) is det.

:- pragma foreign_proc("Java",
    do_new_url(S::in, U::out, Ok::out, Err::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        U = new java.net.URL(S);
        Ok = bool.YES;
        Err = null;
    } catch (java.net.MalformedURLException e) {
        Ok = bool.NO;
        U = null;
        Err = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_authority(U::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = U.getAuthority();
").

%---------------------------------------------------------------------------%

get_default_port(URL) = Result :-
    do_get_default_port(URL, RawPort),
    ( if RawPort = -1 then
        Result = no
    else
        Port = uint16.det_from_int(RawPort),
        Result = yes(Port)
    ).

:- pred do_get_default_port(url::in, int::out) is det.
:- pragma foreign_proc("Java",
    do_get_default_port(U::in, P::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    P = U.getDefaultPort();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_file(U::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = U.getFile();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_host(U::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = U.getHost();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_path(U::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = U.getPath();
").

%---------------------------------------------------------------------------%

get_port(URL) = Result :-
    do_get_port(URL, RawPort),
    ( if RawPort = -1 then
        Result = no
    else
        Port = uint16.det_from_int(RawPort),
        Result = yes(Port)
    ).

:- pred do_get_port(url::in, int::out) is det.
:- pragma foreign_proc("Java",
    do_get_port(U::in, P::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    P = U.getPort();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_protocol(U::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = U.getProtocol();
").

%---------------------------------------------------------------------------%

get_query(URL) = Result :-
    do_get_query(URL, Ok, Query),
    (
        Ok = no,
        Result = no
    ;
        Ok = yes,
        Result = yes(Query)
    ).

:- pred do_get_query(url::in, bool::out, string::out) is det.
:- pragma foreign_proc("Java",
    do_get_query(U::in, Ok::out, Q::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Q = U.getQuery();
    Ok = (Q == null) ? bool.NO : bool.YES;
").

%---------------------------------------------------------------------------%

get_ref(URL) = Result :-
    do_get_ref(URL, Ok, Ref),
    (
        Ok = no,
        Result = no
    ;
        Ok = yes,
        Result = yes(Ref)
    ).

:- pred do_get_ref(url::in, bool::out, string::out) is det.
:- pragma foreign_proc("Java",
    do_get_ref(U::in, Ok::out, R::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    R = U.getRef();
    Ok = (R == null) ? bool.NO : bool.YES;
").

%---------------------------------------------------------------------------%

get_user_info(URL) = Result :-
    do_get_user_info(URL, Ok, UserInfo),
    (
        Ok = no,
        Result = no
    ;
        Ok = yes,
        Result = yes(UserInfo)
    ).

:- pred do_get_user_info(url::in, bool::out, string::out) is det.
:- pragma foreign_proc("Java",
    do_get_user_info(U::in, Ok::out, UI::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    UI = U.getUserInfo();
    Ok = (UI == null) ? bool.NO : bool.YES;
").

%---------------------------------------------------------------------------%

to_uri(URL) = URI :-
    do_to_uri(URL, URI, Ok, Err),
    (
        Ok = no,
        throw(java_exception(Err))
    ;
        Ok = yes
    ).

:- pred do_to_uri(url::in, uri::out, bool::out, throwable::out) is det.
:- pragma foreign_proc("Java",
    do_to_uri(Url::in, Uri::out, Ok::out, Err::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        Uri = Url.toURI();
        Ok = bool.YES;
        Err = null;
    } catch (java.net.URISyntaxException e) {
        Uri = null;
        Ok = bool.NO;
        Err = e;
    }
").

%---------------------------------------------------------------------------%
:- end_module url.
%---------------------------------------------------------------------------%
