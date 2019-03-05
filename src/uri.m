%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.net.URI.
%
%---------------------------------------------------------------------------%

:- module uri.
:- interface.

:- import_module maybe.

%---------------------------------------------------------------------------%

:- type uri.

:- func uri(string) = uri.

:- func get_authority(uri) = maybe(string).

:- func get_fragment(uri) = maybe(string).

:- func get_host(uri) = maybe(string).

:- func get_port(uri) = maybe(uint16).

:- func get_query(uri) = maybe(string).

:- func get_raw_authority(uri) = maybe(string).

:- func get_raw_path(uri) = maybe(string).

:- func get_raw_query(uri) = maybe(string).

:- func get_raw_scheme_specific_part(uri) = maybe(string).

:- func get_raw_user_info(uri) = maybe(string).

:- func get_scheme(uri) = maybe(string).

:- func get_scheme_specific_part(uri) = maybe(string).

:- func get_user_info(uri) = maybe(string).

:- pred is_absolute(uri::in) is semidet.

:- pred is_opaque(uri::in) is semidet.

:- func normalize(uri) = uri.

:- func to_ascii_string(uri) = string.

:- func to_string(uri) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.
:- import_module uint16.

:- pragma foreign_type("Java", uri, "java.net.URI").

%---------------------------------------------------------------------------%

uri(String) = URI :-
    do_new_uri(String, URI, Ok, ErrMsg),
    (
        Ok = no,
        throw(software_error(ErrMsg))
    ;
        Ok = yes
    ).

:- pred do_new_uri(string::in, uri::out, bool::out, string::out) is det.

:- pragma foreign_proc("Java",
    do_new_uri(S::in, U::out, Ok::out, ErrMsg::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        U = new java.net.URI(S);
        Ok = bool.YES;
        ErrMsg = """";
    } catch (java.net.URISyntaxException e) {
        Ok = bool.NO;
        U = null;
        ErrMsg = e.getMessage();
    }
").

%---------------------------------------------------------------------------%

get_authority(URI) = Result :-
    do_get_authority(URI, Ok, Authority),
    (
        Ok = no,
        Result = no
    ;
        Ok = yes,
        Result = yes(Authority)
    ).

:- pred do_get_authority(uri::in, bool::out, string::out) is det.

:- pragma foreign_proc("Java",
    do_get_authority(U::in, Ok::out, V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = U.getAuthority();
    Ok = (V == null) ? bool.NO : bool.YES;
").

%---------------------------------------------------------------------------%

get_fragment(URI) = Result :-
    do_get_fragment(URI, Ok, Fragment),
    (
        Ok = no,
        Result = no
    ;
        Ok = yes,
        Result = yes(Fragment)
    ).

:- pred do_get_fragment(uri::in, bool::out, string::out) is det.

:- pragma foreign_proc("Java",
    do_get_fragment(U::in, Ok::out, V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = U.getFragment();
    Ok = (V == null) ? bool.NO : bool.YES;
").

%---------------------------------------------------------------------------%

get_host(URI) = Result :-
    do_get_host(URI, Ok, Host),
    (
        Ok = no,
        Result = no
    ;
        Ok = yes,
        Result = yes(Host)
    ).

:- pred do_get_host(uri::in, bool::out, string::out) is det.

:- pragma foreign_proc("Java",
    do_get_host(U::in, Ok::out, V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = U.getHost();
    Ok = (V == null) ? bool.NO : bool.YES;
").

%---------------------------------------------------------------------------%

get_port(URI) = Result :-
    do_get_port(URI, Ok, RawPort),
    (
        Ok = no,
        Result = no
    ;
        Ok = yes,
        Port = uint16.det_from_int(RawPort),
        Result = yes(Port)
    ).

:- pred do_get_port(uri::in, bool::out, int::out) is det.

:- pragma foreign_proc("Java",
    do_get_port(U::in, Ok::out, V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = U.getPort();
    Ok = (V == -1) ? bool.NO : bool.YES;
").

%---------------------------------------------------------------------------%

get_query(URI) = Result :-
    do_get_query(URI, Ok, Query),
    (
        Ok = no,
        Result = no
    ;
        Ok = yes,
        Result = yes(Query)
    ).

:- pred do_get_query(uri::in, bool::out, string::out) is det.

:- pragma foreign_proc("Java",
    do_get_query(U::in, Ok::out, V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = U.getQuery();
    Ok = (V == null) ? bool.NO : bool.YES;
").

%---------------------------------------------------------------------------%

get_raw_authority(URI) = Result :-
    do_get_raw_authority(URI, Ok, RawAuthority),
    (
        Ok = no,
        Result = no
    ;
        Ok = yes,
        Result = yes(RawAuthority)
    ).

:- pred do_get_raw_authority(uri::in, bool::out, string::out) is det.

:- pragma foreign_proc("Java",
    do_get_raw_authority(U::in, Ok::out, V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = U.getRawAuthority();
    Ok = (V == null) ? bool.NO : bool.YES;
").

%---------------------------------------------------------------------------%

get_raw_path(URI) = Result :-
    do_get_raw_path(URI, Ok, RawPath),
    (
        Ok = no,
        Result = no
    ;
        Ok = yes,
        Result = yes(RawPath)
    ).

:- pred do_get_raw_path(uri::in, bool::out, string::out) is det.

:- pragma foreign_proc("Java",
    do_get_raw_path(U::in, Ok::out, V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = U.getRawPath();
    Ok = (V == null) ? bool.NO : bool.YES;
").

%---------------------------------------------------------------------------%

get_raw_query(URI) = Result :-
    do_get_raw_query(URI, Ok, RawQuery),
    (
        Ok = no,
        Result = no
    ;
        Ok = yes,
        Result = yes(RawQuery)
    ).

:- pred do_get_raw_query(uri::in, bool::out, string::out) is det.

:- pragma foreign_proc("Java",
    do_get_raw_query(U::in, Ok::out, V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = U.getRawQuery();
    Ok = (V == null) ? bool.NO : bool.YES;
").

%---------------------------------------------------------------------------%

get_raw_scheme_specific_part(URI) = Result :-
    do_get_raw_scheme_specific_part(URI, Ok, RawSchemeSpecificPart),
    (
        Ok = no,
        Result = no
    ;
        Ok = yes,
        Result = yes(RawSchemeSpecificPart)
    ).

:- pred do_get_raw_scheme_specific_part(uri::in, bool::out, string::out) is det.

:- pragma foreign_proc("Java",
    do_get_raw_scheme_specific_part(U::in, Ok::out, V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = U.getRawSchemeSpecificPart();
    Ok = (V == null) ? bool.NO : bool.YES;
").

%---------------------------------------------------------------------------%

get_raw_user_info(URI) = Result :-
    do_get_raw_user_info(URI, Ok, RawUserInfo),
    (
        Ok = no,
        Result = no
    ;
        Ok = yes,
        Result = yes(RawUserInfo)
    ).

:- pred do_get_raw_user_info(uri::in, bool::out, string::out) is det.

:- pragma foreign_proc("Java",
    do_get_raw_user_info(U::in, Ok::out, V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = U.getRawUserInfo();
    Ok = (V == null) ? bool.NO : bool.YES;
").

%---------------------------------------------------------------------------%

get_scheme(URI) = Result :-
    do_get_scheme(URI, Ok, Scheme),
    (
        Ok = no,
        Result = no
    ;
        Ok = yes,
        Result = yes(Scheme)
    ).

:- pred do_get_scheme(uri::in, bool::out, string::out) is det.

:- pragma foreign_proc("Java",
    do_get_scheme(U::in, Ok::out, V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = U.getScheme();
    Ok = (V == null) ? bool.NO : bool.YES;
").

%---------------------------------------------------------------------------%

get_scheme_specific_part(URI) = Result :-
    do_get_scheme_specific_part(URI, Ok, SchemeSpecificPart),
    (
        Ok = no,
        Result = no
    ;
        Ok = yes,
        Result = yes(SchemeSpecificPart)
    ).

:- pred do_get_scheme_specific_part(uri::in, bool::out, string::out) is det.

:- pragma foreign_proc("Java",
    do_get_scheme_specific_part(U::in, Ok::out, V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = U.getSchemeSpecificPart();
    Ok = (V == null) ? bool.NO : bool.YES;
").

%---------------------------------------------------------------------------%

get_user_info(URI) = Result :-
    do_get_user_info(URI, Ok, UserInfo),
    (
        Ok = no,
        Result = no
    ;
        Ok = yes,
        Result = yes(UserInfo)
    ).

:- pred do_get_user_info(uri::in, bool::out, string::out) is det.

:- pragma foreign_proc("Java",
    do_get_user_info(U::in, Ok::out, V::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    V = U.getUserInfo();
    Ok = (V == null) ? bool.NO : bool.YES;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_absolute(U::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = U.isAbsolute();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_opaque(U::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = U.isOpaque();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    normalize(U::in) = (N::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    N = U.normalize();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_ascii_string(U::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = U.toASCIIString();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(U::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = U.toString();
").

%---------------------------------------------------------------------------%
:- end_module uri.
%---------------------------------------------------------------------------%
