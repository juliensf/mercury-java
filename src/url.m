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

:- module url.
:- interface.

:- import_module jnet.uri.

%:- import_module maybe.

%---------------------------------------------------------------------------%

:- type url.

:- func url(string) = url.

:- func get_authority(url) = string.

/*
:- func get_default_port(url) = maybe(uint16).

:- func get_file(url)

:- func get_host(url)

:- func get_path(url)

:- func get_port(url)

:- func get_protocol(url)

:- func get_query(url)

:- func get_refurl(url)

:- func get_user_info(url)
*/

:- func to_uri(url) = uri.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module exception.

:- pragma foreign_type("Java", url, "java.net.URL").

%---------------------------------------------------------------------------%

url(String) = URL :-
    do_new_url(String, URL, Ok, ErrMsg),
    (
        Ok = no,
        throw(software_error(ErrMsg))
    ;
        Ok = yes
    ).

:- pred do_new_url(string::in, url::out, bool::out, string::out) is det.

:- pragma foreign_proc("Java",
    do_new_url(S::in, U::out, Ok::out, ErrMsg::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        U = new java.net.URL(S);
        Ok = bool.YES;
        ErrMsg = """";
    } catch (java.net.MalformedURLException e) {
        Ok = bool.NO;
        U = null;
        ErrMsg = e.getMessage();
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

to_uri(URL) = URI :-
    do_to_uri(URL, URI, Ok, ErrMsg),
    (
        Ok = no,
        throw(software_error(ErrMsg))
    ;
        Ok = yes
    ).

:- pred do_to_uri(url::in, uri::out, bool::out, string::out) is det.
:- pragma foreign_proc("Java",
    do_to_uri(Url::in, Uri::out, Ok::out, ErrMsg::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        Uri = Url.toURI();
        Ok = bool.YES;
        ErrMsg = """";
    } catch (java.net.URISyntaxException e) {
        Uri = null;
        Ok = bool.NO;
        ErrMsg = e.getMessage();
    }
").

%---------------------------------------------------------------------------%
:- end_module url.
%---------------------------------------------------------------------------%
