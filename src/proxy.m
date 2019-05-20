%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.net.Proxy.
%
%---------------------------------------------------------------------------%

:- module jnet.proxy.
:- interface.

:- import_module jlang.
:- import_module jlang.throwable.
:- import_module jnet.socket_address.

:- import_module maybe.

%---------------------------------------------------------------------------%

:- type proxy.

:- type proxy_type
    --->    direct
    ;       http
    ;       socks.

%---------------------------------------------------------------------------%

:- func new_proxy(proxy_type, SA) = maybe_error(proxy, throwable)
    <= socket_address(SA).

:- func no_proxy = proxy.

    % Returns 'no' if it is a direct connection.
    %
:- func address(proxy) = maybe(socket_address).

:- func to_string(proxy) = string.

:- func type(proxy) = proxy_type.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- interface.

:- pred proxy_equals(proxy::in, proxy::in) is semidet.

:- implementation.

:- import_module bool.

%---------------------------------------------------------------------------%

:- pragma foreign_decl("Java", "
    import java.net.Proxy;
    import java.net.Proxy.Type;
").

:- pragma foreign_type("Java", proxy, "java.net.Proxy")
    where equality is proxy_equals.

:- pragma foreign_export_enum("Java", proxy_type/0,
    [prefix("PROXY_TYPE_"), uppercase]).

:- pragma foreign_code("Java", "
    // public since do_new_proxy/5 below may be opt exported.
    public static java.net.Proxy.Type[] proxy_type_table = {
        java.net.Proxy.Type.DIRECT,
        java.net.Proxy.Type.HTTP,
        java.net.Proxy.Type.SOCKS
    };
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    proxy_equals(A::in, B::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = A.equals(B);
").

%---------------------------------------------------------------------------%

new_proxy(Type, SocketAddress) = Result :-
    do_new_proxy(Type, SocketAddress, IsOk, Proxy, Error),
    (
        IsOk = yes,
        Result = ok(Proxy)
    ;
        IsOk = no,
        Result = error(Error)
    ).

:- pred do_new_proxy(proxy_type::in, SA::in, bool::out, proxy::out,
    throwable::out) is det <= socket_address(SA).
:- pragma foreign_proc("Java",
    do_new_proxy(PT::in, SA::in, IsOk::out, P::out, Error::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        P = new java.net.Proxy(
            jmercury.jnet__proxy.proxy_type_table[PT.MR_value],
            ((java.net.SocketAddress) SA));
        IsOk = bool.YES;
        Error = null;
    } catch (java.lang.IllegalArgumentException e) {
        IsOk = bool.NO;
        P = null;
        Error = e;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    no_proxy = (P::out),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    P = java.net.Proxy.NO_PROXY;
").

%---------------------------------------------------------------------------%

address(P) = Result :-
    do_address(P, HaveResult, SA),
    (
        HaveResult = yes,
        Result = yes(SA)
    ;
        HaveResult = no,
        Result = no
    ).

:- pred do_address(proxy::in, bool::out, socket_address::out) is det.
:- pragma foreign_proc("Java",
    do_address(P::in, HaveResult::out, SA::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SA = P.address();
    HaveResult = (SA != null ? bool.YES : bool.NO);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(P::in) = (S::out),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    S = P.toString();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    type(P::in) = (T::out),
    [promise_pure, thread_safe, will_not_call_mercury],
"
    switch (P.type()) {
        case DIRECT:
            T = jmercury.jnet__proxy.PROXY_TYPE_DIRECT;
            break;
        case HTTP:
            T = jmercury.jnet__proxy.PROXY_TYPE_HTTP;
            break;
        case SOCKS:
            T = jmercury.jnet__proxy.PROXY_TYPE_SOCKS;
            break;
        default:
            throw new jmercury.runtime.UnreachableDefault(
                ""unknown proxy type"");
    }
").

%---------------------------------------------------------------------------%
:- end_module proxy.
%---------------------------------------------------------------------------%
