%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.net.Inet6Address.
%
%---------------------------------------------------------------------------%

:- module jnet.inet6_address.
:- interface.

:- import_module jnet.inet_address.

:- import_module io.

%---------------------------------------------------------------------------%

:- type inet6_address.

:- instance inet_address(inet6_address).

:- pred from_inet_address(inet_address::in, inet6_address::out) is semidet.

:- func to_inet_address(inet6_address) = inet_address.

%---------------------------------------------------------------------------%

:- pred get_canonical_host_name(inet6_address::in, string::out,
    io::di, io::uo) is det.

:- pred get_host_address(inet6_address::in, string::out,
    io::di, io::uo) is det.

:- pred get_host_name(inet6_address::in, string::out,
    io::di, io::uo) is det.

:- pred is_any_local_address(inet6_address::in) is semidet.

:- pred is_link_local_address(inet6_address::in) is semidet.

:- pred is_loopback_address(inet6_address::in) is semidet.

:- pred is_mc_global(inet6_address::in) is semidet.

:- pred is_mc_link_local(inet6_address::in) is semidet.

:- pred is_mc_node_local(inet6_address::in) is semidet.

:- pred is_mc_org_local(inet6_address::in) is semidet.

:- pred is_mc_site_local(inet6_address::in) is semidet.

:- pred is_multicast_address(inet6_address::in) is semidet.

:- func to_string(inet6_address) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", inet6_address, "java.net.Inet6Address").

:- instance inet_address(inet6_address) where [].

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    from_inet_address(IP::in, IP6::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        IP6 = (java.net.Inet6Address) IP;
        SUCCESS_INDICATOR = true;
    } catch (java.lang.ClassCastException e) {
        IP6 = null;
        SUCCESS_INDICATOR = false;
    }
").

:- pragma foreign_proc("Java",
    to_inet_address(IP6::in) = (IP::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    IP = (java.net.InetAddress) IP6;
").

%---------------------------------------------------------------------------%

get_canonical_host_name(IP, Name, !IO) :-
    inet_address.get_canonical_host_name(IP, Name, !IO).

get_host_address(IP, Addr, !IO) :-
    inet_address.get_host_address(IP, Addr, !IO).

get_host_name(IP, Name, !IO) :-
    inet_address.get_host_name(IP, Name, !IO).

is_any_local_address(IP) :-
    inet_address.is_any_local_address(IP).

is_link_local_address(IP) :-
    inet_address.is_link_local_address(IP).

is_loopback_address(IP) :-
    inet_address.is_loopback_address(IP).

is_mc_global(IP) :-
    inet_address.is_mc_global(IP).

is_mc_link_local(IP) :-
    inet_address.is_mc_link_local(IP).

is_mc_node_local(IP) :-
    inet_address.is_mc_node_local(IP).

is_mc_org_local(IP) :-
    inet_address.is_mc_org_local(IP).

is_mc_site_local(IP) :-
    inet_address.is_mc_site_local(IP).

is_multicast_address(IP) :-
    inet_address.is_multicast_address(IP).

to_string(IP) =
    inet_address.to_string(IP).

%---------------------------------------------------------------------------%
:- end_module inet6_address.
%---------------------------------------------------------------------------%
