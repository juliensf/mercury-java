%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.net.InetAddress.
%
%---------------------------------------------------------------------------%

:- module jnet.inet_address.
:- interface.

:- import_module io.

%---------------------------------------------------------------------------%

:- typeclass inet_address(T) where [
    pred get_canonical_host_name(T::in, string::out, io::di, io::uo) is det,
    pred get_host_address(T::in, string::out, io::di, io::uo) is det,
    pred get_host_name(T::in, string::out, io::di, io::uo) is det,
    pred is_any_local_address(T::in) is semidet,
    pred is_link_local_address(T::in) is semidet,
    pred is_loopback_address(T::in) is semidet,
    pred is_mc_global(T::in) is semidet,
    pred is_mc_link_local(T::in) is semidet,
    pred is_mc_node_local(T::in) is semidet,
    pred is_mc_org_local(T::in) is semidet,
    pred is_mc_site_local(T::in) is semidet,
    pred is_multicast_address(T::in) is semidet,
    func to_string(T) = string
].

%---------------------------------------------------------------------------%

:- type inet_address.

:- instance inet_address(inet_address).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", inet_address, "java.net.InetAddress").

%---------------------------------------------------------------------------%

:- instance inet_address(inet_address) where [
    pred(get_canonical_host_name/4) is inet_address_get_canonical_host_name,
    pred(get_host_address/4) is inet_address_get_host_address,
    pred(get_host_name/4) is inet_address_get_host_name,
    pred(is_any_local_address/1) is inet_address_is_any_local_address,
    pred(is_link_local_address/1) is inet_address_is_link_local_address,
    pred(is_loopback_address/1) is inet_address_is_loopback_address,
    pred(is_mc_global/1) is inet_address_is_mc_global,
    pred(is_mc_link_local/1) is inet_address_is_mc_link_local,
    pred(is_mc_node_local/1) is inet_address_is_mc_node_local,
    pred(is_mc_org_local/1) is inet_address_is_mc_org_local,
    pred(is_mc_site_local/1) is inet_address_is_mc_site_local,
    pred(is_multicast_address/1) is inet_address_is_multicast_address,
    func(to_string/1) is inet_address_to_string
].

%---------------------------------------------------------------------------%

:- pred inet_address_get_canonical_host_name(inet_address::in, string::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    inet_address_get_canonical_host_name(IP::in, Name::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = IP.getCanonicalHostName();
").

%---------------------------------------------------------------------------%

:- pred inet_address_get_host_address(inet_address::in, string::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    inet_address_get_host_address(IP::in, Addr::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Addr = IP.getHostAddress();
").

%---------------------------------------------------------------------------%

:- pred inet_address_get_host_name(inet_address::in, string::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    inet_address_get_host_name(IP::in, Name::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = IP.getHostAddress();
").

%---------------------------------------------------------------------------%

:- pred inet_address_is_any_local_address(inet_address::in) is semidet.
:- pragma foreign_proc("Java",
    inet_address_is_any_local_address(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = IP.isAnyLocalAddress();
").

%---------------------------------------------------------------------------%

:- pred inet_address_is_link_local_address(inet_address::in) is semidet.
:- pragma foreign_proc("Java",
    inet_address_is_link_local_address(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = IP.isLinkLocalAddress();
").

%---------------------------------------------------------------------------%

:- pred inet_address_is_loopback_address(inet_address::in) is semidet.
:- pragma foreign_proc("Java",
    inet_address_is_loopback_address(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = IP.isLoopbackAddress();
").

%---------------------------------------------------------------------------%

:- pred inet_address_is_mc_global(inet_address::in) is semidet.
:- pragma foreign_proc("Java",
    inet_address_is_mc_global(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = IP.isMCGlobal();
").

%---------------------------------------------------------------------------%

:- pred inet_address_is_mc_link_local(inet_address::in) is semidet.
:- pragma foreign_proc("Java",
    inet_address_is_mc_link_local(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = IP.isMCLinkLocal();
").

%---------------------------------------------------------------------------%

:- pred inet_address_is_mc_node_local(inet_address::in) is semidet.
:- pragma foreign_proc("Java",
    inet_address_is_mc_node_local(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = IP.isMCNodeLocal();
").

%---------------------------------------------------------------------------%

:- pred inet_address_is_mc_org_local(inet_address::in) is semidet.
:- pragma foreign_proc("Java",
    inet_address_is_mc_org_local(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = IP.isMCOrgLocal();
").

%---------------------------------------------------------------------------%

:- pred inet_address_is_mc_site_local(inet_address::in) is semidet.
:- pragma foreign_proc("Java",
    inet_address_is_mc_site_local(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = IP.isMCSiteLocal();
").

%---------------------------------------------------------------------------%

:- pred inet_address_is_multicast_address(inet_address::in) is semidet.
:- pragma foreign_proc("Java",
    inet_address_is_multicast_address(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = IP.isMCSiteLocal();
").

%---------------------------------------------------------------------------%

:- func inet_address_to_string(inet_address) = string.
:- pragma foreign_proc("Java",
    inet_address_to_string(IP::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = IP.toString();
").

%---------------------------------------------------------------------------%
:- end_module inet_address.
%---------------------------------------------------------------------------%
