%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
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

:- type inet6_address.

:- instance inet_address(inet6_address).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module io.

:- pragma foreign_type("Java", inet6_address, "java.net.Inet6Address").

%---------------------------------------------------------------------------%

:- instance inet_address(inet6_address) where [
    pred(get_canonical_host_name/4) is inet6_address.get_canonical_host_name,
    pred(get_host_address/4) is inet6_address.get_host_address,
    pred(get_host_name/4) is inet6_address.get_host_name,
    pred(is_any_local_address/1) is inet6_address.is_any_local_address,
    pred(is_link_local_address/1) is inet6_address.is_link_local_address,
    pred(is_loopback_address/1) is inet6_address.is_loopback_address,
    pred(is_mc_global/1) is inet6_address.is_mc_global,
    pred(is_mc_link_local/1) is inet6_address.is_mc_link_local,
    pred(is_mc_node_local/1) is inet6_address.is_mc_node_local,
    pred(is_mc_org_local/1) is inet6_address.is_mc_org_local,
    pred(is_mc_site_local/1) is inet6_address.is_mc_site_local,
    pred(is_multicast_address/1) is inet6_address.is_multicast_address,
    func(to_string/1) is inet6_address.to_string
].

%---------------------------------------------------------------------------%

:- pred get_canonical_host_name(inet6_address::in, string::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    get_canonical_host_name(IP::in, Name::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = IP.getCanonicalHostName();
").

%---------------------------------------------------------------------------%

:- pred get_host_address(inet6_address::in, string::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    get_host_address(IP::in, Addr::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Addr = IP.getHostAddress();
").

%---------------------------------------------------------------------------%

:- pred get_host_name(inet6_address::in, string::out,
    io::di, io::uo) is det.
:- pragma foreign_proc("Java",
    get_host_name(IP::in, Name::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = IP.getHostAddress();
").

%---------------------------------------------------------------------------%

:- pred is_any_local_address(inet6_address::in) is semidet.
:- pragma foreign_proc("Java",
    is_any_local_address(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = IP.isAnyLocalAddress();
").

%---------------------------------------------------------------------------%

:- pred is_link_local_address(inet6_address::in) is semidet.
:- pragma foreign_proc("Java",
    is_link_local_address(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = IP.isLinkLocalAddress();
").

%---------------------------------------------------------------------------%

:- pred is_loopback_address(inet6_address::in) is semidet.
:- pragma foreign_proc("Java",
    is_loopback_address(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = IP.isLoopbackAddress();
").

%---------------------------------------------------------------------------%

:- pred is_mc_global(inet6_address::in) is semidet.
:- pragma foreign_proc("Java",
    is_mc_global(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = IP.isMCGlobal();
").

%---------------------------------------------------------------------------%

:- pred is_mc_link_local(inet6_address::in) is semidet.
:- pragma foreign_proc("Java",
    is_mc_link_local(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = IP.isMCLinkLocal();
").

%---------------------------------------------------------------------------%

:- pred is_mc_node_local(inet6_address::in) is semidet.
:- pragma foreign_proc("Java",
    is_mc_node_local(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = IP.isMCNodeLocal();
").

%---------------------------------------------------------------------------%

:- pred is_mc_org_local(inet6_address::in) is semidet.
:- pragma foreign_proc("Java",
    is_mc_org_local(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = IP.isMCOrgLocal();
").

%---------------------------------------------------------------------------%

:- pred is_mc_site_local(inet6_address::in) is semidet.
:- pragma foreign_proc("Java",
    is_mc_site_local(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = IP.isMCSiteLocal();
").

%---------------------------------------------------------------------------%

:- pred is_multicast_address(inet6_address::in) is semidet.
:- pragma foreign_proc("Java",
    is_multicast_address(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = IP.isMCSiteLocal();
").


%---------------------------------------------------------------------------%

:- func to_string(inet6_address) = string.
:- pragma foreign_proc("Java",
    to_string(IP::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = IP.toString();
").

%---------------------------------------------------------------------------%
:- end_module inet6_address.
%---------------------------------------------------------------------------%
