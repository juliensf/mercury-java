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

:- typeclass inet_address(T) where [].

:- pred get_canonical_host_name(T::in, string::out, io::di, io::uo) is det
    <= inet_address(T).
:- pred get_host_address(T::in, string::out, io::di, io::uo) is det
    <= inet_address(T).
:- pred get_host_name(T::in, string::out, io::di, io::uo) is det
    <= inet_address(T).
:- pred is_any_local_address(T::in) is semidet <= inet_address(T).
:- pred is_link_local_address(T::in) is semidet <= inet_address(T).
:- pred is_loopback_address(T::in) is semidet <= inet_address(T).
:- pred is_mc_global(T::in) is semidet <= inet_address(T).
:- pred is_mc_link_local(T::in) is semidet <= inet_address(T).
:- pred is_mc_node_local(T::in) is semidet <= inet_address(T).
:- pred is_mc_org_local(T::in) is semidet <= inet_address(T).
:- pred is_mc_site_local(T::in) is semidet <= inet_address(T).
:- pred is_multicast_address(T::in) is semidet <= inet_address(T).
:- func to_string(T) = string <= inet_address(T).

%---------------------------------------------------------------------------%

:- type inet_address.

:- instance inet_address(inet_address).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", inet_address, "java.net.InetAddress").

:- instance inet_address(inet_address) where [].

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_canonical_host_name(IP::in, Name::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = ((java.net.InetAddress) IP).getCanonicalHostName();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_host_address(IP::in, Addr::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Addr = ((java.net.InetAddress) IP).getHostAddress();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_host_name(IP::in, Name::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Name = ((java.net.InetAddress) IP).getHostAddress();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_any_local_address(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((java.net.InetAddress) IP).isAnyLocalAddress();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_link_local_address(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((java.net.InetAddress) IP).isLinkLocalAddress();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_loopback_address(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((java.net.InetAddress) IP).isLoopbackAddress();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_mc_global(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((java.net.InetAddress) IP).isMCGlobal();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_mc_link_local(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((java.net.InetAddress) IP).isMCLinkLocal();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_mc_node_local(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((java.net.InetAddress) IP).isMCNodeLocal();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_mc_org_local(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((java.net.InetAddress) IP).isMCOrgLocal();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_mc_site_local(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((java.net.InetAddress) IP).isMCSiteLocal();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_multicast_address(IP::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = ((java.net.InetAddress) IP).isMCSiteLocal();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(IP::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = ((java.net.InetAddress) IP).toString();
").

%---------------------------------------------------------------------------%
:- end_module inet_address.
%---------------------------------------------------------------------------%
