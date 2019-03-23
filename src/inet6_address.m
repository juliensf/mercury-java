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

:- type inet6_address.

:- func to_string(inet6_address) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", inet6_address, "java.net.Inet6Address").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(IP::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = IP.toString();
").

%---------------------------------------------------------------------------%
:- end_module inet6_address.
%---------------------------------------------------------------------------%
