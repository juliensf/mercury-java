%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.net.InetSocketAddress.
%
%---------------------------------------------------------------------------%

:- module jnet.inet_socket_address.
:- interface.

:- import_module jnet.socket_address.

%---------------------------------------------------------------------------%

:- type inet_socket_address.
:- instance socket_address(inet_socket_address).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", inet_socket_address,
    "java.net.InetSocketAddress").

:- instance socket_address(inet_socket_address) where [].

%---------------------------------------------------------------------------%
:- end_module inet_socket_address.
%---------------------------------------------------------------------------%
