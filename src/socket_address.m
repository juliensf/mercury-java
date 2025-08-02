%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.net.SocketAddress.
%
%---------------------------------------------------------------------------%

:- module jnet.socket_address.
:- interface.

:- typeclass socket_address(T) where [].

:- type socket_address.
:- instance socket_address(socket_address).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", socket_address, "java.net.SocketAddress").

:- instance socket_address(socket_address) where [].

%---------------------------------------------------------------------------%
:- end_module socket_address.
%---------------------------------------------------------------------------%
