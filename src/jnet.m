%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% A Mercury wrapper for the java.net package.
%
%-----------------------------------------------------------------------------%

:- module jnet.
:- interface.

:- include_module http_url_connection.
:- include_module inet_address.
:- include_module inet4_address.
:- include_module inet6_address.
:- include_module inet_socket_address.
:- include_module proxy.
:- include_module socket.
:- include_module socket_address.
:- include_module url.
:- include_module url_connection.
:- include_module uri.

%-----------------------------------------------------------------------------%
:- end_module jnet.
%-----------------------------------------------------------------------------%
