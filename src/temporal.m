%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.time.temporal.Temporal interface.
%
%---------------------------------------------------------------------------%

:- module jtime.jtemporal.temporal.
:- interface.

:- import_module jtime.jtemporal.temporal_accessor.

%---------------------------------------------------------------------------%

:- typeclass temporal(T) <= temporal_accessor(T) where [].

%---------------------------------------------------------------------------%
:- end_module jtime.jtemporal.temporal.
%---------------------------------------------------------------------------%
