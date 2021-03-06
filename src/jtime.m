%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% A Mercury wrapper for the java.time package.
%
%-----------------------------------------------------------------------------%

:- module jtime.
:- interface.

:- include_module jtemporal.

:- include_module day_of_week.
:- include_module duration.
:- include_module format.
:- include_module instant.
:- include_module local_date.
:- include_module local_date_time.
:- include_module local_time.
:- include_module month.
:- include_module offset_date_time.
:- include_module offset_time.
:- include_module period.
:- include_module year.
:- include_module year_month.
:- include_module zoned_date_time.
:- include_module zone_offset.

%-----------------------------------------------------------------------------%
:- end_module jtime.
%-----------------------------------------------------------------------------%
