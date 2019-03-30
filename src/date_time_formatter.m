%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% A Mercury wrapper for the java.time.format.DateTimeFormatter class.
%
%-----------------------------------------------------------------------------%

:- module jtime.format.date_time_formatter.
:- interface.

:- type date_time_formatter.

:- func basic_iso_date = date_time_formatter.
:- func iso_date = date_time_formatter.
:- func iso_date_time = date_time_formatter.
:- func iso_instant = date_time_formatter.
:- func iso_local_date = date_time_formatter.
:- func iso_local_date_time = date_time_formatter.
:- func iso_local_time = date_time_formatter.
:- func iso_offset_date = date_time_formatter.
:- func iso_offset_date_time = date_time_formatter.
:- func iso_offset_time = date_time_formatter.
:- func iso_ordinal_date = date_time_formatter.
:- func iso_time = date_time_formatter.
:- func iso_week_date = date_time_formatter.
:- func iso_zoned_date_time = date_time_formatter.
:- func rfc_1123_date_time = date_time_formatter.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", date_time_formatter,
    "java.time.format.DateTimeFormatter").

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    basic_iso_date = (DTF::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DTF = java.time.format.DateTimeFormatter.BASIC_ISO_DATE;
").

:- pragma foreign_proc("Java",
    iso_date = (DTF::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DTF = java.time.format.DateTimeFormatter.ISO_DATE;
").

:- pragma foreign_proc("Java",
    iso_date_time = (DTF::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DTF = java.time.format.DateTimeFormatter.ISO_DATE_TIME;
").

:- pragma foreign_proc("Java",
    iso_instant = (DTF::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DTF = java.time.format.DateTimeFormatter.ISO_INSTANT;
").

:- pragma foreign_proc("Java",
    iso_local_date = (DTF::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DTF = java.time.format.DateTimeFormatter.ISO_LOCAL_DATE;
").

:- pragma foreign_proc("Java",
    iso_local_date_time = (DTF::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DTF = java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME;
").

:- pragma foreign_proc("Java",
    iso_local_time = (DTF::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DTF = java.time.format.DateTimeFormatter.ISO_LOCAL_TIME;
").

:- pragma foreign_proc("Java",
    iso_offset_date = (DTF::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DTF = java.time.format.DateTimeFormatter.ISO_OFFSET_DATE;
").

:- pragma foreign_proc("Java",
    iso_offset_date_time = (DTF::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DTF = java.time.format.DateTimeFormatter.ISO_OFFSET_DATE_TIME;
").

:- pragma foreign_proc("Java",
    iso_offset_time = (DTF::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DTF = java.time.format.DateTimeFormatter.ISO_OFFSET_TIME;
").

:- pragma foreign_proc("Java",
    iso_ordinal_date = (DTF::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DTF = java.time.format.DateTimeFormatter.ISO_ORDINAL_DATE;
").

:- pragma foreign_proc("Java",
    iso_time = (DTF::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DTF = java.time.format.DateTimeFormatter.ISO_TIME;
").

:- pragma foreign_proc("Java",
    iso_week_date = (DTF::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DTF = java.time.format.DateTimeFormatter.ISO_WEEK_DATE;
").

:- pragma foreign_proc("Java",
    iso_zoned_date_time = (DTF::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DTF = java.time.format.DateTimeFormatter.ISO_ZONED_DATE_TIME;
").

:- pragma foreign_proc("Java",
    rfc_1123_date_time = (DTF::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    DTF = java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME;
").

%-----------------------------------------------------------------------------%
:- end_module date_time_formatter.
%-----------------------------------------------------------------------------%
