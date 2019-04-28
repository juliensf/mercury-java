%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% A Mercury wrapper for the java.nio.charset.StandardCharsets class.
%
%-----------------------------------------------------------------------------%

:- module jnio.jcharset.standard_charsets.
:- interface.

:- import_module jnio.jcharset.charset.

%-----------------------------------------------------------------------------%

:- func iso_8859_1 = charset.

:- func us_ascii = charset.

:- func utf_16 = charset.

:- func utf_16be = charset.

:- func utf_16le = charset.

:- func utf_8 = charset.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

%-----------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    iso_8859_1 = (CS::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    CS = java.nio.charset.StandardCharsets.ISO_8859_1;
").

:- pragma foreign_proc("Java",
    us_ascii = (CS::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    CS = java.nio.charset.StandardCharsets.US_ASCII;
").

:- pragma foreign_proc("Java",
    utf_16 = (CS::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    CS = java.nio.charset.StandardCharsets.UTF_16;
").

:- pragma foreign_proc("Java",
    utf_16be = (CS::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    CS = java.nio.charset.StandardCharsets.UTF_16BE;
").

:- pragma foreign_proc("Java",
    utf_16le = (CS::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    CS = java.nio.charset.StandardCharsets.UTF_16LE;
").

:- pragma foreign_proc("Java",
    utf_8 = (CS::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    CS = java.nio.charset.StandardCharsets.UTF_8;
").

%-----------------------------------------------------------------------------%
:- end_module jnio.jcharset.standard_charsets.
%-----------------------------------------------------------------------------%
