%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.util.Locale.
%
%---------------------------------------------------------------------------%

:- module jutil.locale.
:- interface.

:- import_module io.

:- type locale.

:- pred get_default(locale::out, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", locale, "java.util.Locale").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_default(L::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    L = java.util.Locale.getDefault();
").

%---------------------------------------------------------------------------%
:- end_module locale.
%---------------------------------------------------------------------------%
