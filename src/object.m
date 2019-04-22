%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.lang.Object.
%
%---------------------------------------------------------------------------%

:- module jlang.object.
:- interface.

:- import_module io.

:- type object.

:- func to_string(object::in, io::ui) = (string::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", object, "java.lang.Object").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(O::in, _IO::ui) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = O.toString();
").

%---------------------------------------------------------------------------%
:- end_module object.
%---------------------------------------------------------------------------%
