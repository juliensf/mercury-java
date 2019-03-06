%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.io.File.
%
%---------------------------------------------------------------------------%

:- module file.
:- interface.

:- import_module maybe.

%---------------------------------------------------------------------------%

:- type file.

:- func file(string) = file.

:- func get_name(file) = string.

:- func get_parent(file) = maybe(string).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", file, "java.io.File").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    file(S::in) = (F::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    F = new java.io.File(S);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_name(F::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = F.getName();
").

%---------------------------------------------------------------------------%

get_parent(File) = Result :-
    do_get_raw_query(File, Ok, Parent),
    (
        Ok = no,
        Result = no
    ;
        Ok = yes,
        Result = yes(Parent)
    ).

:- pred do_get_raw_query(file::in, bool::out, string::out) is det.

:- pragma foreign_proc("Java",
    do_get_raw_query(F::in, Ok::out, P::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    P = F.getParent();
    Ok = (P == null) ? bool.NO : bool.YES;
").

%---------------------------------------------------------------------------%
:- end_module file.
%---------------------------------------------------------------------------%
