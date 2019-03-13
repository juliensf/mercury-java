%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.lang.Throwable.
%
%---------------------------------------------------------------------------%

:- module jlang.throwable.
:- interface.

:- import_module maybe.

%---------------------------------------------------------------------------%

    % A java.lang.Throwable or one of its subclasses.
    %
:- type throwable.

    % A Mercury wrapper for Java exceptions and errors.
    %
:- type java_exception
    --->    java_exception(throwable).

%---------------------------------------------------------------------------%

:- func get_message(throwable) = maybe(string).

:- func to_string(throwable) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.

%---------------------------------------------------------------------------%

:- pragma foreign_type("Java", throwable, "java.lang.Throwable").

%---------------------------------------------------------------------------%

get_message(T) = MaybeMessage :-
    do_get_message(T, Message, Ok),
    (
        Ok = no,
        MaybeMessage = no
    ;
        Ok = yes,
        MaybeMessage = yes(Message)
    ).

:- pred do_get_message(throwable::in, string::out, bool::out) is det.
:- pragma foreign_proc("Java",
    do_get_message(T::in, M::out, Ok::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    M = T.getMessage();
    if (M == null) {
        Ok = bool.NO;
    } else {
        Ok = bool.YES;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(T::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = T.toString();
").

%---------------------------------------------------------------------------%
:- end_module throwable.
%---------------------------------------------------------------------------%
