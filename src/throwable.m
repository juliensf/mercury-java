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

:- import_module jlang.stack_trace_element.

:- import_module array.
:- import_module maybe.
:- import_module stream.

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

:- func get_stack_trace(throwable) = array(stack_trace_element).

    % NOTE: this is implemented in Mercury so we can support writing exceptions
    % to Mercury streams.
    %
    % XXX does not yet handle suppressed exceptions or causes.
    %
:- pred print_stack_trace(Stream::in, throwable::in, State::di, State::uo)
    is det <= stream.writer(Stream, string, State).

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

:- pragma foreign_proc("Java",
    get_stack_trace(T::in) = (ST::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ST = T.getStackTrace();
").

%---------------------------------------------------------------------------%

print_stack_trace(Stream, Throwable, !State) :-
    FirstLine = to_string(Throwable),
    stream.put(Stream, FirstLine, !State),
    stream.put(Stream, "\n", !State),
    StackTrace = get_stack_trace(Throwable),
    array.foldl(print_stack_trace_element(Stream), StackTrace, !State).

:- pred print_stack_trace_element(Stream::in, stack_trace_element::in,
    State::di, State::uo) is det <= stream.writer(Stream, string, State).

print_stack_trace_element(Stream, StackTraceElement, !State) :-
    stream.put(Stream, "\tat ", !State),
    Desc = to_string(StackTraceElement),
    stream.put(Stream, Desc, !State),
    stream.put(Stream, "\n", !State).

%---------------------------------------------------------------------------%
:- end_module throwable.
%---------------------------------------------------------------------------%
