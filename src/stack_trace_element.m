%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.lang.StackTraceElement.
%
%---------------------------------------------------------------------------%

:- module jlang.stack_trace_element.
:- interface.

:- type stack_trace_element.

:- func to_string(stack_trace_element) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", stack_trace_element,
    "java.lang.StackTraceElement").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(E::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = E.toString();
").

%---------------------------------------------------------------------------%
:- end_module stack_trace_element.
%---------------------------------------------------------------------------%
