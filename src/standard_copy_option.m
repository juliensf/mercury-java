%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.nio.file.StandardCopyOption enum.
%
%---------------------------------------------------------------------------%

:- module jnio.jfile.standard_copy_option.
:- interface.

:- import_module jnio.jfile.copy_option.

%---------------------------------------------------------------------------%

:- type standard_copy_option
    --->    atomic_move
    ;       copy_attributes
    ;       replace_existing.

:- instance copy_option(standard_copy_option).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- instance copy_option(standard_copy_option) where [
    func(to_jcopy_option/1) is sco_to_co
].

:- func sco_to_co(standard_copy_option) = jcopy_option.

sco_to_co(atomic_move) = jatomic_move.
sco_to_co(copy_attributes) = jcopy_attributes.
sco_to_co(replace_existing) = jreplace_existing.

:- func jatomic_move = jcopy_option.
:- pragma foreign_proc("Java",
    jatomic_move = (CO::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    CO = java.nio.file.StandardCopyOption.ATOMIC_MOVE;
").

:- func jcopy_attributes = jcopy_option.
:- pragma foreign_proc("Java",
    jcopy_attributes = (CO::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    CO = java.nio.file.StandardCopyOption.COPY_ATTRIBUTES;
").

:- func jreplace_existing = jcopy_option.
:- pragma foreign_proc("Java",
    jreplace_existing = (CO::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    CO = java.nio.file.StandardCopyOption.REPLACE_EXISTING;
").

%---------------------------------------------------------------------------%
:- end_module jnio.jfile.standard_copy_option.
%---------------------------------------------------------------------------%
