%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.nio.file.StandardOpenOption enum.
%
%---------------------------------------------------------------------------%

:- module jnio.jfile.standard_open_option.
:- interface.

:- import_module jnio.jfile.open_option.

%---------------------------------------------------------------------------%

:- type standard_open_option
    --->    append
    ;       create
    ;       create_new
    ;       delete_on_close
    ;       dsync
    ;       read
    ;       sparse
    ;       sync
    ;       truncate_existing
    ;       write.

:- instance open_option(standard_open_option).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- instance open_option(standard_open_option) where [
    func(to_jopen_option/1) is soo_to_oo
].

:- func soo_to_oo(standard_open_option) = jopen_option.

soo_to_oo(append) = jappend.
soo_to_oo(create) = jcreate.
soo_to_oo(create_new) = jcreate_new.
soo_to_oo(delete_on_close) = jdelete_on_close.
soo_to_oo(dsync) = jdsync.
soo_to_oo(read) = jread.
soo_to_oo(sparse) = jsparse.
soo_to_oo(sync) = jsync.
soo_to_oo(truncate_existing) = jtruncate_existing.
soo_to_oo(write) = jwrite.

:- func jappend = jopen_option.
:- pragma foreign_proc("Java",
    jappend = (OO::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    OO = java.nio.file.StandardOpenOption.APPEND;
").

:- func jcreate = jopen_option.
:- pragma foreign_proc("Java",
    jcreate = (OO::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    OO = java.nio.file.StandardOpenOption.CREATE;
").

:- func jcreate_new = jopen_option.
:- pragma foreign_proc("Java",
    jcreate_new = (OO::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    OO = java.nio.file.StandardOpenOption.CREATE_NEW;
").

:- func jdelete_on_close = jopen_option.
:- pragma foreign_proc("Java",
    jdelete_on_close = (OO::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    OO = java.nio.file.StandardOpenOption.DELETE_ON_CLOSE;
").

:- func jdsync = jopen_option.
:- pragma foreign_proc("Java",
    jdsync = (OO::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    OO = java.nio.file.StandardOpenOption.DSYNC;
").

:- func jread = jopen_option.
:- pragma foreign_proc("Java",
    jread = (OO::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    OO = java.nio.file.StandardOpenOption.READ;
").

:- func jsparse = jopen_option.
:- pragma foreign_proc("Java",
    jsparse = (OO::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    OO = java.nio.file.StandardOpenOption.SPARSE;
").

:- func jsync = jopen_option.
:- pragma foreign_proc("Java",
    jsync = (OO::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    OO = java.nio.file.StandardOpenOption.SYNC;
").

:- func jtruncate_existing = jopen_option.
:- pragma foreign_proc("Java",
    jtruncate_existing = (OO::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    OO = java.nio.file.StandardOpenOption.TRUNCATE_EXISTING;
").

:- func jwrite = jopen_option.
:- pragma foreign_proc("Java",
    jwrite = (OO::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    OO = java.nio.file.StandardOpenOption.WRITE;
").

%---------------------------------------------------------------------------%
:- end_module jnio.jfile.standard_open_option.
%---------------------------------------------------------------------------%
