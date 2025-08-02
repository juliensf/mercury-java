%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.nio.file.LinkOption enum.
%
%---------------------------------------------------------------------------%

:- module jnio.jfile.link_option.
:- interface.

:- import_module jnio.jfile.copy_option.
:- import_module jnio.jfile.open_option.

%---------------------------------------------------------------------------%

:- type link_option
    --->    nofollow_links.

:- instance copy_option(link_option).
:- instance open_option(link_option).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- instance copy_option(link_option) where [
    func(to_jcopy_option/1) is lo_to_co
].

:- func lo_to_co(link_option) = jcopy_option.

lo_to_co(nofollow_links) = jnofollow_links_co.

:- func jnofollow_links_co = jcopy_option.
:- pragma foreign_proc("Java",
    jnofollow_links_co = (CO::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    CO = java.nio.file.LinkOption.NOFOLLOW_LINKS;
").

%---------------------------------------------------------------------------%

:- instance open_option(link_option) where [
    func(to_jopen_option/1) is lo_to_oo
].

:- func lo_to_oo(link_option) = jopen_option.

lo_to_oo(nofollow_links) = jnofollow_links_oo.

:- func jnofollow_links_oo = jopen_option.
:- pragma foreign_proc("Java",
    jnofollow_links_oo = (OO::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    OO = java.nio.file.LinkOption.NOFOLLOW_LINKS;
").

%---------------------------------------------------------------------------%
:- end_module jnio.jfile.link_option.
%---------------------------------------------------------------------------%
