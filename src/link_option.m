%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
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

%---------------------------------------------------------------------------%

:- type link_option
    --->    nofollow_links.

:- instance copy_option(link_option).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- instance copy_option(link_option) where [
    func(to_copy_option/1) is lo_to_co
].

:- func lo_to_co(link_option) = copy_option.

lo_to_co(nofollow_links) = jnofollow_links.

:- func jnofollow_links = copy_option.
:- pragma foreign_proc("Java",
    jnofollow_links = (CO::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    CO = java.nio.file.LinkOption.NOFOLLOW_LINKS;
").

%---------------------------------------------------------------------------%
:- end_module jnio.jfile.link_option.
%---------------------------------------------------------------------------%
