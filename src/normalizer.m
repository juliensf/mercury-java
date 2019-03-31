%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.text.Normalizer.
%
%---------------------------------------------------------------------------%

:- module jtext.normalizer.
:- interface.

:- type form
    --->    nfc
    ;       nfd
    ;       nfkc
    ;       nfkd.

:- pred is_normalized(string::in, form::in) is semidet.

:- func normalize(string, form) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

is_normalized(S, Form) :-
    JForm = form_to_jform(Form),
    do_is_normalized(S, JForm).

:- pred do_is_normalized(string::in, jform::in) is semidet.
:- pragma foreign_proc("Java",
    do_is_normalized(S::in, JF::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.text.Normalizer.isNormalized(S, JF);
").

%---------------------------------------------------------------------------%

normalize(S, Form) = SPrime :-
    JForm = form_to_jform(Form),
    SPrime = do_normalize(S, JForm).

:- func do_normalize(string, jform) = string.
:- pragma foreign_proc("Java",
    do_normalize(S::in, JF::in) = (SPrime::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SPrime = java.text.Normalizer.normalize(S, JF);
").

%---------------------------------------------------------------------------%

% NOTE: we do not yet have a verison of foreign_enum for Java that maps on to
% Java enumerations, so this is handled in a rather roundabout way.

:- type jform.
:- pragma foreign_type("Java", jform, "java.text.Normalizer.Form").

:- func jnfc = jform.
:- func jnfd = jform.
:- func jnfkc = jform.
:- func jnfkd = jform.

:- pragma foreign_proc("Java",
    jnfc = (F::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    F = java.text.Normalizer.Form.NFC;
").

:- pragma foreign_proc("Java",
    jnfd = (F::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    F = java.text.Normalizer.Form.NFD;
").

:- pragma foreign_proc("Java",
    jnfkc = (F::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    F = java.text.Normalizer.Form.NFKC;
").

:- pragma foreign_proc("Java",
    jnfkd = (F::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    F = java.text.Normalizer.Form.NFKD;
").

:- func form_to_jform(form) = jform.

form_to_jform(nfc) = jnfc.
form_to_jform(nfd) = jnfd.
form_to_jform(nfkc) = jnfkc.
form_to_jform(nfkd) = jnfkd.

%---------------------------------------------------------------------------%
:- end_module normalizer.
%---------------------------------------------------------------------------%
