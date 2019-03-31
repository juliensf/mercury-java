%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.util.Locale.
%
%---------------------------------------------------------------------------%

:- module jutil.locale.
:- interface.

:- import_module array.
:- import_module io.

:- type locale.

:- pred get_available_locales(array(locale)::out, io::di, io::uo) is det.

:- func get_country(locale) = string.

:- pred get_display_country(locale::in, string::out, io::di, io::uo) is det.
:- pred get_display_language(locale::in, string::out, io::di, io::uo) is det.
:- pred get_display_name(locale::in, string::out, io::di, io::uo) is det.
:- pred get_display_script(locale::in, string::out, io::di, io::uo) is det.
:- pred get_display_variant(locale::in, string::out, io::di, io::uo) is det.

:- func get_display_country(locale, locale) = string.
:- func get_display_language(locale, locale) = string.
:- func get_display_name(locale, locale) = string.
:- func get_display_script(locale, locale) = string.
:- func get_display_variant(locale, locale) = string.

:- pred get_default(locale::out, io::di, io::uo) is det.

:- pred get_iso3_country(locale::in, string::out) is semidet.

:- pred get_iso3_language(locale::in, string::out) is semidet.

:- func to_language_tag(locale) = string.

:- func to_string(locale) = string.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", locale, "java.util.Locale").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_available_locales(A::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    A = java.util.Locale.getAvailableLocales();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_country(L::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = L.getCountry();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_display_country(L::in, S::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = L.getDisplayCountry();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_display_language(L::in, S::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = L.getDisplayLanguage();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_display_name(L::in, S::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = L.getDisplayName();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_display_script(L::in, S::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = L.getDisplayScript();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_display_variant(L::in, S::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = L.getDisplayVariant();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_display_country(L::in, DL::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = L.getDisplayCountry(DL);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_display_language(L::in, DL::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = L.getDisplayLanguage(DL);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_display_name(L::in, DL::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = L.getDisplayName(DL);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_display_script(L::in, DL::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = L.getDisplayScript(DL);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_display_variant(L::in, DL::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = L.getDisplayVariant(DL);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_default(L::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    L = java.util.Locale.getDefault();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_iso3_country(L::in, S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        S = L.getISO3Country();
        SUCCESS_INDICATOR = true;
    } catch (java.util.MissingResourceException e) {
        S = null;
        SUCCESS_INDICATOR = false;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    get_iso3_language(L::in, S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    try {
        S = L.getISO3Language();
        SUCCESS_INDICATOR = true;
    } catch (java.util.MissingResourceException e) {
        S = null;
        SUCCESS_INDICATOR = false;
    }
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_language_tag(L::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = L.toLanguageTag();
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_string(L::in) = (S::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    S = L.toString();
").

%---------------------------------------------------------------------------%
:- end_module locale.
%---------------------------------------------------------------------------%
