%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019 Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury wrapper for java.lang.Character.
%
%---------------------------------------------------------------------------%

:- module jlang.character.
:- interface.

:- import_module char.

%---------------------------------------------------------------------------%

:- pred is_alphabetic(char::in) is semidet.

:- pred is_bmp_code_point(char::in) is semidet.

:- pred is_defined(char::in) is semidet.

:- pred is_digit(char::in) is semidet.

:- pred is_identifier_ignorable(char::in) is semidet.

:- pred is_ideographic(char::in) is semidet.

:- pred is_iso_control(char::in) is semidet.

:- pred is_java_identifier_part(char::in) is semidet.

:- pred is_java_identifier_start(char::in) is semidet.

:- pred is_letter(char::in) is semidet.

:- pred is_letter_or_digit(char::in) is semidet.

:- pred is_lower_case(char::in) is semidet.

:- pred is_mirrored(char::in) is semidet.

:- pred is_space_char(char::in) is semidet.

:- pred is_supplementary_code_point(char::in) is semidet.

:- pred is_title_case(char::in) is semidet.

:- pred is_unicode_identifier_part(char::in) is semidet.

:- pred is_unicode_identifier_start(char::in) is semidet.

:- pred is_upper_case(char::in) is semidet.

:- pred is_whitespace(char::in) is semidet.

:- pred to_lower_case(char::in, char::out) is semidet.

:- pred to_title_case(char::in, char::out) is semidet.

:- pred to_upper_case(char::in, char::out) is semidet.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_alphabetic(C::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Character.isAlphabetic(C);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_bmp_code_point(C::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Character.isBmpCodePoint(C);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_defined(C::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Character.isDefined(C);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_digit(C::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Character.isDigit(C);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_identifier_ignorable(C::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Character.isIdentifierIgnorable(C);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_ideographic(C::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Character.isIdeographic(C);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_iso_control(C::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Character.isISOControl(C);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_java_identifier_part(C::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Character.isJavaIdentifierPart(C);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_java_identifier_start(C::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Character.isJavaIdentifierStart(C);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_letter(C::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Character.isLetter(C);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_letter_or_digit(C::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Character.isLetterOrDigit(C);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_lower_case(C::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Character.isLowerCase(C);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_mirrored(C::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Character.isMirrored(C);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_space_char(C::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Character.isSpaceChar(C);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_supplementary_code_point(C::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Character.isSupplementaryCodePoint(C);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_title_case(C::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Character.isTitleCase(C);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_unicode_identifier_part(C::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Character.isUnicodeIdentifierPart(C);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_unicode_identifier_start(C::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Character.isUnicodeIdentifierStart(C);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_upper_case(C::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Character.isUpperCase(C);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    is_whitespace(C::in),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    SUCCESS_INDICATOR = java.lang.Character.isWhitespace(C);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_lower_case(A::in, B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = java.lang.Character.toLowerCase(A);
    SUCCESS_INDICATOR = (A != B);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_title_case(A::in, B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = java.lang.Character.toTitleCase(A);
    SUCCESS_INDICATOR = (A != B);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    to_upper_case(A::in, B::out),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    B = java.lang.Character.toUpperCase(A);
    SUCCESS_INDICATOR = (A != B);
").

%---------------------------------------------------------------------------%
:- end_module character.
%---------------------------------------------------------------------------%
