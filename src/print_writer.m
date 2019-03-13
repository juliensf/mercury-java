%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.io.PrintWriter using the stream type classes.
%
%---------------------------------------------------------------------------%

:- module jio.print_writer.
:- interface.

:- import_module jio.file.

:- import_module bool.
:- import_module char.
:- import_module io.
:- import_module maybe.
:- import_module stream.

%---------------------------------------------------------------------------%

:- type print_writer.

:- instance stream(print_writer, io).
:- instance output(print_writer, io).
:- instance writer(print_writer, char, io).
:- instance writer(print_writer, float, io).
:- instance writer(print_writer, int, io).
:- instance writer(print_writer, int64, io).
:- instance writer(print_writer, string, io).

:- pred print_writer(file::in, maybe_error(print_writer)::out,
    io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- pred check_error(print_writer::in, bool::out, io::di, io::uo) is det.

:- pred close(print_writer::in, io::di, io::uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", print_writer, "java.io.PrintWriter").

%---------------------------------------------------------------------------%

:- instance stream(print_writer, io) where [
    (name(_, "<<java.io.PrintWriter>>", !IO))
].

:- instance output(print_writer, io) where [
    pred(flush/3) is do_flush
].

:- instance writer(print_writer, char, io) where [
    pred(put/4) is do_print_char
].

:- instance writer(print_writer, float, io) where [
    pred(put/4) is do_print_float
].

:- instance writer(print_writer, int, io) where [
    pred(put/4) is do_print_int
].

:- instance writer(print_writer, int64, io) where [
    pred(put/4) is do_print_int64
].

:- instance writer(print_writer, string, io) where [
    pred(put/4) is do_print_string
].

%---------------------------------------------------------------------------%

print_writer(File, Result, !IO) :-
    print_writer_from_file(File, PW, Ok, ErrMsg, !IO),
    (
        Ok = no,
        Result = error(ErrMsg)
    ;
        Ok = yes,
        Result = ok(PW)
    ).

:- pred print_writer_from_file(file::in, print_writer::out, bool::out,
    string::out, io::di, io::uo) is det.

:- pragma foreign_proc("Java",
    print_writer_from_file(F::in, PW::out, Ok::out, ErrMsg::out,
        _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    PW = null;
    try {
        PW = new java.io.PrintWriter(F);
        Ok = bool.YES;
        ErrMsg = """";
    } catch (java.io.FileNotFoundException | java.lang.SecurityException e) {
        Ok = bool.NO;
        ErrMsg = e.getMessage();
    }
").

%---------------------------------------------------------------------------%

:- pred do_flush(print_writer::in, io::di, io::uo) is det.

:- pragma foreign_proc("Java",
    do_flush(PW::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    PW.flush();
").

%---------------------------------------------------------------------------%

:- pred do_print_char(print_writer::in, char::in, io::di, io::uo) is det.

:- pragma foreign_proc("Java",
    do_print_char(PW::in, C::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    PW.print(java.lang.Character.toChars(C));
").

%---------------------------------------------------------------------------%

:- pred do_print_float(print_writer::in, float::in, io::di, io::uo) is det.

:- pragma foreign_proc("Java",
    do_print_float(PW::in, F::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    PW.print(F);
").

%---------------------------------------------------------------------------%

:- pred do_print_int(print_writer::in, int::in, io::di, io::uo) is det.

:- pragma foreign_proc("Java",
    do_print_int(PW::in, I::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    PW.print(I);
").

%---------------------------------------------------------------------------%

:- pred do_print_int64(print_writer::in, int64::in, io::di, io::uo) is det.

:- pragma foreign_proc("Java",
    do_print_int64(PW::in, I64::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    PW.print(I64);
").

%---------------------------------------------------------------------------%

:- pred do_print_string(print_writer::in, string::in, io::di, io::uo) is det.

:- pragma foreign_proc("Java",
    do_print_string(PW::in, S::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    PW.print(S);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    check_error(PW::in, Result::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    Result = (PW.checkError()) ? bool.YES : bool.NO;
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    close(PW::in, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    PW.close();
").

%---------------------------------------------------------------------------%
:- end_module print_writer.
%---------------------------------------------------------------------------%
