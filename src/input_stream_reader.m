%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% Mercury interface to java.io.InputStreamReader.
%
%---------------------------------------------------------------------------%

:- module jio.input_stream_reader.
:- interface.

:- import_module io.

:- import_module jio.input_stream.
:- import_module jio.reader.
:- import_module jnio.
:- import_module jnio.jcharset.
:- import_module jnio.jcharset.charset.

%---------------------------------------------------------------------------%

:- typeclass input_stream_reader(R) <= reader(R) where [].

:- type input_stream_reader.
:- instance input_stream_reader(input_stream_reader).
:- instance reader(input_stream_reader).

%---------------------------------------------------------------------------%

:- pred new_input_stream_reader(IS::in, input_stream_reader::out,
    io::di, io::uo) is det <= input_stream(IS).

:- pred new_input_stream_reader(IS::in, CS::in, input_stream_reader::out,
    io::di, io::uo) is det <= (input_stream(IS), charset(CS)).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- pragma foreign_type("Java", input_stream_reader,
    "java.io.InputStreamReader").

:- instance input_stream_reader(input_stream_reader) where [].
:- instance reader(input_stream_reader) where [].

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    new_input_stream_reader(IS::in, ISR::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ISR = new java.io.InputStreamReader((java.io.InputStream) IS);
").

%---------------------------------------------------------------------------%

:- pragma foreign_proc("Java",
    new_input_stream_reader(IS::in, CS::in, ISR::out, _IO0::di, _IO::uo),
    [will_not_call_mercury, promise_pure, thread_safe],
"
    ISR = new java.io.InputStreamReader((java.io.InputStream) IS,
        (java.nio.charset.Charset) CS);
").

%---------------------------------------------------------------------------%
:- end_module input_stream_reader.
%---------------------------------------------------------------------------%
