%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2019, Julien Fischer.
% See the file COPYING for license details.
%
% Author: Julien Fischer <juliensf@gmail.com>
%
% A Mercury wrapper for the java.io package.
%
%-----------------------------------------------------------------------------%

:- module jio.
:- interface.

:- include_module buffered_input_stream.
:- include_module buffered_reader.
:- include_module buffered_writer.
:- include_module console.
:- include_module file.
:- include_module file_descriptor.
:- include_module file_input_stream.
:- include_module filter_input_stream.
:- include_module input_stream.
:- include_module input_stream_reader.
:- include_module line_number_reader.
:- include_module output_stream.
:- include_module print_writer.
:- include_module reader.
:- include_module writer.

%-----------------------------------------------------------------------------%
:- end_module jio.
%-----------------------------------------------------------------------------%
