%%%-------------------------------------------------------------------
%%% @author Edward Tate <edward.tate@erlang-solutions.com>
%%% @copyright (C) 2010, Erlang Solutions Ltd.
%%% @doc
%%%
%%% @end
%%% Created : 15 Oct 2010 by Edward Tate <edward.tate@erlang-solutions.com>
%%%-------------------------------------------------------------------
%%% Copyright (c) 2009,2010 Erlang Solutions formerly Erlang Training & Consulting
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% * Redistributions of source code must retain the above copyright
%%%   notice, this list of conditions and the following disclaimer. 
%%% * Redistributions in binary form must reproduce the above copyright
%%%   notice, this list of conditions and the following disclaimer in the
%%%   documentation and/or other materials provided with the distribution.
%%% * Neither the name of the Erlang Solutions nor the names of its
%%%   contributors may be used to endorse or promote products
%%%   derived from this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
%%% BE  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------

%% This module defines the standard log format used in Exago
-module(exa_field).

-export([annotation/2, instance_key/2, foreign_key/5]).
-export([timestamp/2, timestamp/3, transition/2, state/2]).

%% @doc Closes over a field type by capturing its basic type, such as 'timestamp',
%% its identifier which may be viewed as a column in a larger event source, 
%% and a function responsible for parsing its value into a known type, such as an
%% integer, or a string.
-spec(close_over (FieldType::string(), FieldIdentifier::term(), ParserSpec::tuple())
      -> ParsedField::tuple()).	     
close_over(Type, Identifier, {Module, Function, Arguments}) ->
    fun (FieldString) ->
	    {Type, 
	     {field_identifier, Identifier},
	     {field_value, apply({Module, Function}, [FieldString] ++ Arguments)}}
    end.

%% @doc An annotation parser
annotation(Identifier, PrimitiveType) -> 
    close_over(annotation, Identifier, {exa_field_parser, parse_field_type, [PrimitiveType]}).

%% @doc An instance key parser
instance_key(Identifier, PrimitiveType) ->
    close_over(instance_key, Identifier, {exa_field_parser, parse_field_type, [PrimitiveType]}).

%% @doc A foreign key parser
foreign_key(Identifier, PrimitiveType, EventSource, FieldKey, FieldList) ->
    close_over(foreign_key, Identifier, {exa_field_parser, parse_foreign_key, [PrimitiveType, EventSource, FieldKey, FieldList]}).

%% @doc A timestamp parser
timestamp(Identifier, Format) ->
    close_over(timestamp, Identifier, {exa_field_parser, parse_timestamp, [Format]}).

timestamp(Identifier, Format, FormatString) ->
    close_over(timestamp, Identifier, {exa_field_parser, parse_timestamp, [Format, FormatString]}).

transition(Identifier, PrimitiveType) ->
    close_over(transition, Identifier, {exa_field_parser, parse_field_type, [PrimitiveType]}).

state(Identifier, PrimitiveType) ->
    close_over(state, Identifier, {exa_field_parser, parse_field_type, [PrimitiveType]}).
