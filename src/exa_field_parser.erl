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
-module(exa_field_parser).

-export([parse_field_type/2, parse_timestamp/2, parse_timestamp/3]).
-export([parse_foreign_key/5]).

parse_field_type(FieldString, PrimitiveType) ->
    case PrimitiveType of
	identity -> parse_identity(FieldString);
	atom     -> parse_atom(FieldString);
	integer  -> parse_integer(FieldString);
	string   -> parse_string(FieldString)
    end.

parse_foreign_key(FieldString, PrimitiveType, EventSource, FieldKey, FieldList) ->
    case parse_field_type(FieldString, PrimitiveType) of
	{parse_error, Error}       -> {parse_error, Error};
	{partial_integer, Integer} -> {partial_integer, Integer};
	FieldValue                 ->
	    {foreign_reference, {EventSource, FieldValue,  FieldKey, FieldList}}
    end.

parse_identity(String) -> 
    {string, String}.
parse_atom(String) ->
    {atom, list_to_atom(String)}.
parse_integer(String) ->
    case string:to_integer(String) of
	{Integer, []}       -> {integer, Integer};
	{error, no_integer} -> {parse_error, not_an_integer};
	{Integer, _}        -> {partial_integer, Integer}
    end.
parse_string(String) ->
    {string, String}.

parse_timestamp(FieldString, rfc3339) ->
    exa_rfc3339_ts:parse(FieldString);
parse_timestamp(_FieldString, _TimestampType) ->
    {parse_error, timestamp_format_not_supported}.

parse_timestamp(FieldString, partial, FieldList) ->
    exa_partial_ts:parse(FieldString, FieldList);
parse_timestamp(_FieldString, _TimestampType, _FieldList) ->
    {parse_error, timestamp_format_not_supported}.

