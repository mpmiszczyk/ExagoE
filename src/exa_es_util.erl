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
-module(exa_es_util).

-export([remove_event_source_by_name/2]).
-export([find_field_by_identifier/2, find_field_by_identifier_and_value/3]).
-export([extract_field_value_by_id/2, extract_field_value_by_type/2]).

%%
remove_event_source_by_name(EventSourceNameReference, EventSources) ->
    remove_event_source_by_name_(EventSourceNameReference, EventSources, []).

remove_event_source_by_name_(_EventSourceNameReference, [], Result) ->
    lists:reverse(Result);
remove_event_source_by_name_(EventSourceNameReference, [{EventSourceName, FieldList}|EventSources], Result) ->
    case EventSourceName =:= EventSourceNameReference of
	true  -> 
	    remove_event_source_by_name_(EventSourceNameReference, EventSources, Result);
	false -> 
	    remove_event_source_by_name_(EventSourceNameReference, EventSources, [{EventSourceName, FieldList}|Result])
    end.

%%
find_field_by_identifier(_Identifier, []) ->
    undefined;
find_field_by_identifier(Identifier, [{FieldName, {field_identifier, FieldIdentifier}, FieldValue}|Events]) ->
    case Identifier =:= FieldIdentifier of
	true  -> {success, {FieldName, {field_identifier, FieldIdentifier}, FieldValue}};
	false -> find_field_by_identifier(Identifier, Events)
    end.

%%
find_field_by_identifier_and_value(_Identifier, _Value, []) ->
    undefined;
find_field_by_identifier_and_value(Identifier, Value, [{FieldName, 
							{field_identifier, FieldIdentifier}, 
							{field_value, FieldValue}}|Events]) ->
    case Identifier =:= FieldIdentifier andalso Value =:= FieldValue of
	true  -> {success, {FieldName, {field_identifier, FieldIdentifier}, FieldValue}};
	false -> find_field_by_identifier_and_value(Identifier, Value, Events)
    end.

%%
extract_field_value_by_type(_FieldType, []) ->
    undefined;
extract_field_value_by_type(FieldType, [Field|Fields]) ->
    case Field of
	{FType, _FieldIdentifier, {field_value, FieldValue}} ->
	    case FieldType =:= FType of
		true  -> FieldValue;
		false -> extract_field_value_by_type(FieldType, Fields)
	    end;
	_ ->
	    extract_field_value_by_type(FieldType, Fields)
    end.

%%
extract_field_value_by_id(_Identifier, []) ->
    undefined;
extract_field_value_by_id(Identifier, [Field|Fields]) ->
    case Field of
	{_FieldType, {field_identifier, FieldIdentifier}, {field_value, FieldValue}} ->
	    case Identifier =:= FieldIdentifier of
		true  -> FieldValue;
		false -> extract_field_value_by_id(Identifier, Fields)
	    end;
	_ ->
	    extract_field_value_by_id(Identifier, Fields)
    end.
		
		    
