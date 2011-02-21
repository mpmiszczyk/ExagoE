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

%% @doc Removes an event source from a list of event sources by its name
-spec(remove_event_source_by_name (EventSourceName::any(), EventSources::list()) 
      -> EventSources::list()).
remove_event_source_by_name(EventSourceNameReference, EventSources) ->
    remove_event_source_by_name(EventSourceNameReference, EventSources, []).

remove_event_source_by_name(_EventSourceNameReference, [], Result) ->
    lists:reverse(Result);
remove_event_source_by_name(EventSourceNameReference, [{EventSourceName, FieldList}|EventSources], Result) ->
    case EventSourceName =:= EventSourceNameReference of
	true  -> 
	    remove_event_source_by_name(EventSourceNameReference, EventSources, Result);
	false -> 
	    remove_event_source_by_name(EventSourceNameReference, EventSources, [{EventSourceName, FieldList}|Result])
    end.

%% @doc Find a field by its identifier in a list of fields
find_field(id, _,  []) ->  undefined;
find_field(id, Id, [{Name, {field_identifier, Id}, Value}|_FieldSet]) ->
    {success, {Name, {field_identifier, Id}, Value}};
find_field(id, Id0, [{_, {field_identifier, _Id1}, _}|FieldSet]) ->
    find_field(id, Id0, FieldSet).

-spec(find_field_by_identifier (Identifier::any(), FieldSet::list())
      -> Field::(tuple() | undefined)).
find_field_by_identifier(_Identifier, []) ->
    undefined;
find_field_by_identifier(Identifier, [{FieldName, {field_identifier, FieldIdentifier}, FieldValue}|Events]) ->
    case Identifier =:= FieldIdentifier of
	true  -> {success, {FieldName, {field_identifier, FieldIdentifier}, FieldValue}};
	false -> find_field_by_identifier(Identifier, Events)
    end.

%% @doc Find a field by its identifier and its value
-spec(find_field_by_identifier_and_value (Identifier::any(), Value::any(), FieldSet::list())
      -> Field::(tuple() | undefined)).
find_field_by_identifier_and_value(_Identifier, _Value, []) ->
    undefined;
find_field_by_identifier_and_value(Identifier, Value, [{FieldName, 
							{field_identifier, FieldIdentifier}, 
							{field_value, FieldValue}}|Events]) ->
    case Identifier =:= FieldIdentifier andalso Value =:= FieldValue of
	true  -> {success, {FieldName, {field_identifier, FieldIdentifier}, FieldValue}};
	false -> find_field_by_identifier_and_value(Identifier, Value, Events)
    end.

%% @doc Extract a fields value according to its type
-spec(extract_field_value_by_type (FieldType::atom(), FieldSet::list())
      -> FieldValue::any()).
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

%% @doc Extract a fields value according to its identifier
-spec(extract_field_value_by_id (Identifier::list(), FieldSet::list())
      -> FieldValue::any()).
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
		
		    
