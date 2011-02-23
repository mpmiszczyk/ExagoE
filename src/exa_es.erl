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
-module(exa_es).

-export([collect/3, collect/4, combine/1, sort_events/1]).

%% @doc Combines Event Sources based on the type of combination, which can be either 'absorb' or 
%% 'append'. The difference between the two is that absorb is used where relational log data is 
%% absorbed into another event source, and then discarded. Append is used when two relational
%% logs contain valid events, but one depends on some values of the other.
%% The StateOption can be either 'source_state' or any other atom, with the preferred 'implicit_state'.
%% The difference between the two is that source_state appends a state to the events based on the 
%% event source name, whereas any other atom assumes that the state is implicit within the logs.

%%
sort_events(StrippedSources) ->
    lists:sort(fun (A, B) ->
		       TA = exa_es_util:extract_field_value_by_id(timestamp, A),
		       TB = exa_es_util:extract_field_value_by_id(timestamp, B),
		       TA > TB
	       end, StrippedSources).

%% combine
extract_transaction(FieldSet) ->
    extract_transaction(FieldSet, [], [], none).

extract_transaction([], TransactionFields, KeptFields, {field_value, Key}) ->
    {Key, lists:reverse(TransactionFields), lists:reverse(KeptFields)};
extract_transaction([], TransactionFields, KeptFields, Key) ->
    {Key, lists:reverse(TransactionFields), lists:reverse(KeptFields)};
extract_transaction([{timestamp, TSId, TSValue}|FieldSet], TransactionFields, KeptFields, Key) ->
    extract_transaction(FieldSet, [{timestamp, TSId, TSValue}|TransactionFields], KeptFields, Key);
extract_transaction([{transaction_type, TTId, TTValue}|FieldSet], TransactionFields, KeptFields, Key) ->
    extract_transaction(FieldSet, [{transaction_type, TTId, TTValue}|TransactionFields], KeptFields, Key);
extract_transaction([{transaction_key, TKId, TKValue}|FieldSet], TransactionFields, KeptFields, _Key) ->
    extract_transaction(FieldSet, [{transaction_key, TKId, TKValue}|TransactionFields], KeptFields, TKValue);
extract_transaction([{transition, TId, TValue}|FieldSet], TransactionFields, KeptFields, Key) ->
    extract_transaction(FieldSet, [{transition, TId, TValue}|TransactionFields], KeptFields, Key);
extract_transaction([{state, SId, SValue}|FieldSet], TransactionFields, KeptFields, Key) ->
    extract_transaction(FieldSet, [{state, SId, SValue}|TransactionFields], KeptFields, Key);
extract_transaction([OtherField|FieldSet], TransactionFields, KeptFields, Key) ->
    extract_transaction(FieldSet, TransactionFields, [OtherField|KeptFields], Key).

extract_key_list([], KeyList) ->
    lists:reverse(KeyList);
extract_key_list([{Result, {Key, TransactionFields, Rest}, FormatResult}|Results], KeyList) ->
    extract_key_list(Results, [Key|KeyList]).

%% @doc extract_by_key
extract_by_key(Key, Results) ->
    extract_by_key(Key, Results, [], []).

extract_by_key(Key, [], Transaction, KeptResults) ->
    {lists:reverse(Transaction), lists:reverse(KeptResults)};
extract_by_key(Key, [{ResultType, {Key, TransactionFields, Rest}, FormatResult}|Results], Transaction, KeptResults) ->
    extract_by_key(Key, Results, [{ResultType, {Key, TransactionFields, Rest}, FormatResult}|Transaction], KeptResults);
extract_by_key(Key, [{ResultType, {OtherKey, TransactionFields, Rest}, FormatResult}|Results], Transaction, KeptResults) ->
    extract_by_key(Key, Results, Transaction, [{ResultType, {OtherKey, TransactionFields, Rest}, FormatResult}|KeptResults]).

%% @doc combine_transaction
combine_transaction([{ResultType0, {Key, Fields0, Rest0}, F0}, 
		     {ResultType1, {Key, Fields1, Rest1}, F1}]) ->
    {ResultType0, combine_transaction_fields(Fields0, Rest0, Fields1, Rest1), F0};
combine_transaction([{ResultType0, {Key0, Fields0, Rest0}, F0},
		     {ResultType1, {Key1, Fields1, Rest1}, F1}]) ->
    {error, fatal_different_key};
combine_transaction(L) ->
    {error, combining_transaction}.

%% @doc resolve_transaction (Fields0 is sender, Fields1 is receiver)
resolve_transaction(Fields0, Rest0, Fields1, Rest1) ->
    [proplists:lookup(timestamp, Fields0), proplists:lookup(transition, Fields0),
     {state_from, proplists:lookup(state, Fields0)},
     {state_to, proplists:lookup(state, Fields1)}
    ] ++ Rest0.

combine_transaction_fields(Fields0, Rest0, Fields1, Rest1) ->
    TT0 = proplists:lookup(transaction_type, Fields0),
    TT1 = proplists:lookup(transaction_type, Fields1),
    case TT0 of
	{transaction_type, _, {field_value, 'receive'}} ->
	    case TT1 of
		{transaction_type, _, {field_value, send}} ->
		    resolve_transaction(Fields1, Rest1, Fields0, Rest0);
		{transaction_type, _, {field_value, 'receive'}} ->
		    {error, multiple_receives}
	    end;
	{transaction_type, _, {field_value, send}} ->
	    case TT1 of
		{transaction_type, _, {field_value, send}} ->
		    {error, multiple_sends};
		{transaction_type, _, {field_value, 'receive'}} ->
		    resolve_transaction(Fields0, Rest0, Fields1, Rest1)
	    end
    end.

%% @doc combine
combine_by_key([], UnresolvedResults, CombinedResult) ->
    lists:reverse(CombinedResult);
combine_by_key([Key|Keys], Results, CombinedResult) ->
    {Transaction, UnresolvedResults} = extract_by_key(Key, Results),
    combine_by_key(Keys, UnresolvedResults, [combine_transaction(Transaction)|CombinedResult]).

combine(Results) ->
    combine(Results, []).

combine([], CombinedResults) ->
    R = lists:reverse(CombinedResults),
    combine_by_key(extract_key_list(R, []), R, []);
combine([{ResultType, FieldSet, FormatResult}|Results], Sources) ->
    combine(Results, [{ResultType, extract_transaction(FieldSet), FormatResult}|Sources]).

%%
collect(EventSources, CombinationType, StateOption, Transmuters) ->
    ModifiedResults = apply_modifiers(Transmuters, collect(EventSources, CombinationType, StateOption)),
    apply_filters(Transmuters, ModifiedResults).

%% @doc collect the event sources
collect(EventSources, CombinationType, StateOption) ->
    collect_(EventSources, CombinationType, StateOption, []).

collect_([], append, StateOption, Sources) ->
    lists:flatten(resolve_foreign_keys(lists:reverse(Sources), StateOption));
collect_([], absorb, StateOption, Sources) ->
    [Result|_] = resolve_foreign_keys(lists:reverse(Sources), StateOption),
    Result;
collect_([{EventSourceName, Files, RowFormat}|EventSources], CombinationType, StateOption, Sources) ->
    collect_(EventSources, CombinationType, StateOption,
	     [{EventSourceName, 
	       lists:flatten([exa_field_fmt:apply_field_format(exa_parse:parse(File), RowFormat)
			      || File <- Files])}|Sources]).

%%
apply_modifiers([], CollectedSources) ->
    CollectedSources;
apply_modifiers([{result_modifier, {Module, Function}}|Transmuters], CollectedSources) ->
    apply_modifiers(Transmuters, lists:foldl(fun (Result, Rest) ->
						     [Module:Function(Result, CollectedSources)|Rest]
					     end, [], CollectedSources));
apply_modifiers([{event_modifier, {Module, Function}}|Transmuters], CollectedSources) ->
    apply_modifiers(Transmuters, lists:map(fun ({ResultType, Event, FormatResult}) ->
						   {ResultType,
						    lists:reverse(
						      lists:foldl(fun (E, Rest) -> 
									  [Module:Function(E, Event)|Rest]
								  end, [], Event)
						     ), 
						    FormatResult}
					   end, CollectedSources));
apply_modifiers([_|Transmuters], CollectedSources) ->
    apply_modifiers(Transmuters, CollectedSources).

%%
apply_filters([], CollectedSources) ->
    lists:filter(fun (R) -> R =/= [] end, CollectedSources);
apply_filters([{result_filter, {Module, Function}}|Transmuters], CollectedSources) ->
    apply_filters(Transmuters, lists:filter(fun (Result) -> Module:Function(Result) end, CollectedSources));
apply_filters([{event_filter, {Module, Function}}|Transmuters], CollectedSources) ->
    apply_filters(Transmuters, lists:map(fun ({_ResultType, Event, _FormatResult}) ->
						lists:filter(fun (E) -> Module:Function(E) end, Event)
					 end, CollectedSources));
apply_filters([_|Transmuters], CollectedSources) ->
    apply_filters(Transmuters, CollectedSources).

%%
source_transitions(EventSource) ->
    lists:map(fun (Instance) ->
		      [Transition || {Transition, _State} <- Instance]
	      end, exa_sm:sort_fsms(exa_sm:strip_results([EventSource]))).

%%
resolve_foreign_keys(EventSources, StateOption) ->
    resolve_foreign_keys_(EventSources, StateOption, EventSources, []).

resolve_foreign_keys_([], _StateOption, _EventSourcesUnchanged, ResolvedSources) ->
    lists:reverse(ResolvedSources);
resolve_foreign_keys_([{EventSourceName, Events}|EventSources], StateOption, EventSourcesUnchanged, ResolvedSources) ->
    resolve_foreign_keys_(EventSources, StateOption, EventSourcesUnchanged,
			  [[resolve_foreign_key(Event, EventSourceName, StateOption, 
						exa_es_util:remove_event_source_by_name(EventSourceName, EventSourcesUnchanged))
			    || Event <- Events]|ResolvedSources]).

resolve_foreign_key({ResultType, FieldList, FormatStatus}, EventSourceName, StateOption, EventSources) ->
    case StateOption of
	source_state ->
	    {ResultType, lists:flatten([{state, 
					 {field_identifier, EventSourceName},
					 {field_value, EventSourceName}}] ++
					   [resolve_field(Field, EventSources) || Field <- FieldList]), FormatStatus};
	_Other       ->
	    {ResultType, lists:flatten([resolve_field(Field, EventSources) || Field <- FieldList]), FormatStatus}
    end.

%% Here is where a check for duplication would occur
%% @doc
resolve_field({foreign_key, FieldIdentifier, {field_value, {foreign_reference, ForeignReference}}}, EventSources) ->
    [{foreign_key, FieldIdentifier, {field_value, {foreign_reference, ForeignReference}}}|
     resolve_reference({internal_reference, FieldIdentifier, ForeignReference}, EventSources, [])];
resolve_field(OtherField, _EventSources) ->
    OtherField.

%% @doc
resolve_reference(_ForeignReference, [], ResolvedFields) ->
    lists:reverse(ResolvedFields);
resolve_reference({internal_reference, FieldIdentifier, {EventSourceNameReference, ParsedField, ForeignKey, FieldList}}, 
		  [{EventSourceName, Events}|EventSources], ResolvedFields) ->
    case EventSourceName =:= EventSourceNameReference of
	true  -> 
	    resolve_reference(
	      {internal_reference, FieldIdentifier, {EventSourceNameReference, ParsedField, ForeignKey, FieldList}}, 
	      EventSources, 
	      [resolve_reference_field(
		 {internal_reference, FieldIdentifier, {ParsedField, ForeignKey, FieldList}},
		 Events
		)|ResolvedFields]
	     );
	false -> 
	    resolve_reference(
	      {internal_reference, FieldIdentifier, {EventSourceNameReference, ParsedField, ForeignKey, FieldList}},
	      EventSources, ResolvedFields
	     )
    end.

%% @doc
resolve_reference_field(InternalReference, Events) ->
    resolve_reference_field_(InternalReference, Events, []).

resolve_reference_field_(_InternalReference, [], []) ->
    {resolution_error, field_resolution_failed};
resolve_reference_field_(_InternalReference, [], Result) ->
    lists:reverse(Result);
resolve_reference_field_({internal_reference, FieldIdentifier, {ParsedField, ForeignKeyIdentifier, FieldList}}, 
			 [{_ResultType, Fields, _ResultStatus}|Events], Result) ->
    case exa_es_util:find_field_by_identifier_and_value(ForeignKeyIdentifier, ParsedField, Fields) of
	{success, _Field} -> absorb_fields(FieldList, Fields);
	undefined         -> 
	    resolve_reference_field_(
	      {internal_reference, FieldIdentifier, {ParsedField, ForeignKeyIdentifier, FieldList}},
	      Events, Result
	      )
    end.

%% @doc 
absorb_fields(FieldList, Events) ->
    absorb_fields_(FieldList, Events, []).

absorb_fields_([], _Events, Result) ->
    lists:reverse(Result);
absorb_fields_([FieldIdentifier|FieldIdentifierList], Events, Result) ->
    case exa_es_util:find_field_by_identifier(FieldIdentifier, Events) of 
	{success, Field} -> 
	    absorb_fields_(FieldIdentifierList, Events, [Field|Result]);
	undefined        ->
	    absorb_fields_(FieldIdentifierList, Events, [{resolution_error, field_resolution_failed}])
    end.
