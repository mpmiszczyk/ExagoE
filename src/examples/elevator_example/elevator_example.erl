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
-module(elevator_example).

-export([combined_event_source/0, generate_model/0]).
-export([transition_modifier/2, transition_filter/1]).

timestamp_format() ->
    [date_fullyear, date_month, date_day,
     time_hour, time_minute, time_second,
     time_secfrac].

transition_modifier({FieldType, FieldIdentifier, {field_value, FieldValue}}, Fields) ->
    case FieldValue of
	{string, "reset"}       ->
	    {string, FloorN} = exa_es_util:extract_field_value_by_id("floor2", Fields),
	    {FieldType, FieldIdentifier, {field_value, {string, "reset_to_" ++ FloorN}}};
	{string, "approaching"} ->
	    {string, FloorN} = exa_es_util:extract_field_value_by_id("floor1", Fields),
	    {FieldType, FieldIdentifier, {field_value, {string, "approaching_" ++ FloorN}}};
	{string, "stopped_at"}  ->
	    {string, FloorN} = exa_es_util:extract_field_value_by_id("floor1", Fields),
	    {FieldType, FieldIdentifier, {field_value, {string, "stopped_at_" ++ FloorN}}};
	_             ->
	    {FieldType, FieldIdentifier, {field_value, FieldValue}}
    end.

transition_filter({ResultType, Event, FormatResult}) ->
    case exa_es_util:extract_field_value_by_id("elevator_control", Event) of
	{string, "reset"       ++ _} -> true;
	{string, "open"        ++ _} -> true;
	{string, "close"       ++ _} -> true;
	{string, "approaching" ++ _} -> true;
	{string, "stopped_at"  ++ _} -> true;
	Other                        -> false
    end.    

row_format() ->
    [exa_field:timestamp("timestamp", partial, timestamp_format()),
     exa_field:annotation("event_id", integer),
     exa_field:transition("elevator_control", string),
     exa_field:instance_key("elevator_number", integer),
     exa_field:annotation("floor1", string),
     exa_field:annotation("floor2", string)].

include_files() -> [{csv, absolute, "./elevator.log"}].

event_source() -> {"elevator_log", include_files(), row_format()}.

combined_event_source() ->
    {"elevator_log", exa_es:collect([event_source()], absorb, implicit_state, 
				    [{result_filter, {exa_elevator, transition_filter}},
				     {event_modifier, {exa_elevator, transition_modifier}}])}.

generate_model() ->
    exa_sm:generate_state_machine(combined_event_source(), [{gen_state, true}, {uniques, true}]).

    

