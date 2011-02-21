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

-export([combined/0, generate_model/0]).
-export([transition_modifier/2, transition_filter/1]).

timestamp_format() ->
    [date_fullyear, date_month, date_day,
     time_hour, time_minute, time_second,
     time_secfrac].

transition_modifier({transition, FieldIdentifier, {field_value, FieldValue}}, Fields) when is_list(FieldValue) ->
    case FieldValue of
	"reset"       ->
	    FloorN = exa_es_util:extract_field_value_by_id("floor2", Fields),
	    {transition, FieldIdentifier, {field_value, "reset_to_" ++ FloorN}};
	"approaching" ->
	    FloorN = exa_es_util:extract_field_value_by_id("floor1", Fields),
	    {transition, FieldIdentifier, {field_value, "approaching_" ++ FloorN}};
	"stopped_at"  ->
	    FloorN = exa_es_util:extract_field_value_by_id("floor1", Fields),
	    {transition, FieldIdentifier, {field_value, "stopped_at_" ++ FloorN}};
	_             ->
	    {transition, FieldIdentifier, {field_value, FieldValue}}
    end;
transition_modifier(Field, _Fields) ->
    Field.

transition_filter({_ResultType, Event, _FormatResult}) ->
    case exa_es_util:extract_field_value_by_id("elevator_control", Event) of
	"reset"       ++ _ -> true;
	"open"        ++ _ -> true;
	"close"       ++ _ -> true;
	"approaching" ++ _ -> true;
	"stopped_at"  ++ _ -> true;
	_Other             -> false
    end.    

row_format() ->
    [exa_field:timestamp("timestamp", partial, timestamp_format()),
     exa_field:annotation("event_id", integer),
     exa_field:transition("elevator_control", string),
     exa_field:instance_key("elevator_number", integer),
     exa_field:annotation("floor1", string),
     exa_field:annotation("floor2", string)].

event_source() -> {"elevator_log", [{csv, absolute, "log_files/elevator.log"}], row_format()}.

combined() ->
    {"combined", exa_es:collect([event_source()], absorb, implicit_state, 
				[{result_filter, {elevator_example, transition_filter}},
				 {event_modifier, {elevator_example, transition_modifier}}])}.

generate_model() ->
    exa_sm:generate_state_machine(combined(), [{gen_state, true}, {uniques, true}]).

    

