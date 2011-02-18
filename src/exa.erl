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
-module(exa).

-export([generate_uniques/2, generate_uniques/3]).
-export([generate_combined/2, generate_combined/3]).
%%-export([execute/0, execute_multiple/0]).
-export([generate_quickcheck_interface/0, enable_ttb_trace/1]).

generate_uniques(EventSource, Opt) ->
    Uniques = exa_sm:generate_state_machine(EventSource, [{uniques, true}]),
    case proplists:get_value(visualize, Opt, false) of
	{true, BasePath} -> exa_sm:generate_visualizations(BasePath, Uniques, 0);
	false            -> ok
    end, Uniques.

generate_uniques(EventSource, StateFormat, Opt) ->
    AugmentedUniques = 
	case proplists:get_value(multiple_state_formats, Opt, false) of
	    true  -> Uniques = exa_sm:generate_state_machine(EventSource, [{uniques, true}]),
		     exa_sm:augment_models(Uniques, StateFormat);
	    false -> Uniques = exa_sm:generate_state_machine(EventSource, [{uniques, true}]),
		     lists:map(fun (Unique) ->
				       exa_sm:augment_model(Unique, StateFormat)
			       end, Uniques)
	end,
    case proplists:get_value(visualize, Opt, false) of
	{true, BasePath} -> exa_sm:generate_visualizations(BasePath, AugmentedUniques, 0);
	false            -> ok
    end, AugmentedUniques.

generate_combined(EventSource, Opt) ->
    Combined = exa_sm:generate_state_machine(EventSource, []),
    case proplists:get_value(visualize, Opt, false) of
	{true, BasePath} -> exa_sm:generate_visualizations(BasePath, [Combined], 0);
	false            -> ok
    end, Combined.

generate_combined(EventSource, StateFormat, Opt) ->
    Combined = exa_sm:generate_state_machine(EventSource, []),
    AugmentedCombined = exa_sm:augment_model(Combined, StateFormat),
    case proplists:get_value(visualize, Opt, false) of
	{true, BasePath} -> exa_sm:generate_visualizations(BasePath, [AugmentedCombined], 0);
	false            -> ok
    end, AugmentedCombined.

generate_quickcheck_interface() ->
    quickcheck_interface.

enable_ttb_trace(ProcessList) ->
    ttb_trace_enabled.

%% @doc execute an fsm against a source and return the success rates
execute_fsm_source(FSMModel, EventSource) ->
    exa_sm_sup:execute_fsm_against_source(FSMModel, EventSource).

%% @doc execute an fsm against another fsm and return the success status
execute_fsm_fsm(FSMModel1, FSMModel2) ->
    exa_sm_sup:execute_fsm_against_fsm(FSMModel1, FSMModel2).

%execute_fsm_source_multiple(fsm_source, FSMModels, EventSource, Result) ->
%%    .
%%execute_fsm_fsm_multiple(fsm_fsm, FSMModels1, FSMModels2) ->
%%    .

